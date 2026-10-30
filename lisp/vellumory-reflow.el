;;; vellumory-reflow.el --- Read documents reflowed to the window -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Wolfhard Prell

;; Author: Wolfhard Prell <mail@wolfhard.net>
;; URL: https://github.com/cypl0x/vellumory

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; `vellumory-reflow' (C-c C-r; zr with evil) shows a document as text
;; reflowed to the window: one column at a comfortable measure, in the size,
;; font and colours you choose, the way a phone's reading mode shows a PDF.
;; Like it, it is a view: the document is not changed.
;;
;; The worker makes each page HTML from its structure tree when the PDF is
;; tagged, and from its layout when not.  Headings, paragraphs, lists, code,
;; notes and links keep their kind; figures, formulas and drawings are
;; pictures of the page, sized so their lettering matches the text; running
;; heads and page numbers are left out.  Pages load as you read, in either
;; direction, and leaving returns the document to the place you reached.
;; With `vellumory-reflow-narrow-width', windows too narrow for the pages
;; show them reflowed by themselves, and the pages again once wide enough.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'dom)
(require 'shr)
(require 'face-remap)

(defvar vellumory--pages)
(defvar vellumory--outline)
(defvar vellumory--last-position)
(defvar vellumory--window-positions)
(defvar vellumory--zoom)
(defvar vellumory-reflow-narrow-width)
(defvar shr-fill-text)                 ; Emacs 30
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--reading-position "vellumory" (&optional window))
(declare-function vellumory--window-size "vellumory" (window))
(declare-function vellumory--theme "vellumory" ())
(declare-function vellumory--this-document "vellumory" ())
(declare-function vellumory--page-description "vellumory-navigation" (page))

(defgroup vellumory-reflow nil "Reading documents reflowed to the window." :group 'vellumory)

(defcustom vellumory-reflow-measure 70
  "Most characters in a line of reflowed text, or nil for the window's width.
Lines of 60 to 75 characters read best; narrower windows get shorter lines."
  :type '(choice (natnum :tag "Characters") (const :tag "The window's width" nil)))

(defcustom vellumory-reflow-variable-pitch t
  "Set reflowed text in the proportional `variable-pitch' font." :type 'boolean)

(defcustom vellumory-reflow-line-spacing 0.3
  "Extra space between lines of reflowed text: a fraction of a line, or pixels."
  :type '(choice (float :tag "Fraction of a line") (natnum :tag "Pixels") (const :tag "None" nil)))

(defcustom vellumory-reflow-images t
  "Show figures, formulas and drawings as pictures; nil shows their text."
  :type 'boolean)

(defcustom vellumory-reflow-running-text nil
  "Show running heads, page numbers and other text outside the content."
  :type 'boolean)

(defcustom vellumory-reflow-page-breaks t "Mark where each page begins." :type 'boolean)

(defcustom vellumory-reflow-colours 'auto
  "Colours of reflowed text.
`auto' reads dark while the document's pages are inverted (night mode, or
`vellumory-theme' set to `auto' with a dark Emacs theme), and in the Emacs
theme's colours otherwise.  `theme' always takes the Emacs theme's colours;
`paper', `sepia' and `night' take the faces `vellumory-reflow-paper',
`vellumory-reflow-sepia' and `vellumory-reflow-night'."
  :type '(choice (const :tag "Follow the pages" auto) (const :tag "The Emacs theme" theme)
                 (const :tag "Paper" paper) (const :tag "Sepia" sepia) (const :tag "Night" night)))

(defface vellumory-reflow-paper '((t :foreground "#1f1f1f" :background "#fbfaf7"))
  "Reflowed text on white paper.")
(defface vellumory-reflow-sepia '((t :foreground "#5b4636" :background "#f4ecd8"))
  "Reflowed text on sepia paper.")
(defface vellumory-reflow-night '((t :foreground "#cfccc6" :background "#141414"))
  "Reflowed text for reading at night.")
(defface vellumory-reflow-page-break '((t :inherit shadow :height 0.8))
  "The number where a page begins.")
(defface vellumory-reflow-caption '((t :slant italic :height 0.92))
  "Captions of figures.")
(defface vellumory-reflow-note '((t :height 0.9))
  "Notes and asides.")
(defface vellumory-reflow-running '((t :inherit shadow :height 0.85))
  "Running heads and page numbers, when shown.")

(defvar-local vellumory-reflow--source nil "The document this buffer shows reflowed.")
(defvar-local vellumory-reflow--html nil "Zero-based page → its HTML from the worker.")
(defvar-local vellumory-reflow--pending nil "Pages asked of the worker and not answered yet.")
(defvar-local vellumory-reflow--generation 0 "Bumped when replies in flight no longer apply.")
(defvar-local vellumory-reflow--first nil "First page in the buffer, or nil when empty.")
(defvar-local vellumory-reflow--last nil "Last page in the buffer.")
(defvar-local vellumory-reflow--target nil "(PAGE . Y) to show once PAGE is in, Y a fraction of it.")
(defvar-local vellumory-reflow--width nil "Pixels of text in a line the buffer was made for.")
(defvar-local vellumory-reflow--colour-remaps nil "Face remappings giving the colours.")
(defvar-local vellumory-reflow--dark nil "Whether the figures were asked for dark reading.")
(defvar-local vellumory-reflow--automatic nil "Non-nil when shown because its window was narrow.")
(defvar-local vellumory-reflow--rerender-timer nil)
(defvar-local vellumory-reflow--buffer nil "In a document: its reflowed buffer.")

(defconst vellumory-reflow--keep 14 "Pages kept in the buffer around the one read.")

(defvar vellumory-reflow--rendering nil
  "While a page renders, a plist describing it.
Its keys are :page, :size (its body text in points), :width (pixels of
text in a line) and :scale (the text scale).")

;;;; Rendering

(defun vellumory-reflow--mark (start end property value)
  "Give the text from START to END PROPERTY VALUE where it has none yet."
  (when value
    (let ((position start))
      (while (< position end)
        (let ((next (next-single-property-change position property nil end)))
          (unless (get-text-property position property) (put-text-property position next property value))
          (setq position next))))))

(defun vellumory-reflow--with-y (dom render)
  "Render DOM with RENDER, marking its text with the block's position on the page."
  (let ((start (point)))
    (funcall render dom)
    (when-let* ((y (dom-attr dom 'y)))
      (vellumory-reflow--mark start (point) 'vellumory-y (string-to-number y)))))

(defun vellumory-reflow--tag-page (dom)
  "Render a page DOM, its number first, every character knowing its page."
  (let* ((start (point))
         (page (plist-get vellumory-reflow--rendering :page))
         (source vellumory-reflow--source))
    (setq vellumory-reflow--rendering
          (plist-put vellumory-reflow--rendering :size (string-to-number (or (dom-attr dom 'size) "10"))))
    (when vellumory-reflow-page-breaks
      (let ((label (if (buffer-live-p source)
                       (with-current-buffer source (vellumory--page-description page))
                     (number-to-string (1+ page)))))
        (insert (propertize (concat "— " label " —\n") 'face 'vellumory-reflow-page-break
                            'vellumory-y 0.0 'vellumory-break t))))
    (shr-generic dom)
    (shr-ensure-newline)
    (put-text-property start (point) 'vellumory-page page)))

(defun vellumory-reflow--tag-running (dom)
  "Render running text DOM only when `vellumory-reflow-running-text' asks for it."
  (when vellumory-reflow-running-text
    (shr-ensure-newline)
    (let ((start (point)))
      (shr-generic dom)
      (add-face-text-property start (point) 'vellumory-reflow-running)
      (vellumory-reflow--mark start (point) 'vellumory-y (string-to-number (or (dom-attr dom 'y) "0"))))
    (shr-ensure-newline)))

(defun vellumory-reflow--image-width (points &optional height)
  "Pixels for a picture POINTS wide on the page: lettering as big as the text.
It is no wider than the text, nor, HEIGHT points high, taller than 80% of
the window."
  (let* ((tallest (* 0.8 (or (plist-get vellumory-reflow--rendering :height) 800)))
         (size (max 4 (or (plist-get vellumory-reflow--rendering :size) 10)))
         (pixels-per-point (/ (* (frame-char-height) (or (plist-get vellumory-reflow--rendering :scale) 1))
                              (* 1.2 size))))
    (max 8 (min (or (plist-get vellumory-reflow--rendering :width) 600)
                (round (* points pixels-per-point))
                (if (and height (> height 0)) (round (* tallest (/ points height))) most-positive-fixnum)))))

(defun vellumory-reflow--image (dom)
  "The picture of img DOM and its width, or nil (no picture, or images off)."
  (let ((source (dom-attr dom 'src)) (prefix "data:image/png;base64,"))
    (when (and source (string-prefix-p prefix source) (display-images-p))
      (let ((width (vellumory-reflow--image-width (string-to-number (or (dom-attr dom 'pw) "0"))
                                                  (string-to-number (or (dom-attr dom 'ph) "0")))))
        (cons (create-image (base64-decode-string (substring source (length prefix))) 'png t
                            :width width :ascent 'center)
              width)))))

(defun vellumory-reflow--alt (dom)
  "The alt text of img DOM, its whitespace collapsed, or \"figure\"."
  (let ((alt (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or (dom-attr dom 'alt) "")))))
    (if (string-empty-p alt) "figure" alt)))

(defun vellumory-reflow--tag-img (dom)
  "Render img DOM in the line: its picture, or its alt text in brackets.
The picture stands for its alt text, which copying and searching find."
  (if-let* ((image (vellumory-reflow--image dom)))
      (insert-image (car image) (vellumory-reflow--alt dom))
    (insert (propertize (format "[%s]" (truncate-string-to-width (vellumory-reflow--alt dom) 300 nil nil "…"))
                        'face 'shadow))))

(defun vellumory-reflow--tag-figure (dom)
  "Render figure DOM as a paragraph of its own, its picture centred."
  (shr-ensure-paragraph)
  (let ((start (point)))
    (dolist (child (dom-children dom))
      (if (and (consp child) (eq (dom-tag child) 'img))
          (if-let* ((image (vellumory-reflow--image child)))
              (progn
                (insert (propertize " " 'display `(space :align-to (- center (,(/ (cdr image) 2))))))
                (insert-image (car image) (vellumory-reflow--alt child)))
            (vellumory-reflow--tag-img child))
        (shr-descend child)))
    (vellumory-reflow--mark start (point) 'vellumory-y (string-to-number (or (dom-attr dom 'y) "0"))))
  (shr-ensure-paragraph))

(defun vellumory-reflow--tag-pre (dom)
  "Render code DOM, a wrapped line continuing under its indentation, deeper."
  (let ((start (point)))
    (shr-tag-pre dom)
    (save-excursion
      (goto-char start)
      (while (< (point) (point-max))
        (let* ((bol (point))
               (indent (progn (skip-chars-forward " ") (- (point) bol))))
          (put-text-property bol (line-end-position) 'wrap-prefix
                             (propertize (make-string (+ indent 4) ?\s) 'face (get-text-property (point) 'face)))
          (forward-line 1))))
    (when-let* ((y (dom-attr dom 'y)))
      (vellumory-reflow--mark start (point) 'vellumory-y (string-to-number y)))))

(defun vellumory-reflow--tag-figcaption (dom)
  "Render the caption DOM below its figure."
  (shr-ensure-newline)
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'vellumory-reflow-caption)
    (vellumory-reflow--mark start (point) 'vellumory-y (string-to-number (or (dom-attr dom 'y) "0"))))
  (shr-ensure-paragraph))

(defun vellumory-reflow--tag-aside (dom)
  "Render a note or aside DOM a little smaller."
  (shr-ensure-paragraph)
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'vellumory-reflow-note)
    (vellumory-reflow--mark start (point) 'vellumory-y (string-to-number (or (dom-attr dom 'y) "0"))))
  (shr-ensure-paragraph))

(defconst vellumory-reflow--renderers
  (append
   '((vellumory-page . vellumory-reflow--tag-page)
     (vellumory-running . vellumory-reflow--tag-running)
     (figure . vellumory-reflow--tag-figure)
     (pre . vellumory-reflow--tag-pre)
     (figcaption . vellumory-reflow--tag-figcaption)
     (aside . vellumory-reflow--tag-aside)
     (img . vellumory-reflow--tag-img))
   (mapcar (lambda (tag)
             (let ((render (intern (format "shr-tag-%s" tag))))
               (cons tag (lambda (dom) (vellumory-reflow--with-y dom render)))))
           '(p h1 h2 h3 h4 h5 h6 li ul blockquote table)))
  "How the worker's HTML renders, as `shr-external-rendering-functions'.
shr's renderers keep each block's position on its page; Vellumory's own
render pages, figures and running text.")

(defun vellumory-reflow--render (page)
  "PAGE as text with properties, rendered from its HTML."
  (let ((html (gethash page vellumory-reflow--html))
        (source vellumory-reflow--source)
        (rendering (list :page page :width vellumory-reflow--width
                         :height (window-body-height (get-buffer-window nil t) t)
                         :scale (if (bound-and-true-p text-scale-mode)
                                    (expt text-scale-mode-step text-scale-mode-amount)
                                  1))))
    (if (string-empty-p (or html ""))
        (propertize (format "[Page %d could not be read]\n" (1+ page)) 'face 'shadow 'vellumory-page page)
      (with-temp-buffer
        (setq vellumory-reflow--source source)
        (insert html)
        ;; Leader dots of contents pages: a short run reads the same.
        (goto-char (point-min))
        (while (re-search-forward "\\(?: ?\\. \\)\\{3,\\}\\.?" nil t) (replace-match " … "))
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (let ((vellumory-reflow--rendering rendering)
                (shr-external-rendering-functions
                 (append vellumory-reflow--renderers shr-external-rendering-functions))
                (shr-use-fonts vellumory-reflow-variable-pitch)
                (shr-width (max 20 (/ (or vellumory-reflow--width 600) (frame-char-width))))
                (shr-max-width nil)
                (shr-fill-text nil)
                (shr-inhibit-images (not vellumory-reflow-images)))
            (shr-insert-document dom)))
        (buffer-string)))))

;;;; Pages in the buffer

(defun vellumory-reflow--count ()
  "Pages in the document."
  (length (buffer-local-value 'vellumory--pages vellumory-reflow--source)))

(defun vellumory-reflow--page-bounds (page)
  "Start and end of PAGE's text in the buffer, or nil."
  (when-let* ((start (text-property-any (point-min) (point-max) 'vellumory-page page)))
    (cons start (or (text-property-not-all start (point-max) 'vellumory-page page) (point-max)))))

(defun vellumory-reflow--keeping-place (insert)
  "Call INSERT, keeping every window and point at the text they show."
  (let ((anchors (mapcar (lambda (window)
                           (list window (copy-marker (window-start window) t)
                                 (copy-marker (window-point window) t)))
                         (get-buffer-window-list nil nil t)))
        (here (copy-marker (point) t))
        (inhibit-read-only t))
    (funcall insert)
    (goto-char here)
    (pcase-dolist (`(,window ,start ,point) anchors)
      (set-window-start window start t)
      (set-window-point window point))))

(defun vellumory-reflow--grow ()
  "Put pages that arrived next to those shown into the buffer.
Then show the target, and ask for more as the windows need them."
  (let ((inhibit-read-only t))
    (when (and (null vellumory-reflow--first) vellumory-reflow--target
               (gethash (car vellumory-reflow--target) vellumory-reflow--html))
      (erase-buffer)
      (let ((page (car vellumory-reflow--target)))
        (insert (vellumory-reflow--render page))
        (setq vellumory-reflow--first page vellumory-reflow--last page)))
    (when vellumory-reflow--first
      (while (gethash (1+ vellumory-reflow--last) vellumory-reflow--html)
        (let ((page (cl-incf vellumory-reflow--last)))
          (save-excursion (goto-char (point-max)) (insert (vellumory-reflow--render page)))))
      (while (and (> vellumory-reflow--first 0) (gethash (1- vellumory-reflow--first) vellumory-reflow--html))
        (let ((page (cl-decf vellumory-reflow--first)))
          (vellumory-reflow--keeping-place
           (lambda () (save-excursion (goto-char (point-min)) (insert (vellumory-reflow--render page)))))))))
  (when vellumory-reflow--target (vellumory-reflow--show-target))
  (vellumory-reflow--want))

(defun vellumory-reflow--fetch (pages)
  "Ask the worker for those of PAGES not held, asked for, or outside."
  (let ((buffer (current-buffer)) (generation vellumory-reflow--generation)
        (images (if vellumory-reflow-images t :false)) (dark (if vellumory-reflow--dark t :false))
        (count (vellumory-reflow--count)))
    (dolist (page pages)
      (unless (or (< page 0) (>= page count) (gethash page vellumory-reflow--html)
                  (memq page vellumory-reflow--pending))
        (push page vellumory-reflow--pending)
        (with-current-buffer vellumory-reflow--source
          (vellumory--rpc
           "page-reflow" `((page . ,page) (images . ,images) (invert . ,dark))
           (lambda (reply)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (= generation vellumory-reflow--generation)
                   (setq vellumory-reflow--pending (delq page vellumory-reflow--pending))
                   (when-let* ((error (alist-get 'error reply)))
                     (message "Vellumory: page %d could not be reflowed: %s" (1+ page) error))
                   (puthash page (or (alist-get 'result reply) "") vellumory-reflow--html)
                   (vellumory-reflow--grow)))))))))))

(defun vellumory-reflow--want ()
  "Ask for the pages next to those shown when a window nears either end.
Also drop pages far from the one read."
  (when vellumory-reflow--first
    (let (more-after more-before)
      (dolist (window (get-buffer-window-list nil nil t))
        (let ((start (window-start window)))
          (save-excursion
            (goto-char start)
            (when (< (- (point-max) start) 6000) (setq more-after t))
            (when (< (- start (point-min)) 3000) (setq more-before t)))))
      (when more-after (vellumory-reflow--fetch (list (1+ vellumory-reflow--last) (+ 2 vellumory-reflow--last))))
      (when more-before (vellumory-reflow--fetch (list (1- vellumory-reflow--first))))
      (vellumory-reflow--trim))))

(defun vellumory-reflow--trim ()
  "Keep the buffer to `vellumory-reflow--keep' pages around the one read."
  (when-let* ((window (get-buffer-window nil t))
              (page (get-text-property (window-start window) 'vellumory-page)))
    (let ((inhibit-read-only t) (half (/ vellumory-reflow--keep 2)))
      (while (> (- page vellumory-reflow--first) half)
        (when-let* ((bounds (vellumory-reflow--page-bounds vellumory-reflow--first)))
          (vellumory-reflow--keeping-place (lambda () (delete-region (car bounds) (cdr bounds)))))
        (cl-incf vellumory-reflow--first))
      (while (> (- vellumory-reflow--last page) half)
        (when-let* ((bounds (vellumory-reflow--page-bounds vellumory-reflow--last)))
          (delete-region (car bounds) (cdr bounds)))
        (cl-decf vellumory-reflow--last)))
    ;; Their HTML is kept for going back, a little further.
    (let (far)
      (maphash (lambda (key _) (when (> (abs (- key page)) (* 3 vellumory-reflow--keep)) (push key far)))
               vellumory-reflow--html)
      (dolist (key far) (remhash key vellumory-reflow--html)))))

;;;; Positions

(defun vellumory-reflow--position-of (page y)
  "Where Y, a fraction of PAGE's height, begins in the buffer, or nil.
That is the block starting nearest above Y (the page's start if none)."
  (when-let* ((bounds (vellumory-reflow--page-bounds page)))
    (let ((position (car bounds)) (best (car bounds)) (best-y -1.0))
      (while (< position (cdr bounds))
        (let ((block-y (get-text-property position 'vellumory-y))
              (next (next-single-property-change position 'vellumory-y nil (cdr bounds))))
          (when (and block-y (<= block-y (+ y 0.005)) (> block-y best-y))
            (setq best position best-y block-y))
          (setq position next)))
      best)))

(defun vellumory-reflow--show-target ()
  "Show `vellumory-reflow--target' at the top of the windows, once it is in."
  (pcase-let ((`(,page . ,y) vellumory-reflow--target))
    (when-let* ((position (vellumory-reflow--position-of page y)))
      (setq vellumory-reflow--target nil)
      (goto-char position)
      (dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window position)
        (set-window-start window (save-excursion (goto-char position) (line-beginning-position)) t)))))

(defun vellumory-reflow--here (&optional window)
  "(PAGE . Y) of the text at the top of WINDOW, Y a fraction of the page."
  (let* ((start (if window (window-start window) (point)))
         (page (get-text-property start 'vellumory-page)))
    (when page
      (let ((position start) y)
        (while (and (not y) (< position (point-max)) (eq (get-text-property position 'vellumory-page) page))
          (setq y (get-text-property position 'vellumory-y)
                position (next-single-property-change position 'vellumory-y nil (point-max))))
        (cons page (or y 0.0))))))

(defun vellumory-reflow--document-position (here)
  "HERE, as (PAGE . Y) of the reflow, as a reading position of the document."
  (with-current-buffer vellumory-reflow--source
    (let ((height (alist-get 'height (aref vellumory--pages (car here)))))
      `((page . ,(car here)) (y . ,(* (cdr here) height)) (zoom . ,vellumory--zoom)))))

(defun vellumory-reflow--remember (&rest _)
  "Make the document's reading position the place read here."
  (when-let* ((window (get-buffer-window nil t))
              (here (vellumory-reflow--here window))
              ((buffer-live-p vellumory-reflow--source)))
    (let ((position (vellumory-reflow--document-position here)))
      (with-current-buffer vellumory-reflow--source (setq vellumory--last-position position)))))

;;;; Width, size and colours

(defun vellumory-reflow--text-width (window)
  "Pixels of text in a line in WINDOW: the measure, or less if WINDOW is narrow."
  (let* ((character (* (frame-char-width (window-frame window))
                       (if (bound-and-true-p text-scale-mode)
                           (expt text-scale-mode-step text-scale-mode-amount)
                         1)))
         (margins (window-margins window))
         (whole (+ (window-body-width window t)
                   (* (frame-char-width (window-frame window)) (+ (or (car margins) 0) (or (cdr margins) 0)))))
         (available (- whole (* 2 (frame-char-width (window-frame window))))))
    (round (if vellumory-reflow-measure (min available (* vellumory-reflow-measure character)) available))))

(defun vellumory-reflow--fit (window)
  "Centre the text in WINDOW with margins; render it again for a new width."
  (let* ((width (vellumory-reflow--text-width window))
         (char (frame-char-width (window-frame window)))
         (margins (window-margins window))
         (whole (+ (window-body-width window t) (* char (+ (or (car margins) 0) (or (cdr margins) 0)))))
         (margin (max 0 (/ (- whole width) 2 char))))
    (unless (equal (window-margins window) (cons margin margin))
      (set-window-margins window margin margin))
    (when (and vellumory-reflow--width (> (abs (- width vellumory-reflow--width)) (/ char 2)))
      (when vellumory-reflow--rerender-timer (cancel-timer vellumory-reflow--rerender-timer))
      (setq vellumory-reflow--rerender-timer
            (run-with-timer 0.15 nil #'vellumory-reflow--rerender-in (current-buffer))))))

(defun vellumory-reflow--resized (window)
  "Fit WINDOW, or give it back to the pages when it grew out of being narrow."
  (if (and vellumory-reflow--automatic vellumory-reflow-narrow-width
           (>= (window-body-width window) (+ vellumory-reflow-narrow-width 8)))
      (run-at-time 0 nil (lambda ()
                           (when (and (window-live-p window) (buffer-live-p (window-buffer window)))
                             (with-selected-window window
                               (when (derived-mode-p 'vellumory-reflow-mode) (vellumory-reflow-quit))))))
    (vellumory-reflow--fit window)))

(defun vellumory-reflow--rerender-in (buffer)
  "Render BUFFER's pages again for its window's width and text size."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (vellumory-reflow--rerender))))

(defun vellumory-reflow--rerender (&optional refetch)
  "Render the pages again for the width and options now, at the same place.
With REFETCH, ask the worker for them again (their pictures changed)."
  (let ((window (or (get-buffer-window nil t) (selected-window))))
    (setq vellumory-reflow--width (vellumory-reflow--text-width window))
    (let ((here (or (vellumory-reflow--here (get-buffer-window nil t)) vellumory-reflow--target))
          (inhibit-read-only t))
      (when refetch
        (cl-incf vellumory-reflow--generation)
        (setq vellumory-reflow--pending nil)
        (clrhash vellumory-reflow--html))
      (erase-buffer)
      (setq vellumory-reflow--first nil vellumory-reflow--last nil vellumory-reflow--target here)
      (if refetch
          (when here (vellumory-reflow--fetch (number-sequence (car here) (+ (car here) 2))))
        (when here (vellumory-reflow--grow))))))

(defun vellumory-reflow--dark-p ()
  "Whether reflowed text reads dark.
That is with the night colours, or `auto' while the pages are inverted."
  (pcase vellumory-reflow-colours
    ('night t)
    ('auto (and (buffer-live-p vellumory-reflow--source)
                (eq (with-current-buffer vellumory-reflow--source (vellumory--theme)) 'invert)))))

(defun vellumory-reflow--apply-colours ()
  "Colour the buffer as `vellumory-reflow-colours' says."
  (mapc #'face-remap-remove-relative vellumory-reflow--colour-remaps)
  (setq vellumory-reflow--colour-remaps nil)
  (let ((face (pcase vellumory-reflow-colours
                ('paper 'vellumory-reflow-paper) ('sepia 'vellumory-reflow-sepia)
                ('night 'vellumory-reflow-night)
                ('auto (and (vellumory-reflow--dark-p) 'vellumory-reflow-night)))))
    (when face (push (face-remap-add-relative 'default face) vellumory-reflow--colour-remaps))))

;;;; Commands

(defun vellumory-reflow--buffer-for (source)
  "SOURCE's reflow buffer, made if needed."
  (let ((buffer (buffer-local-value 'vellumory-reflow--buffer source)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer (format "*Reflow: %s*" (buffer-name source))))
      (with-current-buffer buffer
        (vellumory-reflow-mode)
        (setq vellumory-reflow--source source
              vellumory-reflow--html (make-hash-table :test #'eql)))
      (with-current-buffer source
        (setq vellumory-reflow--buffer buffer)
        (add-hook 'kill-buffer-hook
                  (lambda () (when (buffer-live-p buffer) (kill-buffer buffer))) nil t)))
    buffer))

;;;###autoload
(defun vellumory-reflow (&optional window automatic)
  "Read this document reflowed to the window, or return to its pages.
The text flows in one column at `vellumory-reflow-measure' characters, in
the size (\\`+', \\`-'), font and colours (\\`C') you choose; figures are
pictures of the page.  It is a view: the document is not changed, and
leaving with \\`q' shows the pages at the place you reached.  WINDOW is the
window to show it in, by default the selected one; AUTOMATIC marks it shown
for being narrow, to give it back to the pages when it widens."
  (interactive)
  (if (derived-mode-p 'vellumory-reflow-mode)
      (vellumory-reflow-quit)
    (let* ((window (or window (selected-window)))
           (source (vellumory--this-document))
           (position (with-current-buffer source
                       (or (vellumory--reading-position window) vellumory--last-position
                           '((page . 0) (y . 0)))))
           (page (alist-get 'page position))
           (height (with-current-buffer source (alist-get 'height (aref vellumory--pages page))))
           (target (cons page (/ (alist-get 'y position) (max 1 height))))
           (buffer (vellumory-reflow--buffer-for source)))
      (set-window-buffer window buffer)
      (with-current-buffer buffer
        (setq vellumory-reflow--automatic automatic)
        (let ((dark (vellumory-reflow--dark-p)))
          (unless (eq dark vellumory-reflow--dark)
            (setq vellumory-reflow--dark dark)
            (cl-incf vellumory-reflow--generation)
            (setq vellumory-reflow--pending nil)
            (clrhash vellumory-reflow--html)
            (let ((inhibit-read-only t)) (erase-buffer))
            (setq vellumory-reflow--first nil vellumory-reflow--last nil)))
        (vellumory-reflow--apply-colours)
        (setq vellumory-reflow--width (vellumory-reflow--text-width window))
        (vellumory-reflow--fit window)
        (setq vellumory-reflow--target target)
        (if (and vellumory-reflow--first (<= vellumory-reflow--first page vellumory-reflow--last))
            (with-selected-window window (vellumory-reflow--show-target))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "Reflowing…" 'face 'shadow)))
          (setq vellumory-reflow--first nil vellumory-reflow--last nil)
          (vellumory-reflow--fetch (number-sequence page (+ page 2))))))))

(defun vellumory-reflow-quit ()
  "Show the document's pages in this window, at the place read here."
  (interactive)
  (let* ((window (selected-window))
         (source vellumory-reflow--source)
         (here (vellumory-reflow--here window)))
    (unless (buffer-live-p source) (user-error "The document was closed"))
    (when here
      (let ((position (vellumory-reflow--document-position here)))
        (with-current-buffer source
          (setq vellumory--last-position position)
          (setf (alist-get window vellumory--window-positions)
                (cons position (vellumory--window-size window))))))
    (set-window-margins window nil nil)
    (set-window-buffer window source)))

(defun vellumory-reflow-goto-page (page)
  "Read from one-based PAGE on."
  (interactive (list (read-number "Page: " (1+ (or (car (vellumory-reflow--here (selected-window))) 0)))))
  (let ((page (1- page)))
    (unless (< -1 page (vellumory-reflow--count)) (user-error "Page outside document"))
    (setq vellumory-reflow--target (cons page 0.0))
    (if (and vellumory-reflow--first (<= vellumory-reflow--first page vellumory-reflow--last))
        (vellumory-reflow--show-target)
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq vellumory-reflow--first nil vellumory-reflow--last nil)
      (vellumory-reflow--fetch (number-sequence page (+ page 2))))))

(defun vellumory-reflow-next-page (count)
  "Read from the start of the next page, or the COUNTth."
  (interactive "p")
  (vellumory-reflow-goto-page
   (max 1 (min (vellumory-reflow--count) (+ 1 count (or (car (vellumory-reflow--here (selected-window))) 0))))))

(defun vellumory-reflow-previous-page (count)
  "Read from the start of the previous page, or the COUNTth before."
  (interactive "p")
  (vellumory-reflow-next-page (- count)))

(defun vellumory-reflow-first-page () "Read from the first page." (interactive) (vellumory-reflow-goto-page 1))
(defun vellumory-reflow-last-page () "Read the last page." (interactive)
  (vellumory-reflow-goto-page (vellumory-reflow--count)))

(defun vellumory-reflow-outline ()
  "Go to a heading of the document's contents."
  (interactive)
  (let ((choices (cl-loop for entry across (buffer-local-value 'vellumory--outline vellumory-reflow--source)
                          for page = (alist-get 'page entry)
                          when (natnump page)
                          collect (cons (concat (make-string (* 2 (alist-get 'depth entry)) ?\s)
                                                (alist-get 'title entry))
                                        (cons page (alist-get 'y entry))))))
    (unless choices (user-error "This document has no contents"))
    (pcase-let ((`(,page . ,y) (cdr (assoc (completing-read "Heading: " choices nil t) choices))))
      (vellumory-reflow-goto-page (1+ page))
      (when-let* ((height (alist-get 'height (aref (buffer-local-value 'vellumory--pages vellumory-reflow--source)
                                                    page))))
        (setq vellumory-reflow--target (cons page (/ (max 0 (or y 0)) height)))
        (vellumory-reflow--show-target)))))

(defun vellumory-reflow--follow (url &rest _)
  "Follow URL, a link to a page of this document (#page-N)."
  (vellumory-reflow-goto-page (string-to-number (substring url 6))))

(defun vellumory-reflow-narrower (count)
  "Make lines COUNT times five characters shorter."
  (interactive "p")
  (setq-local vellumory-reflow-measure
              (max 20 (- (or vellumory-reflow-measure (window-body-width)) (* 5 count))))
  (vellumory-reflow--fit (selected-window))
  (message "Lines of at most %d characters" vellumory-reflow-measure))

(defun vellumory-reflow-wider (count)
  "Make lines COUNT times five characters longer."
  (interactive "p")
  (vellumory-reflow-narrower (- count)))

(defun vellumory-reflow-reset-size ()
  "Set the text in its normal size again."
  (interactive)
  (text-scale-set 0))

(defconst vellumory-reflow--colour-names
  '((auto . "following the pages") (theme . "the Emacs theme") (paper . "paper") (sepia . "sepia")
    (night . "night"))
  "Values of `vellumory-reflow-colours', in the order they cycle, and their names.")

(defun vellumory-reflow-cycle-colours ()
  "Cycle the colours: following the pages, the Emacs theme, paper, sepia, night."
  (interactive)
  (vellumory-reflow-set-colours
   (car (or (cadr (memq (assq vellumory-reflow-colours vellumory-reflow--colour-names)
                        vellumory-reflow--colour-names))
            (car vellumory-reflow--colour-names)))))

(defun vellumory-reflow-set-colours (colours)
  "Read in COLOURS, a value of `vellumory-reflow-colours'; interactively, choose."
  (interactive (list (car (rassoc (completing-read "Colours: " (mapcar #'cdr vellumory-reflow--colour-names) nil t)
                                  vellumory-reflow--colour-names))))
  (let ((was (vellumory-reflow--dark-p)))
    (setq-local vellumory-reflow-colours colours)
    (vellumory-reflow--apply-colours)
    (unless (eq was (vellumory-reflow--dark-p))
      (setq vellumory-reflow--dark (vellumory-reflow--dark-p))
      (vellumory-reflow--rerender t))
    (message "Colours: %s" (alist-get colours vellumory-reflow--colour-names))))

(defun vellumory-reflow-toggle-images ()
  "Show figures as pictures, or as their text."
  (interactive)
  (setq-local vellumory-reflow-images (not vellumory-reflow-images))
  (vellumory-reflow--rerender t)
  (message "Figures as %s" (if vellumory-reflow-images "pictures" "text")))

(defun vellumory-reflow-toggle-running-text ()
  "Show or hide running heads and page numbers."
  (interactive)
  (setq-local vellumory-reflow-running-text (not vellumory-reflow-running-text))
  (vellumory-reflow--rerender)
  (message "Running heads and page numbers %s" (if vellumory-reflow-running-text "shown" "hidden")))

(defun vellumory-reflow-toggle-page-breaks ()
  "Show or hide where each page begins."
  (interactive)
  (setq-local vellumory-reflow-page-breaks (not vellumory-reflow-page-breaks))
  (vellumory-reflow--rerender)
  (message "Page numbers %s" (if vellumory-reflow-page-breaks "shown" "hidden")))

(defun vellumory-reflow-customize ()
  "Customize reading reflowed."
  (interactive)
  (customize-group 'vellumory-reflow))

(defun vellumory-reflow--header ()
  "The header line: page, document, and how to leave."
  (let ((here (vellumory-reflow--here (selected-window))))
    (concat " " (propertize "Reflowed" 'face 'bold)
            (if (and here (buffer-live-p vellumory-reflow--source))
                (format "  page %s of %d"
                        (with-current-buffer vellumory-reflow--source (vellumory--page-description (car here)))
                        (vellumory-reflow--count))
              "")
            (if (buffer-live-p vellumory-reflow--source)
                (concat "  · " (buffer-name vellumory-reflow--source)) "")
            (propertize (substitute-command-keys
                         "   \\<vellumory-reflow-mode-map>\\[vellumory-reflow-quit] pages  \\[vellumory-reflow-menu] menu")
                        'face 'shadow))))

(defvar vellumory-reflow-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (pcase-dolist (`(,key . ,command)
                   '(("q" . vellumory-reflow-quit) ("C-c C-r" . vellumory-reflow)
                     ("+" . text-scale-increase) ("=" . text-scale-increase) ("-" . text-scale-decrease)
                     ("0" . vellumory-reflow-reset-size)
                     ("{" . vellumory-reflow-narrower) ("}" . vellumory-reflow-wider)
                     ("C" . vellumory-reflow-cycle-colours) ("I" . vellumory-reflow-toggle-images)
                     ("H" . vellumory-reflow-toggle-running-text) ("B" . vellumory-reflow-toggle-page-breaks)
                     ("n" . vellumory-reflow-next-page) ("p" . vellumory-reflow-previous-page)
                     ("g" . vellumory-reflow-goto-page) ("M-g g" . vellumory-reflow-goto-page)
                     ("<" . vellumory-reflow-first-page) (">" . vellumory-reflow-last-page)
                     ("o" . vellumory-reflow-outline) ("i" . vellumory-reflow-outline)
                     ("TAB" . shr-next-link) ("<backtab>" . shr-previous-link)
                     ("?" . vellumory-reflow-menu)))
      (keymap-set map key command))
    map)
  "Keys of reflowed documents.")

(defun vellumory-reflow-menu ()
  "Every command and setting of reading reflowed."
  (interactive)
  (require 'vellumory-dispatch)
  (call-interactively 'vellumory-dispatch-reflow))

(easy-menu-define vellumory-reflow-mode-menu vellumory-reflow-mode-map "Menu of reflowed documents."
  '("Reflowed"
    ["Back to the pages" vellumory-reflow-quit]
    "---"
    ["Next page" vellumory-reflow-next-page] ["Previous page" vellumory-reflow-previous-page]
    ["Go to page…" vellumory-reflow-goto-page] ["Contents…" vellumory-reflow-outline]
    "---"
    ["Larger text" text-scale-increase] ["Smaller text" text-scale-decrease]
    ["Normal size" vellumory-reflow-reset-size]
    ["Shorter lines" vellumory-reflow-narrower] ["Longer lines" vellumory-reflow-wider]
    ("Colours"
     ["Follow the pages" (vellumory-reflow-set-colours 'auto)
      :style radio :selected (eq vellumory-reflow-colours 'auto)]
     ["The Emacs theme" (vellumory-reflow-set-colours 'theme)
      :style radio :selected (eq vellumory-reflow-colours 'theme)]
     ["Paper" (vellumory-reflow-set-colours 'paper)
      :style radio :selected (eq vellumory-reflow-colours 'paper)]
     ["Sepia" (vellumory-reflow-set-colours 'sepia)
      :style radio :selected (eq vellumory-reflow-colours 'sepia)]
     ["Night" (vellumory-reflow-set-colours 'night)
      :style radio :selected (eq vellumory-reflow-colours 'night)])
    ["Figures as pictures" vellumory-reflow-toggle-images
     :style toggle :selected vellumory-reflow-images]
    ["Running heads and page numbers" vellumory-reflow-toggle-running-text
     :style toggle :selected vellumory-reflow-running-text]
    ["Page numbers" vellumory-reflow-toggle-page-breaks :style toggle :selected vellumory-reflow-page-breaks]
    "---"
    ["All commands…" vellumory-reflow-menu] ["Customize…" vellumory-reflow-customize]))

(define-derived-mode vellumory-reflow-mode special-mode "Vellumory-Reflow"
  "A document reflowed to the window, from `vellumory-reflow'.
\\{vellumory-reflow-mode-map}"
  ;; No fringes: the column has no edges but its text.
  (setq-local truncate-lines nil
              word-wrap t
              left-fringe-width 0
              right-fringe-width 0
              line-spacing vellumory-reflow-line-spacing
              header-line-format '(:eval (vellumory-reflow--header))
              revert-buffer-function (lambda (&rest _) (vellumory-reflow--rerender t))
              browse-url-handlers (cons '("\\`#page-[0-9]+\\'" . vellumory-reflow--follow)
                                        (bound-and-true-p browse-url-handlers)))
  (visual-line-mode)
  ;; A line here is a paragraph: highlighting it would band whole paragraphs.
  (setq-local global-hl-line-mode nil)
  (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))
  (add-hook 'window-size-change-functions #'vellumory-reflow--resized nil t)
  (add-hook 'window-scroll-functions (lambda (&rest _) (vellumory-reflow--want)) nil t)
  (add-hook 'post-command-hook #'vellumory-reflow--remember nil t)
  (add-hook 'text-scale-mode-hook (lambda () (dolist (window (get-buffer-window-list nil nil t))
                                               (vellumory-reflow--fit window)))
            nil t))

(provide 'vellumory-reflow)
;;; vellumory-reflow.el ends here
