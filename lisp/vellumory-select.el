;;; vellumory-select.el --- Links, text selection and page overlays -*- lexical-binding: t; -*-

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
;; Mouse selection (drag, double-click words, triple-click lines, across
;; pages), copying, clickable links with hover help, keyboard link hints,
;; and the overlay description that the worker composites into sharp tiles.

;;; Code:
(require 'cl-lib)
(require 'browse-url)

(defvar vellumory--pages)
(defvar vellumory--hash)
(defvar vellumory--generation)
(defvar vellumory--visible)
(defvar vellumory-theme)
(declare-function vellumory--theme "vellumory" ())
(defvar vellumory--last-motion)
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--points-per-pixel "vellumory" (page))
(declare-function vellumory--note-motion "vellumory" (pixels))
(declare-function vellumory--tile-row "vellumory" (tile))
(declare-function vellumory--tile-left "vellumory" (tile))
(declare-function vellumory--tile-right "vellumory" (tile))
(declare-function vellumory--row-page "vellumory" (row))
(declare-function vellumory--tile-page "vellumory" (tile))
(declare-function vellumory--form-field-at "vellumory-form" (page x y))
(declare-function vellumory-fill-field "vellumory-form" (field))
(declare-function vellumory--caret-quads "vellumory-caret" (page))
(declare-function vellumory-caret--place-at "vellumory-caret" (page x y))
(defvar vellumory-caret-mode)
(declare-function vellumory--row-top "vellumory" (row))
(declare-function vellumory--row-bottom "vellumory" (row))
(declare-function vellumory--search-page-quads "vellumory-search" (page))
(declare-function vellumory--diff-quads "vellumory-diff" (page))
(declare-function vellumory--sync-quads "vellumory-sync" (page))

(defgroup vellumory-select nil "Links and selection." :group 'vellumory)
(defcustom vellumory-confirm-external-links t
  "Ask before opening links that leave the document." :type 'boolean)
(defcustom vellumory-link-hint-keys "asdfghjklqwertyuiopzxcvbnm"
  "Characters used to label visible links for `vellumory-follow-link'." :type 'string)

(defface vellumory-highlight-match '((t :background "#ffe14d")) "Search matches.")
(defface vellumory-highlight-current '((t :background "#ff8c1a")) "The current search match.")
(defface vellumory-highlight-selection '((t :background "#8fb8ff")) "Selected text.")
(defface vellumory-highlight-hint '((t :background "#7ee07e")) "Link hint areas.")

(defvar-local vellumory--links nil "Page → vector of link alists, or `pending'.")
(defvar-local vellumory--selection nil "List of (PAGE TEXT QUADS) in document order.")
(defvar-local vellumory--selection-mode 'chars)
(defvar-local vellumory--hints nil "Pages → list of link quads shown as hints.")

;;;; Overlays composited by the worker

(defun vellumory--face-rgba (face alpha)
  "Return FACE's background color as a vector [R G B ALPHA] of 0-255 values.
A face without a background color counts as yellow."
  (let ((rgb (color-name-to-rgb (or (face-background face nil t) "yellow"))))
    (vconcat (mapcar (lambda (c) (round (* 255 c))) rgb) (list alpha))))

(defun vellumory--rect-quad (x0 y0 x1 y1)
  "Return the quad of the rectangle from X0, Y0 to X1, Y1.
Its corners are X0 Y0, X1 Y0, X1 Y1 and X0 Y1, in one flat vector."
  (vector x0 y0 x1 y0 x1 y1 x0 y1))

(defun vellumory--page-overlay (page)
  "Overlay description for PAGE, or nil.  Kinds index the palette."
  (let ((dark (memq (vellumory--theme) '(tint invert))) quads kinds)
    (dolist (entry (vellumory--search-page-quads page))
      (push (car entry) quads) (push (if (cdr entry) 1 0) kinds))
    (dolist (selection vellumory--selection)
      (when (= (car selection) page)
        (cl-loop for quad across (nth 2 selection)
                 do (push (vellumory--quad-vector quad) quads) (push 2 kinds))))
    (dolist (quad (alist-get page vellumory--hints))
      (push quad quads) (push 3 kinds))
    (dolist (quad (vellumory--diff-quads page))
      (push quad quads) (push 4 kinds))
    (dolist (quad (vellumory--sync-quads page))
      (push quad quads) (push 1 kinds))
    (dolist (quad (vellumory--caret-quads page))
      (push quad quads) (push 5 kinds))
    (when quads
      (let ((alpha (if dark 110 255)))
        `((quads . ,(vconcat (nreverse quads))) (kinds . ,(vconcat (nreverse kinds)))
          (palette . [,(vellumory--face-rgba 'vellumory-highlight-match alpha)
                      ,(vellumory--face-rgba 'vellumory-highlight-current alpha)
                      ,(vellumory--face-rgba 'vellumory-highlight-selection alpha)
                      ,(vellumory--face-rgba 'vellumory-highlight-hint alpha)
                      ,(vellumory--face-rgba 'vellumory-highlight-added alpha)
                      ,(vellumory--face-rgba 'cursor 255)])
          (blend . ,(if dark "normal" "multiply")))))))

(defun vellumory--quad-vector (quad)
  "Return the alist QUAD as a vector [X0 Y0 X1 Y1 X2 Y2 X3 Y3]."
  (vector (alist-get 'x0 quad) (alist-get 'y0 quad) (alist-get 'x1 quad) (alist-get 'y1 quad)
          (alist-get 'x2 quad) (alist-get 'y2 quad) (alist-get 'x3 quad) (alist-get 'y3 quad)))

;;;; Coordinates

(defun vellumory--event-location (posn)
  "(PAGE X Y) in page points for the tile under POSN, or nil."
  (when-let* ((pos (posn-point posn))
              (tile (and (integer-or-marker-p pos) (get-text-property pos 'vellumory-tile)))
              (xy (posn-object-x-y posn)))
    (let* ((row (vellumory--tile-row tile)) (page (vellumory--tile-page tile))
           (k (vellumory--points-per-pixel page)))
      (list page (* k (+ (vellumory--tile-left tile) (car xy)))
            (* k (+ (vellumory--row-top row) (cdr xy)))))))

;;;; Links

(defvar vellumory--visible-and-near)
(declare-function vellumory--tile-image "vellumory" (tile))
(declare-function vellumory--put-display "vellumory" (tile display))
(declare-function vellumory--decorate-tile "vellumory" (tile image))

(defun vellumory--redecorate-page (page)
  "Give sharp tiles of PAGE the link areas that arrived after they did.
No new raster work: only the display property gains its image map."
  (dolist (tile vellumory--visible-and-near)
    (when-let* (((= page (vellumory--tile-page tile)))
                (image (vellumory--tile-image tile)))
      (vellumory--put-display tile (vellumory--decorate-tile tile image)))))

(defun vellumory--ensure-links (page)
  "Request PAGE's links once; return them when known."
  (let ((known (gethash page vellumory--links)))
    (cond ((vectorp known) known)
          ((eq known 'pending) nil)
          (t (puthash page 'pending vellumory--links)
             (let ((hash vellumory--hash))
               (vellumory--rpc "links" `((page . ,page))
                               (lambda (reply)
                                 (when (equal hash vellumory--hash)
                                   (puthash page (or (alist-get 'links (alist-get 'result reply)) [])
                                            vellumory--links)
                                   (vellumory--redecorate-page page)))))
             nil))))

(defun vellumory--link-at (page x y)
  "Return the link on PAGE whose rectangle contains point X, Y, or nil.
Links not known yet are requested and count as none."
  (cl-find-if (lambda (link)
                (and (<= (alist-get 'x0 link) x (alist-get 'x1 link))
                     (<= (alist-get 'y0 link) y (alist-get 'y1 link))))
              (vellumory--ensure-links page)))

(defun vellumory--link-help (link)
  "Return the help text of LINK: the page it goes to, else its URI."
  (if (alist-get 'page link)
      (format "Go to page %d" (1+ (alist-get 'page link)))
    (alist-get 'uri link)))

(defvar vellumory-link-preview)
(declare-function vellumory--link-preview-hover "vellumory-link-preview" (window link))

(defun vellumory--link-echo (link)
  "Help echo for LINK: where it goes, in words.
For a link within the document it is a function that also starts its
preview, which then says it instead (see `vellumory-link-preview')."
  (let ((help (vellumory--link-help link)))
    (if (natnump (alist-get 'page link))
        (lambda (window _object _position)
          (vellumory--link-preview-hover window link)
          (unless (and vellumory-link-preview (display-graphic-p (window-frame window))) help))
      help)))

(defun vellumory--link-map (tile)
  "Image map giving TILE's links a hand pointer and help text."
  (when vellumory--links
    (let* ((row (vellumory--tile-row tile)) (page (vellumory--tile-page tile))
           (links (vellumory--ensure-links page))
           (k (vellumory--points-per-pixel page))
           (left (vellumory--tile-left tile)) (top (vellumory--row-top row))
           (width (- (vellumory--tile-right tile) left))
           (height (- (vellumory--row-bottom row) top))
           areas)
      (cl-loop for link across links do
               (let ((x0 (max 0 (- (round (/ (alist-get 'x0 link) k)) left)))
                     (y0 (max 0 (- (round (/ (alist-get 'y0 link) k)) top)))
                     (x1 (min width (- (round (/ (alist-get 'x1 link) k)) left)))
                     (y1 (min height (- (round (/ (alist-get 'y1 link) k)) top))))
                 (when (and (< x0 x1) (< y0 y1))
                   (push `((rect . ((,x0 . ,y0) . (,x1 . ,y1))) vellumory-link
                           (pointer hand help-echo ,(vellumory--link-echo link)))
                         areas))))
      areas)))

(defun vellumory--visit-link (link)
  "Follow LINK: show its target in the document, or open its URI.
Going to a page is a jump `vellumory-history-back' returns from.  A URI is
opened with `browse-url', after asking when
`vellumory-confirm-external-links' is non-nil."
  (if-let* ((page (alist-get 'page link)))
      (progn
        (setq vellumory--last-motion 0.0)
        (push-mark nil t)
        (vellumory--remember-position)
        (vellumory--restore-position `((page . ,page) (y . ,(max 0 (- (alist-get 'y link) 12)))))
        (vellumory--refresh t))
    (let ((uri (alist-get 'uri link)))
      (when (and (not (string-empty-p uri))
                 (or (not vellumory-confirm-external-links)
                     (y-or-n-p (format "Open %s? " uri))))
        (browse-url uri)))))

;;;; History of jumps

(defvar-local vellumory--back nil)
(defvar-local vellumory--forward nil)
(declare-function vellumory--reading-position "vellumory" (&optional window))

(defun vellumory--remember-position ()
  "Record the reading position for `vellumory-history-back'.
The positions `vellumory-history-forward' would return to are forgotten."
  (when-let* ((position (vellumory--reading-position)))
    (push position vellumory--back) (setq vellumory--forward nil)))

(defun vellumory-history-back ()
  "Return to the position before the last link or jump."
  (interactive)
  (unless vellumory--back (user-error "No earlier position"))
  (push (vellumory--reading-position) vellumory--forward)
  (vellumory--restore-position (pop vellumory--back))
  (vellumory--refresh t))

(defun vellumory-history-forward ()
  "Undo `vellumory-history-back'."
  (interactive)
  (unless vellumory--forward (user-error "No later position"))
  (push (vellumory--reading-position) vellumory--back)
  (vellumory--restore-position (pop vellumory--forward))
  (vellumory--refresh t))

(defun vellumory-follow-link ()
  "Label every visible link and follow the one whose label is typed."
  (interactive)
  (let ((candidates nil) (pages nil))
    (dolist (tile vellumory--visible)
      (let ((page (vellumory--tile-page tile)))
        (unless (memq page pages)
          (push page pages)
          (cl-loop for link across (or (vellumory--ensure-links page) [])
                   do (push (cons page link) candidates)))))
    (unless candidates (user-error "No links visible (or still loading)"))
    (setq candidates (nreverse candidates))
    (let* ((keys vellumory-link-hint-keys)
           (labels (vellumory--hint-labels (length candidates) keys))
           (table (cl-mapcar #'cons labels candidates)))
      (setq vellumory--hints
            (mapcar (lambda (page)
                      (cons page (cl-loop for (_ . (p . link)) in table when (= p page)
                                          collect (vellumory--rect-quad
                                                   (alist-get 'x0 link) (alist-get 'y0 link)
                                                   (alist-get 'x1 link) (alist-get 'y1 link)))))
                    pages))
      (vellumory--refresh t)
      (unwind-protect
          (let* ((choice (completing-read
                          "Link: "
                          (mapcar (lambda (entry)
                                    (concat (car entry) "  " (vellumory--link-help (cddr entry))))
                                  table)
                          nil t))
                 (label (car (split-string choice " "))))
            (when-let* ((entry (assoc label table))) (vellumory--visit-link (cddr entry))))
        (setq vellumory--hints nil)
        (vellumory--refresh t)))))

(defun vellumory--hint-labels (count keys)
  "Return COUNT distinct labels made of the characters of the string KEYS.
All labels have the same length, the shortest that suffices."
  (let ((width 1) labels)
    (while (> count (expt (length keys) width)) (cl-incf width))
    (dotimes (i count)
      (let ((n i) (label ""))
        (dotimes (_ width)
          (setq label (concat (string (aref keys (% n (length keys)))) label) n (/ n (length keys))))
        (push label labels)))
    (nreverse labels)))

;;;; Selection

(defun vellumory--select-range (start end mode callback)
  "Select from START to END, each (PAGE X Y), in units of MODE.
MODE is `chars', `words' or `lines'.  The worker selects on every page in
between; CALLBACK then gets a list of (PAGE TEXT QUADS) in page order."
  (let* ((a (if (or (< (car start) (car end))
                    (and (= (car start) (car end)) (<= (nth 2 start) (nth 2 end))))
                start end))
         (b (if (eq a start) end start))
         (pages (number-sequence (car a) (car b)))
         (results (make-vector (length pages) nil))
         (remaining (length pages)) (hash vellumory--hash))
    (cl-loop for page in pages for index from 0 do
             (let ((index index) (page page))
               (vellumory--rpc
                "select"
                `((page . ,page) (mode . ,(symbol-name mode))
                  ,@(and (= page (car a)) `((start . ((x . ,(nth 1 a)) (y . ,(nth 2 a))))))
                  ,@(and (= page (car b)) `((end . ((x . ,(nth 1 b)) (y . ,(nth 2 b)))))))
                (lambda (reply)
                  (when (equal hash vellumory--hash)
                    (let ((result (alist-get 'result reply)))
                      (aset results index (list page (or (alist-get 'text result) "")
                                                (or (alist-get 'quads result) []))))
                    (when (zerop (cl-decf remaining))
                      (funcall callback (append results nil))))))))))

(defun vellumory--set-selection (selections)
  "Show SELECTIONS, less those without text, as the selection.
On a graphical display its text also becomes the primary selection."
  (setq vellumory--selection (cl-remove-if (lambda (s) (equal (nth 1 s) "")) selections))
  (vellumory--refresh t)
  (let ((text (vellumory-selection-text)))
    (when (and text (display-graphic-p))
      (gui-set-selection 'PRIMARY text))))

(defun vellumory-selection-text ()
  "The selected text, or nil."
  (when vellumory--selection
    (mapconcat (lambda (s) (nth 1 s)) vellumory--selection "\n")))

(defun vellumory-copy-selection ()
  "Copy the selected text to the kill ring."
  (interactive)
  (let ((text (vellumory-selection-text)))
    (unless text (user-error "No text selected"))
    (kill-new text)
    (message "Copied %d characters" (length text))))

(defun vellumory-clear-selection ()
  "Remove the text selection."
  (interactive)
  (when vellumory--selection (setq vellumory--selection nil) (vellumory--refresh t)))

(defun vellumory-mouse-press (event)
  "Follow a link, or select text by dragging from EVENT."
  (interactive "e")
  (let* ((posn (event-start event)) (window (posn-window posn)))
    (when (windowp window)
      (select-window window)
      (when-let* ((start (vellumory--event-location posn)))
        (let ((link (apply #'vellumory--link-at start)) (field (apply #'vellumory--form-field-at start))
              (moved nil) (last start) (busy nil))
          (track-mouse
            (cl-loop for next = (read-event)
                     while (mouse-movement-p next)
                     do (when-let* ((here (vellumory--event-location (event-start next))))
                          (when (and (not (equal here last))
                                     (> (+ (abs (- (nth 1 here) (nth 1 start)))
                                           (abs (- (nth 2 here) (nth 2 start)))
                                           (if (= (car here) (car start)) 0 1000))
                                        2))
                            (setq moved t last here)
                            (unless busy
                              (setq busy t)
                              (vellumory--select-range
                               start here 'chars
                               (lambda (selections)
                                 (setq busy nil)
                                 (setq vellumory--selection selections)
                                 (vellumory--refresh t))))))))
          (cond (moved (vellumory--select-range start last 'chars #'vellumory--set-selection))
                (field (vellumory-fill-field field))
                (link (vellumory--visit-link link))
                (vellumory-caret-mode (apply #'vellumory-caret--place-at start))
                (t (vellumory-clear-selection))))))))

(defun vellumory--mouse-select-unit (event mode)
  "Select the unit of MODE, `words' or `lines', under EVENT."
  (when-let* ((posn (event-start event)) (here (vellumory--event-location posn)))
    (vellumory--select-range here here mode #'vellumory--set-selection)))

(defun vellumory-mouse-select-word (event)
  "Select the word under EVENT." (interactive "e")
  (vellumory--mouse-select-unit event 'words))

(defun vellumory-mouse-select-line (event)
  "Select the line under EVENT." (interactive "e")
  (vellumory--mouse-select-unit event 'lines))

(provide 'vellumory-select)
;;; vellumory-select.el ends here
