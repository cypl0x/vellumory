;;; vellumory-thumbnails.el --- Visible PDF page previews -*- lexical-binding: t; -*-

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
;; `vellumory-thumbnails' (T) toggles a sidebar of page previews.  Every
;; page gets a placeholder of its proportions with its label beneath; only
;; the previews visible in the sidebar are rendered, at most two at a time
;; and below every reader tile in priority, and previews scrolled away are
;; released.  The page being read is highlighted and kept in view.  RET or
;; a click visits a page, n/j and p/k move between previews, s switches to
;; the contents.  A document opens with the sidebar shown when
;; `vellumory-show-thumbnails' is non-nil or the document asks for it
;; (/PageMode /UseThumbs).

;;; Code:
(require 'cl-lib)
(require 'button)
(require 'image)

(defvar vellumory--pages)
(defvar vellumory--hash)
(defvar vellumory--inflight)
(defvar vellumory--threads)
(defvar vellumory--scale)
(defvar vellumory--process)
(defvar vellumory--rendered-pixels)
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--theme-params "vellumory" ())
(declare-function vellumory-sidebar-switch "vellumory-navigation" (&optional event))
(declare-function vellumory--pump "vellumory" ())
(declare-function vellumory-mark-page "vellumory-edit" (&optional page))
(declare-function vellumory-unmark-all "vellumory-edit" ())
(declare-function vellumory-export-marked-pages "vellumory-edit" (output))
(declare-function vellumory--defer-gc "vellumory" ())
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--reading-position "vellumory" (&optional window))
(declare-function vellumory--cancel "vellumory" (ids))
(declare-function vellumory--page-description "vellumory-navigation" (page))
(declare-function vellumory--show-sidebar "vellumory-navigation" (buffer columns))
(declare-function vellumory-goto-page "vellumory" (page))

(defgroup vellumory-thumbnails nil "PDF page previews." :group 'vellumory)
(defcustom vellumory-show-thumbnails t
  "Show page previews when a PDF is first opened." :type 'boolean)
(defcustom vellumory-thumbnail-width 136
  "Maximum thumbnail width in pixels." :type 'integer)
(defcustom vellumory-thumbnail-height 192
  "Maximum thumbnail height in pixels." :type 'integer)

(cl-defstruct (vellumory--thumbnail (:constructor vellumory--thumbnail-create))
  page width height position label-end image state)

;; These live in the document buffer; only the source reference lives in the sidebar.
(defvar-local vellumory--thumbnail-buffer nil)
(defvar-local vellumory--thumbnails [])
(defvar-local vellumory--thumbnail-visible nil)
(defvar-local vellumory--thumbnail-queue nil)
(defvar-local vellumory--thumbnail-generation 0)
(defvar-local vellumory--thumbnail-current nil)
(defvar-local vellumory--thumbnail-initialized nil)
(defvar-local vellumory--thumbnail-source nil)
(defvar-local vellumory--thumbnail-inflight nil "Request id → item for preview renders.")

(defun vellumory--thumbnail-placeholder (item)
  "Return a blank display spec the size of ITEM's preview."
  `(space :width (,(vellumory--thumbnail-width item))
          :height (,(vellumory--thumbnail-height item)) :ascent 100))

(defun vellumory--thumbnail-display (item image state)
  "Show IMAGE as ITEM's preview in the sidebar of the current document.
A nil IMAGE shows the placeholder; STATE becomes ITEM's state.  Released
images are left to cache eviction: `image-flush' garbages the frame."
  (when (buffer-live-p vellumory--thumbnail-buffer)
    (with-current-buffer vellumory--thumbnail-buffer
      (with-silent-modifications
        (let ((pos (vellumory--thumbnail-position item)))
          (put-text-property pos (1+ pos) 'display
                             (or image (vellumory--thumbnail-placeholder item)))))))
  (setf (vellumory--thumbnail-image item) image (vellumory--thumbnail-state item) state))

(defun vellumory--thumbnail-visit (source hash page)
  "Show the zero-based PAGE in the reader SOURCE.
Refuse when SOURCE is gone or its document's content is no longer HASH."
  (unless (buffer-live-p source) (user-error "The PDF buffer has been closed"))
  (with-current-buffer source
    (unless (equal hash vellumory--hash) (user-error "The PDF changed; choose an updated preview"))
    (pop-to-buffer source)
    (vellumory-goto-page (1+ page))))

(declare-function vellumory--call-deferring-gc "vellumory" (fn))

(defun vellumory--thumbnail-layout ()
  "Lay out page placeholders, without requesting any raster work."
  (vellumory--call-deferring-gc #'vellumory--thumbnail-layout-1))

(defun vellumory--thumbnail-layout-1 ()
  "Do the work of `vellumory--thumbnail-layout'.
Cancel preview renders, release shown previews and fill the sidebar with a
placeholder button and a label button for each page."
  (vellumory--cancel-thumbnails)
  (cl-incf vellumory--thumbnail-generation)
  (dolist (item vellumory--thumbnail-visible) (vellumory--thumbnail-display item nil nil))
  (setq vellumory--thumbnail-visible nil vellumory--thumbnail-queue nil
        vellumory--thumbnail-current nil)
  (let ((source (current-buffer)) (pages vellumory--pages) (hash vellumory--hash)
        (labels (cl-loop for page below (length vellumory--pages)
                         collect (vellumory--page-description page))) items)
    (with-current-buffer vellumory--thumbnail-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq vellumory--thumbnail-source source)
        (cl-loop for size across pages for page from 0 for label in labels do
                 (let* ((page page)
                        (ratio (/ (float (alist-get 'height size)) (alist-get 'width size)))
                        (width (max 1 (min (max 1 vellumory-thumbnail-width)
                                           (floor (/ (max 1 vellumory-thumbnail-height) ratio)))))
                        (height (max 1 (ceiling (* width ratio))))
                        (start (point))
                        (item (vellumory--thumbnail-create :page page :width width :height height
                                                           :position (1+ start))))
                   (insert " ")
                   (insert-text-button " " 'display (vellumory--thumbnail-placeholder item)
                                       'face 'default
                                       'follow-link t 'help-echo (concat "Go to page " label)
                                       'action (lambda (_) (vellumory--thumbnail-visit source hash page)))
                   (insert (propertize "\n" 'line-height t) " ")
                   (insert-text-button label 'follow-link t
                                       'action (lambda (_) (vellumory--thumbnail-visit source hash page)))
                   (setf (vellumory--thumbnail-label-end item) (point))
                   (insert "\n\n")
                   (put-text-property start (point) 'vellumory-thumbnail-page page)
                   (push item items)))
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (setq vellumory--thumbnails (vconcat (nreverse items)))
    (vellumory--thumbnail-show-marks)))

(defface vellumory-marked '((t :inherit warning :weight bold))
  "Labels of pages marked for export in the page previews.")

(defvar vellumory--marked-pages)
(defvar-local vellumory--mark-overlays nil "Overlays showing the marked pages.")

(defun vellumory--thumbnail-show-marks ()
  "Show this document's pages marked for export in its page previews."
  (when (buffer-live-p vellumory--thumbnail-buffer)
    (let ((marked vellumory--marked-pages) (items vellumory--thumbnails))
      (with-current-buffer vellumory--thumbnail-buffer
        (mapc #'delete-overlay vellumory--mark-overlays)
        (setq vellumory--mark-overlays nil)
        (dolist (page marked)
          (when-let* ((item (and (<= page (length items)) (aref items (1- page))))
                      (end (vellumory--thumbnail-label-end item))
                      (button (button-at (1- end))))
            (let ((overlay (make-overlay (button-start button) end)))
              (overlay-put overlay 'face 'vellumory-marked)
              (overlay-put overlay 'before-string (propertize "✓ " 'face 'vellumory-marked))
              (push overlay vellumory--mark-overlays))))))))

(defun vellumory--thumbnail-mark (how)
  "Change the export marks from the page previews, as HOW says.
HOW is `mark' (the page at point), `unmark' (it), or `clear' (every mark)."
  (let ((page (get-text-property (point) 'vellumory-thumbnail-page))
        (source vellumory--thumbnail-source))
    (unless (buffer-live-p source) (user-error "The PDF buffer has been closed"))
    (with-current-buffer source
      (pcase how
        ('clear (vellumory-unmark-all))
        (_ (unless page (user-error "No page at point"))
           (when (eq (and (memq (1+ page) vellumory--marked-pages) t) (eq how 'unmark))
             (vellumory-mark-page (1+ page))))))
    (when (and page (memq how '(mark unmark))) (vellumory-thumbnail-next))))

(defun vellumory-thumbnail-mark-page ()
  "Mark the page at point for export and move to the next, as in Dired."
  (interactive)
  (vellumory--thumbnail-mark 'mark))

(defun vellumory-thumbnail-unmark-page ()
  "Unmark the page at point and move to the next."
  (interactive)
  (vellumory--thumbnail-mark 'unmark))

(defun vellumory-thumbnail-unmark-all ()
  "Clear every mark of this document."
  (interactive)
  (vellumory--thumbnail-mark 'clear))

(defun vellumory-thumbnail-export-marked ()
  "Export the pages marked for export to a new document."
  (interactive)
  (let ((source vellumory--thumbnail-source))
    (unless (buffer-live-p source) (user-error "The PDF buffer has been closed"))
    (with-current-buffer source (call-interactively #'vellumory-export-marked-pages))))

(defun vellumory--thumbnail-visible-items ()
  "Find visible previews, including a partially visible first or last preview."
  (let ((items vellumory--thumbnails) visible)
    (when (buffer-live-p vellumory--thumbnail-buffer)
      (with-current-buffer vellumory--thumbnail-buffer
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (let ((index (get-text-property (window-start window) 'vellumory-thumbnail-page)))
            (when index
              ;; A visible label alone does not require its offscreen raster.
              (when (pos-visible-in-window-p (vellumory--thumbnail-position (aref items index)) window t)
                (cl-pushnew (aref items index) visible :test #'eq))
              (cl-incf index)
              (while (and (< index (length items))
                          (pos-visible-in-window-p (vellumory--thumbnail-position (aref items index)) window t))
                (cl-pushnew (aref items index) visible :test #'eq)
                (cl-incf index)))))))
    (nreverse visible)))

(defun vellumory--thumbnail-mark-current (page)
  "Mark PAGE and keep it visible unless the user is browsing the sidebar."
  (unless (equal page vellumory--thumbnail-current)
    (let ((previous vellumory--thumbnail-current) (items vellumory--thumbnails))
      (setq vellumory--thumbnail-current page)
      (with-current-buffer vellumory--thumbnail-buffer
        (with-silent-modifications
          (dolist (entry `((,previous . default) (,page . highlight)))
            (when (and (natnump (car entry)) (< (car entry) (length items)))
              (let* ((item (aref items (car entry)))
                     (start (+ 3 (vellumory--thumbnail-position item))))
                (put-text-property start (vellumory--thumbnail-label-end item) 'face (cdr entry))))))
        (when (and (natnump page) (< page (length items)))
          (dolist (window (get-buffer-window-list (current-buffer) nil t))
            (unless (eq window (selected-window))
              (let ((pos (vellumory--thumbnail-position (aref items page))))
                (set-window-point window pos)
                (unless (pos-visible-in-window-p pos window)
                  (set-window-start window (1- pos) t)
                  (set-window-vscroll window 0 t))))))))))

(defvar-local vellumory--thumbnail-signature nil
  "Sidebar geometry and reader page when visibility was last computed.")

(defun vellumory--update-thumbnails ()
  "Replace pending preview work and release images outside visible sidebars.
Visibility is recomputed only when the sidebar's geometry or the page changes."
  (when (buffer-live-p vellumory--thumbnail-buffer)
    (vellumory--thumbnail-mark-current (alist-get 'page (vellumory--reading-position)))
    (let ((signature (cons vellumory--thumbnail-generation
                           (mapcar (lambda (w) (list (window-start w) (window-vscroll w t)
                                                     (window-body-height w t)))
                                   (get-buffer-window-list vellumory--thumbnail-buffer nil t)))))
      (unless (equal signature vellumory--thumbnail-signature)
        (setq vellumory--thumbnail-signature signature)
        (let ((visible (vellumory--thumbnail-visible-items)))
          (unless (equal visible vellumory--thumbnail-visible)
            (dolist (item vellumory--thumbnail-visible)
              (unless (memq item visible) (vellumory--thumbnail-display item nil nil)))
            (setq vellumory--thumbnail-visible visible)))))
    (setq vellumory--thumbnail-queue
          (cl-remove-if #'vellumory--thumbnail-state vellumory--thumbnail-visible))
    (vellumory--thumbnail-pump)))

(defun vellumory--thumbnail-pump ()
  "Render visible previews with spare worker capacity, below every reader tile."
  (unless vellumory--thumbnail-inflight
    (setq vellumory--thumbnail-inflight (make-hash-table :test #'eql)))
  (while (and vellumory--thumbnail-queue (process-live-p vellumory--process)
              (< (+ (hash-table-count vellumory--inflight) (hash-table-count vellumory--thumbnail-inflight))
                 (1+ vellumory--threads))
              (< (hash-table-count vellumory--thumbnail-inflight) 2))
    (let* ((item (pop vellumory--thumbnail-queue))
           (generation vellumory--thumbnail-generation) (hash vellumory--hash)
           (buffer vellumory--thumbnail-buffer)
           (scale vellumory--scale)
           (width (max 1 (round (* scale (vellumory--thumbnail-width item)))))
           (height (max 1 (round (* scale (vellumory--thumbnail-height item)))))
           (theme (vellumory--theme-params))
           id)
      (setf (vellumory--thumbnail-state item) 'pending)
      (setq id
            (vellumory--rpc
             "render" `((page . ,(vellumory--thumbnail-page item)) (width . ,width)
                        (top . 0) (bottom . ,height) (priority . 1000) (persist . t)
                        ,@(and theme `((theme . ,theme))))
             (lambda (reply)
               (remhash id vellumory--thumbnail-inflight)
               (setf (vellumory--thumbnail-state item) nil)
               (when (and (= generation vellumory--thumbnail-generation) (equal hash vellumory--hash)
                          (eq buffer vellumory--thumbnail-buffer) (buffer-live-p buffer)
                          (memq item vellumory--thumbnail-visible))
                 (if-let* ((error (alist-get 'error reply)))
                     (unless (alist-get 'cancelled reply)
                       (setf (vellumory--thumbnail-state item) 'error)
                       (message "Vellumory page preview: %s" error))
                   (let ((result (alist-get 'result reply)))
                     (cl-incf vellumory--rendered-pixels (or (alist-get 'pixels result) 0))
                     (vellumory--thumbnail-display
                      item (create-image (alist-get 'path result) 'pbm nil :scale 1.0 :ascent 100
                                         :width (vellumory--thumbnail-width item)
                                         :height (vellumory--thumbnail-height item)
                                         :transform-smoothing (/= scale 1.0))
                      'ready))))
               ;; Recompute visibility before sending another preview; reader work has priority.
               (vellumory--update-thumbnails))))
      (puthash id item vellumory--thumbnail-inflight))))

(defun vellumory--cancel-thumbnails ()
  "Cancel the preview renders in flight on the worker."
  (when vellumory--thumbnail-inflight
    (let (ids)
      (maphash (lambda (id item) (push id ids) (setf (vellumory--thumbnail-state item) nil))
               vellumory--thumbnail-inflight)
      (clrhash vellumory--thumbnail-inflight)
      (vellumory--cancel ids))))

(defvar vellumory--page-mode)
(declare-function vellumory--show-outline "vellumory-navigation" ())

(defun vellumory--reset-thumbnails ()
  "Rebuild previews after opening a snapshot; show them initially by default."
  (if (buffer-live-p vellumory--thumbnail-buffer)
      (vellumory--thumbnail-layout)
    (when (and (not vellumory--thumbnail-initialized) (get-buffer-window (current-buffer) t))
      ;; A document that asks for its outline (/PageMode /UseOutlines) gets it.
      (if (equal vellumory--page-mode "UseOutlines") (vellumory--show-outline)
        (when (or vellumory-show-thumbnails (equal vellumory--page-mode "UseThumbs"))
          (vellumory-thumbnails)))))
  (setq vellumory--thumbnail-initialized t)
  (vellumory--update-thumbnails))

(defun vellumory--thumbnails-post-command ()
  "Update the previews of this sidebar's document after a command."
  (when (buffer-live-p vellumory--thumbnail-source)
    (with-current-buffer vellumory--thumbnail-source (vellumory--update-thumbnails))))

(defun vellumory--thumbnails-cleanup ()
  "Release this sidebar's previews and detach it from its document."
  (when (buffer-live-p vellumory--thumbnail-source)
    (with-current-buffer vellumory--thumbnail-source
      (cl-incf vellumory--thumbnail-generation)
      (dolist (item vellumory--thumbnail-visible) (vellumory--thumbnail-display item nil nil))
      (setq vellumory--thumbnail-queue nil vellumory--thumbnail-visible nil
            vellumory--thumbnails [] vellumory--thumbnail-buffer nil))))

(defun vellumory-thumbnail-visit ()
  "Visit the page at point."
  (interactive)
  (let* ((page (get-text-property (point) 'vellumory-thumbnail-page))
         (items (buffer-local-value 'vellumory--thumbnails vellumory--thumbnail-source))
         (button (and page (button-at (vellumory--thumbnail-position (aref items page))))))
    (if button (button-activate button) (user-error "No page at point"))))

(defun vellumory-thumbnail-next (&optional backward)
  "Move to the next preview, or the previous one when BACKWARD is non-nil."
  (interactive)
  (let* ((items (buffer-local-value 'vellumory--thumbnails vellumory--thumbnail-source))
         (current (or (get-text-property (point) 'vellumory-thumbnail-page) 0))
         (page (max 0 (min (1- (length items)) (+ current (if backward -1 1))))))
    (when (> (length items) 0) (goto-char (vellumory--thumbnail-position (aref items page))))))

(defun vellumory-thumbnail-previous () "Move to the previous preview." (interactive) (vellumory-thumbnail-next t))

(defvar vellumory-thumbnails-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (dolist (key '("n" "j")) (define-key map (kbd key) #'vellumory-thumbnail-next))
    (dolist (key '("p" "k")) (define-key map (kbd key) #'vellumory-thumbnail-previous))
    (define-key map (kbd "RET") #'vellumory-thumbnail-visit)
    (define-key map (kbd "T") #'quit-window)
    (define-key map (kbd "s") #'vellumory-sidebar-switch)
    ;; Marking pages for export, as files are marked in Dired.
    (define-key map (kbd "m") #'vellumory-thumbnail-mark-page)
    (define-key map (kbd "u") #'vellumory-thumbnail-unmark-page)
    (define-key map (kbd "U") #'vellumory-thumbnail-unmark-all)
    (define-key map (kbd "x") #'vellumory-thumbnail-export-marked)
    map))

(define-derived-mode vellumory-thumbnails-mode special-mode "PDF Pages"
  "Browse page previews.  RET or a click visits a page; T closes the sidebar.
s shows the contents in its place."
  (setq-local truncate-lines t line-spacing 0 display-line-numbers nil
              header-line-format '(:eval (vellumory--sidebar-tabs 'pages "RET visit · m mark"))
              cursor-type nil)
  (add-hook 'kill-buffer-hook #'vellumory--thumbnails-cleanup nil t)
  (add-hook 'post-command-hook #'vellumory--thumbnails-post-command nil t)
  ;; A click here moves the reader: keep collection out of its redisplay too.
  (add-hook 'pre-command-hook #'vellumory--defer-gc nil t))

(defun vellumory--show-thumbnails ()
  "Show page previews in the left sidebar and return its window."
  (unless vellumory--pages (user-error "PDF is still opening"))
  (unless (buffer-live-p vellumory--thumbnail-buffer)
    (setq vellumory--thumbnail-buffer (generate-new-buffer (format "*Pages: %s*" (buffer-name))))
    (with-current-buffer vellumory--thumbnail-buffer (vellumory-thumbnails-mode))
    (vellumory--thumbnail-layout))
  (prog1 (vellumory--show-sidebar vellumory--thumbnail-buffer
                                 (max 24 (ceiling (/ (+ vellumory-thumbnail-width 40.0) (frame-char-width)))))
    ;; A sidebar shown again must scroll to the page being read now.
    (setq vellumory--thumbnail-current nil)
    (vellumory--refresh t)))

(defun vellumory-thumbnails ()
  "Toggle page previews in the left sidebar, preserving the PDF's position."
  (interactive)
  (unless vellumory--pages (user-error "PDF is still opening"))
  (if-let* ((window (and (buffer-live-p vellumory--thumbnail-buffer)
                         (get-buffer-window vellumory--thumbnail-buffer))))
      (progn (quit-window nil window) (vellumory--refresh t))
    (vellumory--show-thumbnails)))

(provide 'vellumory-thumbnails)
;;; vellumory-thumbnails.el ends here
