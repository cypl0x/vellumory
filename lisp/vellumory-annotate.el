;;; vellumory-annotate.el --- Highlights, underlines and notes -*- lexical-binding: t; -*-

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
;; Annotations are recorded, undoable edits like every other change.  Mark
;; the selected text (`a h' highlight, `a u' underline, `a s' strike out,
;; `a q' squiggly underline), or click to place a note (`a n').  The marked
;; text becomes the annotation's contents unless a comment is given (with a
;; prefix argument), so the annotation reads well to screen readers too.
;; `a l' lists every annotation: RET shows it, `e' edits its text, `d'
;; deletes it.  Hovering an annotation on the page shows its text.

;;; Code:
(require 'cl-lib)
(require 'tabulated-list)

(defvar vellumory--selection)
(defvar vellumory--pages)
(defvar vellumory--hash)
(declare-function vellumory--edit "vellumory-edit" (operation))
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--quad-vector "vellumory-select" (quad))
(declare-function vellumory-clear-selection "vellumory-select" ())
(declare-function vellumory--event-location "vellumory-select" (posn))
(declare-function vellumory--redecorate-page "vellumory-select" (page))
(declare-function vellumory--remember-position "vellumory-select" ())
(declare-function vellumory--page-description "vellumory-navigation" (page))
(declare-function vellumory--pdf-date "vellumory-details" (date))
(declare-function vellumory--points-per-pixel "vellumory" (page))
(declare-function vellumory--tile-row "vellumory" (tile))
(declare-function vellumory--tile-page "vellumory" (tile))
(declare-function vellumory--tile-left "vellumory" (tile))
(declare-function vellumory--tile-right "vellumory" (tile))
(declare-function vellumory--row-top "vellumory" (row))
(declare-function vellumory--row-bottom "vellumory" (row))

(defgroup vellumory-annotate nil "Annotations." :group 'vellumory)

(defcustom vellumory-annotation-author (or user-full-name "")
  "Author recorded in new annotations." :type 'string)

(defcustom vellumory-annotation-colors
  '((Highlight 1.0 0.85 0.2) (Underline 0.1 0.45 0.9) (StrikeOut 0.85 0.15 0.15)
    (Squiggly 0.2 0.65 0.3) (Text 1.0 0.85 0.2))
  "RGB colours, 0 to 1, of each kind of annotation." :type '(alist :key-type symbol))

;;;; Creating

(defun vellumory--annotation-operation (kind page &rest fields)
  "The edit operation adding a KIND annotation on zero-based PAGE.
FIELDS is a plist of the optional :quads, :rect and :contents."
  `((op . "annotate") (page . ,(1+ page)) (kind . ,kind)
    (quads . ,(or (plist-get fields :quads) [])) (rect . ,(or (plist-get fields :rect) []))
    (color . ,(vconcat (alist-get (intern kind) vellumory-annotation-colors)))
    (contents . ,(or (plist-get fields :contents) "")) (author . ,vellumory-annotation-author)
    ;; Recorded, so replaying the edit dates the annotation identically.
    (time . ,(floor (float-time)))))

(defun vellumory--annotate-selection (kind comment)
  "Mark the selection with a KIND annotation per page.
Its contents are COMMENT, or else the marked text."
  (unless vellumory--selection (user-error "Select text first: drag, or double- or triple-click"))
  (pcase-dolist (`(,page ,text ,quads) vellumory--selection)
    (when (> (length quads) 0)
      (vellumory--edit
       (vellumory--annotation-operation
        kind page :contents (or comment (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))
        :quads (vconcat (cl-loop for quad across quads append (append (vellumory--quad-vector quad) nil)))))))
  (vellumory-clear-selection))

(defun vellumory--comment (arg)
  "A comment read from the user when ARG, the prefix argument, is non-nil."
  (and arg (read-string "Comment: ")))

;;;###autoload
(defun vellumory-highlight (&optional comment)
  "Highlight the selected text; with a prefix argument, ask for a COMMENT."
  (interactive (list (vellumory--comment current-prefix-arg)))
  (vellumory--annotate-selection "Highlight" comment))

;;;###autoload
(defun vellumory-underline (&optional comment)
  "Underline the selected text; with a prefix argument, ask for a COMMENT."
  (interactive (list (vellumory--comment current-prefix-arg)))
  (vellumory--annotate-selection "Underline" comment))

;;;###autoload
(defun vellumory-strike-out (&optional comment)
  "Strike out the selected text; with a prefix argument, ask for a COMMENT."
  (interactive (list (vellumory--comment current-prefix-arg)))
  (vellumory--annotate-selection "StrikeOut" comment))

;;;###autoload
(defun vellumory-squiggly (&optional comment)
  "Underline the selected text with a squiggle.
With a prefix argument, ask for a COMMENT."
  (interactive (list (vellumory--comment current-prefix-arg)))
  (vellumory--annotate-selection "Squiggly" comment))

;;;###autoload
(defun vellumory-add-note (location contents)
  "Put a note with CONTENTS at LOCATION, (PAGE X Y), chosen by a click."
  (interactive
   (let ((window (selected-window)) location)
     (while (not location)
       (let ((event (read-event "Click where the note goes")))
         (when (and (memq (car-safe event) '(mouse-1 down-mouse-1))
                    (eq window (posn-window (event-start event))))
           (setq location (vellumory--event-location (event-start event))))))
     (list location (read-string "Note: "))))
  (pcase-let ((`(,page ,x ,y) location))
    (vellumory--edit (vellumory--annotation-operation "Text" page :contents contents :rect (vector x y x y)))))

;;;; Reading: hover text on pages

(defvar-local vellumory--annotations nil "(HASH . table of page → vector of annotations or `pending').")

(defun vellumory--ensure-annotations (page)
  "PAGE's annotations once known; ask the worker the first time."
  (unless (equal (car vellumory--annotations) vellumory--hash)
    (setq vellumory--annotations (cons vellumory--hash (make-hash-table :test #'eql))))
  (let* ((table (cdr vellumory--annotations)) (known (gethash page table)))
    (cond ((vectorp known) known)
          ((eq known 'pending) nil)
          (t (puthash page 'pending table)
             (let ((hash vellumory--hash))
               (vellumory--rpc "annotations" `((page . ,page))
                               (lambda (reply)
                                 (when (equal hash vellumory--hash)
                                   (puthash page (or (alist-get 'annotations (alist-get 'result reply)) [])
                                            table)
                                   (vellumory--redecorate-page page)))))
             nil))))

(defun vellumory--annotation-help (annotation)
  "The hover text of ANNOTATION: its type, author and contents."
  (let ((contents (alist-get 'contents annotation)) (author (alist-get 'author annotation)))
    (concat (alist-get 'type annotation)
            (if (and author (not (string-empty-p author))) (concat " · " author) "")
            (if (and contents (not (string-empty-p contents))) (concat ": " contents) ""))))

(defun vellumory--annotation-areas (tile)
  "Image-map areas showing the text of TILE's annotations on hover."
  (when vellumory--pages
    (let* ((row (vellumory--tile-row tile)) (page (vellumory--tile-page tile))
           (annotations (vellumory--ensure-annotations page))
           (k (vellumory--points-per-pixel page))
           (left (vellumory--tile-left tile)) (top (vellumory--row-top row))
           (width (- (vellumory--tile-right tile) left))
           (height (- (vellumory--row-bottom row) top))
           areas)
      (seq-doseq (annotation annotations)
        (let* ((rect (alist-get 'rect annotation))
               (x0 (max 0 (- (round (/ (aref rect 0) k)) left))) (y0 (max 0 (- (round (/ (aref rect 1) k)) top)))
               (x1 (min width (- (round (/ (aref rect 2) k)) left)))
               (y1 (min height (- (round (/ (aref rect 3) k)) top))))
          (when (and (< x0 x1) (< y0 y1))
            (push `((rect . ((,x0 . ,y0) . (,x1 . ,y1))) vellumory-annotation
                    (help-echo ,(vellumory--annotation-help annotation)))
                  areas))))
      areas)))

;;;; The list

(defvar-local vellumory--annotation-list-source nil)

(defun vellumory--collect-annotations (callback)
  "Call CALLBACK with every annotation, each with its zero-based `page'."
  (let* ((count (length vellumory--pages)) (results (make-vector count nil)) (remaining count))
    (dotimes (page count)
      (let ((page page))
        (vellumory--rpc "annotations" `((page . ,page))
                        (lambda (reply)
                          (aset results page (mapcar (lambda (a) (cons (cons 'page page) a))
                                                     (alist-get 'annotations (alist-get 'result reply))))
                          (when (zerop (cl-decf remaining))
                            (funcall callback (apply #'append (append results nil))))))))))

(defun vellumory--annotation-list-render (annotations)
  "Show ANNOTATIONS in this list, one per line."
  (setq tabulated-list-entries
        (mapcar (lambda (a)
                  (let ((contents (or (alist-get 'contents a) "")))
                    (list a (vector (with-current-buffer vellumory--annotation-list-source
                                      (vellumory--page-description (alist-get 'page a)))
                                    (alist-get 'type a) (or (alist-get 'author a) "")
                                    (truncate-string-to-width (replace-regexp-in-string "\n" " " contents) 60 nil nil "…")
                                    (let ((date (alist-get 'modified a)))
                                      (if (and date (not (string-empty-p date))) (vellumory--pdf-date date) ""))))))
                annotations))
  (tabulated-list-print t))

(defun vellumory-annotation-list-refresh ()
  "Read the annotations again."
  (interactive)
  (let ((list (current-buffer)) (source vellumory--annotation-list-source))
    (unless (buffer-live-p source) (user-error "The document has been closed"))
    (with-current-buffer source
      (vellumory--collect-annotations
       (lambda (annotations)
         (when (buffer-live-p list)
           (with-current-buffer list (vellumory--annotation-list-render annotations))))))))

(defun vellumory--annotation-at-point ()
  "The annotation on this line of the list; signal an error if there is none."
  (or (tabulated-list-get-id) (user-error "No annotation on this line")))

(defun vellumory-annotation-list-visit ()
  "Show the annotation on this line in the document."
  (interactive)
  (let ((a (vellumory--annotation-at-point)) (source vellumory--annotation-list-source))
    (pop-to-buffer source)
    (with-current-buffer source
      (vellumory--remember-position)
      (vellumory--restore-position `((page . ,(alist-get 'page a)) (y . ,(max 0 (- (aref (alist-get 'rect a) 1) 24)))))
      (vellumory--refresh t))))

(defun vellumory-annotation-list-edit (contents)
  "Replace the text of the annotation on this line with CONTENTS."
  (interactive (list (read-string "Text: " (alist-get 'contents (vellumory--annotation-at-point)))))
  (let ((a (vellumory--annotation-at-point)) (list (current-buffer)))
    (with-current-buffer vellumory--annotation-list-source
      (vellumory--edit `((op . "annotation-edit") (page . ,(1+ (alist-get 'page a))) (object . ,(alist-get 'object a))
                         (contents . ,contents) (time . ,(floor (float-time))))))
    (run-at-time 1.5 nil (lambda () (when (buffer-live-p list) (with-current-buffer list (vellumory-annotation-list-refresh)))))))

(defun vellumory-annotation-list-delete ()
  "Delete the annotation on this line (an undoable edit)."
  (interactive)
  (let ((a (vellumory--annotation-at-point)) (list (current-buffer)))
    (when (y-or-n-p (format "Delete this %s? " (alist-get 'type a)))
      (with-current-buffer vellumory--annotation-list-source
        (vellumory--edit `((op . "annotation-delete") (page . ,(1+ (alist-get 'page a)))
                           (object . ,(alist-get 'object a)))))
      (run-at-time 1.5 nil (lambda () (when (buffer-live-p list) (with-current-buffer list (vellumory-annotation-list-refresh))))))))

(defvar vellumory-annotation-list-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'vellumory-annotation-list-visit)
    (keymap-set map "e" #'vellumory-annotation-list-edit)
    (keymap-set map "d" #'vellumory-annotation-list-delete)
    (keymap-set map "g" #'vellumory-annotation-list-refresh)
    map))

(define-derived-mode vellumory-annotation-list-mode tabulated-list-mode "PDF Annotations"
  "Annotations of a document.
\\{vellumory-annotation-list-mode-map}"
  (setq tabulated-list-format [("Page" 10 t) ("Type" 10 t) ("Author" 14 t) ("Text" 60 nil) ("Modified" 22 t)])
  (tabulated-list-init-header))

;;;###autoload
(defun vellumory-annotations ()
  "List every annotation of the document."
  (interactive)
  (unless vellumory--pages (user-error "PDF is still opening"))
  (let ((source (current-buffer))
        (buffer (get-buffer-create (format "*Annotations: %s*" (buffer-name)))))
    (with-current-buffer buffer
      (vellumory-annotation-list-mode)
      (setq vellumory--annotation-list-source source))
    (vellumory--collect-annotations
     (lambda (annotations)
       (with-current-buffer buffer (vellumory--annotation-list-render annotations))
       (pop-to-buffer buffer)
       (message "%d annotations" (length annotations))))))

(defvar vellumory-annotate-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "h" #'vellumory-highlight)
    (keymap-set map "u" #'vellumory-underline)
    (keymap-set map "s" #'vellumory-strike-out)
    (keymap-set map "q" #'vellumory-squiggly)
    (keymap-set map "n" #'vellumory-add-note)
    (keymap-set map "l" #'vellumory-annotations)
    map)
  "Annotation commands, under `a' in readers.")

(provide 'vellumory-annotate)
;;; vellumory-annotate.el ends here
