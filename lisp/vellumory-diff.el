;;; vellumory-diff.el --- What changed between two versions -*- lexical-binding: t; -*-

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
;; `vellumory-diff' compares an earlier version with this document: pages are
;; aligned by their text, changed pages are compared word by word, and
;; metadata and structure are compared field by field.  Added text is
;; highlighted in the reader; removed text is listed.  From the history tree,
;; `d' compares any recorded state with the current file.

;;; Code:
(require 'cl-lib)
(require 'button)

(defvar vellumory--source-file)
(declare-function vellumory--cli "vellumory-edit" (args callback &optional buffer input))
(declare-function vellumory-goto-page "vellumory" (page))
(declare-function vellumory--refresh "vellumory" (&optional force))

(defface vellumory-diff-added '((t :inherit diff-added)) "Text added in this version." :group 'vellumory)
(defface vellumory-diff-removed '((t :inherit diff-removed :strike-through t))
  "Text removed since the other version." :group 'vellumory)
(defface vellumory-highlight-added '((((background dark)) :background "#2f6f3f") (t :background "#9be69b"))
  "Raster highlight for added text." :group 'vellumory)

(defvar-local vellumory--diff-added nil "Page → list of quad vectors added in this version.")
(defvar-local vellumory--diff-reader nil)

(defun vellumory--diff-quads (page)
  "Added-text polygons on PAGE for the renderer."
  (alist-get page vellumory--diff-added))

(defun vellumory--diff-jump (reader page)
  "Show zero-based PAGE in READER."
  (unless (buffer-live-p reader) (user-error "The document has been closed"))
  (pop-to-buffer reader)
  (vellumory-goto-page (1+ page)))

(defun vellumory--diff-render (reader result)
  "Show RESULT, the comparison, in this buffer.
Its page buttons show the page in READER."
  (let ((inhibit-read-only t) (summary (alist-get 'summary result)))
    (erase-buffer)
    (insert (propertize (format "%s → %s" (abbreviate-file-name (alist-get 'path (alist-get 'old result)))
                                (abbreviate-file-name (alist-get 'path (alist-get 'new result))))
                        'face 'bold)
            "\n"
            (format "%d pages changed, %d added, %d removed · %s words added, %s removed\n"
                    (alist-get 'changed_pages summary) (alist-get 'added_pages summary)
                    (alist-get 'removed_pages summary) (alist-get 'added_words summary)
                    (alist-get 'removed_words summary)))
    (when (> (length (alist-get 'structure result)) 0)
      (insert "\n" (propertize "Document\n" 'face 'bold))
      (seq-doseq (change (alist-get 'structure result))
        (insert (format "  %-14s " (alist-get 'field change))
                (propertize (format "%s" (alist-get 'old change)) 'face 'vellumory-diff-removed) " → "
                (propertize (format "%s" (alist-get 'new change)) 'face 'vellumory-diff-added) "\n")))
    (seq-doseq (page (alist-get 'pages result))
      (let ((new (alist-get 'new_page page)) (old (alist-get 'old_page page)) (start (point)))
        (insert "\n")
        (if new
            (insert-text-button (format "Page %d" (1+ new))
                                'action (lambda (_) (vellumory--diff-jump reader new))
                                'help-echo "Show this page")
          (insert (propertize (format "Old page %d" (1+ old)) 'face 'bold)))
        (insert (format "  %s · −%s +%s\n" (alist-get 'status page)
                        (alist-get 'removed_words page) (alist-get 'added_words page)))
        ;; Turned or cropped, with or without changed words.
        (when-let* ((geometry (alist-get 'geometry page)))
          (pcase-dolist (`(,field . ,label) '((rotate . "rotation") (media_box . "media box") (crop_box . "crop box")
                                              (bleed_box . "bleed box") (trim_box . "trim box") (art_box . "art box")))
            (let ((old (alist-get field (alist-get 'old geometry))) (new (alist-get field (alist-get 'new geometry))))
              (unless (equal old new)
                (insert (format "  %s  %s → %s\n" label (vellumory--diff-geometry old) (vellumory--diff-geometry new)))))))
        (seq-doseq (phrase (alist-get 'removed page))
          (insert "  − " (propertize (truncate-string-to-width (alist-get 'text phrase) 200 nil nil "…")
                                     'face 'vellumory-diff-removed)
                  "\n"))
        (seq-doseq (phrase (alist-get 'added page))
          (insert "  + " (propertize (truncate-string-to-width (alist-get 'text phrase) 200 nil nil "…")
                                     'face 'vellumory-diff-added)
                  "\n"))
        (put-text-property start (point) 'vellumory-diff-page new)))
    (goto-char (point-min))))

(defun vellumory--diff-geometry (value)
  "VALUE, a page's rotation (a number) or box ([X0 Y0 X1 Y1]), as words."
  (pcase value
    ('nil "none")
    ((pred numberp) (format "%d°" value))
    (`[,x0 ,y0 ,x1 ,y1] (format "%g × %g pt at %g, %g" (- x1 x0) (- y1 y0) x0 y0))
    (_ (format "%s" value))))

(defun vellumory--diff-highlight (reader result)
  "Give READER's renderer the added text of RESULT."
  (with-current-buffer reader
    (setq vellumory--diff-added
          (cl-loop for page across (alist-get 'pages result)
                   when (alist-get 'new_page page)
                   collect (cons (alist-get 'new_page page)
                                 (cl-loop for phrase across (alist-get 'added page)
                                          append (mapcar (lambda (q)
                                                           (vector (alist-get 'x0 q) (alist-get 'y0 q)
                                                                   (alist-get 'x1 q) (alist-get 'y1 q)
                                                                   (alist-get 'x2 q) (alist-get 'y2 q)
                                                                   (alist-get 'x3 q) (alist-get 'y3 q)))
                                                         (alist-get 'quads phrase))))))
    (vellumory--refresh t)))

(defun vellumory--diff-cleanup ()
  "Remove the added-text highlights from the compared reader."
  (when (buffer-live-p vellumory--diff-reader)
    (with-current-buffer vellumory--diff-reader
      (setq vellumory--diff-added nil)
      (vellumory--refresh t))))

(defun vellumory-diff-next () "Next changed page." (interactive)
  (forward-line 1)
  (while (and (not (eobp)) (not (button-at (point)))) (forward-line 1)))
(defun vellumory-diff-previous () "Previous changed page." (interactive)
  (forward-line -1)
  (while (and (not (bobp)) (not (button-at (point)))) (forward-line -1)))

(defvar vellumory-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "n" #'vellumory-diff-next)
    (keymap-set map "p" #'vellumory-diff-previous)
    (keymap-set map "TAB" #'forward-button)
    map))

(define-derived-mode vellumory-diff-mode special-mode "PDF Diff"
  "Changes between two versions of a document.
Added text is highlighted in the reader until this buffer is killed.
\\{vellumory-diff-mode-map}"
  (add-hook 'kill-buffer-hook #'vellumory--diff-cleanup nil t))

;;;###autoload
(defun vellumory-diff (old)
  "Show what changed from OLD, an earlier version, to this document."
  (interactive (list (read-file-name "Compare with earlier version: " nil nil t)))
  (let* ((reader (current-buffer))
         (new (or vellumory--source-file (user-error "Comparison needs a local file")))
         (buffer (get-buffer-create (format "*Diff: %s*" (file-name-nondirectory new)))))
    (with-current-buffer buffer
      (vellumory-diff-mode)
      (setq vellumory--diff-reader reader)
      (let ((inhibit-read-only t)) (erase-buffer) (insert "Comparing…\n")))
    (display-buffer buffer)
    (vellumory--cli (list "diff" (expand-file-name old) new)
                    (lambda (result)
                      (with-current-buffer buffer (vellumory--diff-render reader result))
                      (vellumory--diff-highlight reader result))
                    buffer)))

(provide 'vellumory-diff)
;;; vellumory-diff.el ends here
