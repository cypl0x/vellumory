;;; vellumory-metadata.el --- Edit document metadata -*- lexical-binding: t; -*-

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
;; `vellumory-edit-metadata' shows the title, author, subject, keywords,
;; creator, producer, language, title display and creation date as a form.
;; C-c C-c records the changes as one undoable edit.  The worker keeps the
;; Info dictionary, the XMP packet and the catalog consistent, as PDF/A and
;; PDF/UA require, and stamps the modification date.  Emptying a field
;; removes it.

;;; Code:
(require 'text-property-search)
(require 'cl-lib)
(require 'subr-x)

(defvar vellumory--source-file)
(declare-function vellumory--rpc "vellumory" (method params callback))
(declare-function vellumory--edit "vellumory-edit" (operation))

(defconst vellumory-metadata--fields
  '(("title" "Title" title) ("author" "Author" author) ("subject" "Subject" subject)
    ("keywords" "Keywords" keywords) ("creator" "Creator" creator) ("producer" "Producer" producer)
    ("language" "Language" language) ("display-title" "Show title" display_title)
    ("created" "Created" created))
  "Field name, label, and the key of the inspection report it starts from.")

(defvar-local vellumory-metadata--source nil "Reader buffer the form edits.")
(defvar-local vellumory-metadata--original nil "Field → value when the form was shown.")

(defun vellumory-metadata--iso-date (pdf)
  "The PDF date string PDF as ISO 8601, or PDF itself if it does not parse.
An ISO 8601 date (from XMP, in PDF 2.0 documents) is returned as it is."
  (if (and (stringp pdf) (not (string-match-p "\\`[0-9]\\{4\\}-" pdf))
           (string-match (concat "\\`\\(?:D:\\)?\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)?\\([0-9]\\{2\\}\\)?"
                                 "\\([0-9]\\{2\\}\\)?\\([0-9]\\{2\\}\\)?\\([0-9]\\{2\\}\\)?"
                                 "\\(Z\\|[-+]\\([0-9]\\{2\\}\\)'?\\([0-9]\\{2\\}\\)?\\)?")
                         pdf))
      (let ((part (lambda (n default) (or (match-string n pdf) default))))
        (concat (match-string 1 pdf) "-" (funcall part 2 "01") "-" (funcall part 3 "01")
                (when (match-string 4 pdf)
                  (concat "T" (match-string 4 pdf) ":" (funcall part 5 "00") ":" (funcall part 6 "00")
                          (let ((zone (match-string 7 pdf)))
                            (cond ((null zone) "")
                                  ((or (equal zone "Z") (and (equal (match-string 8 pdf) "00")
                                                              (member (match-string 9 pdf) '(nil "00"))))
                                   "Z")
                                  (t (format "%c%s:%s" (aref zone 0) (match-string 8 pdf)
                                             (funcall part 9 "00")))))))))
    (or pdf "")))

(defun vellumory-metadata--initial (key info)
  "The form's text for KEY of the inspection report INFO."
  (let ((value (alist-get key info)))
    (pcase key
      ('display_title (if (eq value t) "yes" "no"))
      ('created (if value (vellumory-metadata--iso-date value) ""))
      (_ (if (stringp value) value "")))))

(defun vellumory-metadata--render (info)
  "Show the metadata fields of the inspection report INFO as a form.
Remember the values shown, to find what changed."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Metadata of %s\n" (file-name-nondirectory
                                                    (buffer-local-value 'vellumory--source-file
                                                                        vellumory-metadata--source)))
                        'face 'bold 'read-only t 'rear-nonsticky t))
    (insert (propertize (substitute-command-keys
                         (concat "\\<vellumory-metadata-mode-map>\\[vellumory-metadata-commit] records one undoable edit; "
                                 "an empty field is removed.\nInfo, XMP and catalog stay consistent.\n\n"))
                        'face 'shadow 'read-only t 'rear-nonsticky t))
    (setq vellumory-metadata--original nil)
    (pcase-dolist (`(,field ,label ,key) vellumory-metadata--fields)
      (let ((value (vellumory-metadata--initial key info)))
        (push (cons field value) vellumory-metadata--original)
        (insert (propertize (format "%-12s" (concat label ":")) 'read-only t 'rear-nonsticky t
                            'front-sticky t 'face 'font-lock-keyword-face 'vellumory-metadata-field field)
                value "\n")))
    (goto-char (point-min))
    (text-property-search-forward 'vellumory-metadata-field)
    (set-buffer-modified-p nil)))

(defun vellumory-metadata--values ()
  "Field → value as the form now reads."
  (save-excursion
    (goto-char (point-min))
    (let (values)
      (while (not (eobp))
        (when-let* ((field (get-text-property (point) 'vellumory-metadata-field))
                    (end (next-single-property-change (point) 'vellumory-metadata-field nil
                                                      (line-end-position))))
          (push (cons field (string-trim (buffer-substring-no-properties end (line-end-position)))) values))
        (forward-line 1))
      values)))

(defun vellumory-metadata--operation (values original date)
  "The edit operation that turns ORIGINAL into VALUES, stamped with DATE."
  (let (set remove)
    (pcase-dolist (`(,field . ,value) values)
      (let ((old (alist-get field original nil nil #'equal)))
        (unless (equal value old)
          (cond ((string-empty-p value) (push field remove))
                ((equal field "display-title")
                 (push (cons (intern field)
                             (cond ((member (downcase value) '("yes" "true" "t" "on")) "true")
                                   ((member (downcase value) '("no" "false" "nil" "off")) "false")
                                   (t (user-error "Show title is yes or no"))))
                       set))
                (t (push (cons (intern field) value) set))))))
    (when (and (null set) (null remove)) (user-error "Nothing changed"))
    `((op . "metadata") (set . ,(or (nreverse set) (make-hash-table))) (remove . ,(vconcat (nreverse remove)))
      (date . ,date))))

(defun vellumory-metadata-commit ()
  "Record the form's changes as one undoable edit of the document."
  (interactive)
  (let ((source vellumory-metadata--source)
        (operation (vellumory-metadata--operation (vellumory-metadata--values) vellumory-metadata--original
                                                  (format-time-string "%FT%T%:z"))))
    (unless (buffer-live-p source) (user-error "The document has been closed"))
    (quit-window t)
    (with-current-buffer source (vellumory--edit operation))))

(defun vellumory-metadata-cancel ()
  "Discard the form."
  (interactive)
  (quit-window t))

(defvar vellumory-metadata-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'vellumory-metadata-commit)
    (keymap-set map "C-c C-k" #'vellumory-metadata-cancel)
    map))

(define-derived-mode vellumory-metadata-mode text-mode "PDF Metadata"
  "Edit a document's metadata; \\[vellumory-metadata-commit] records it.
\\{vellumory-metadata-mode-map}"
  (setq-local header-line-format
              (substitute-command-keys "\\<vellumory-metadata-mode-map>\\[vellumory-metadata-commit] record  \\[vellumory-metadata-cancel] cancel")))

;;;###autoload
(defun vellumory-edit-metadata ()
  "Edit this document's title, author, language and other metadata."
  (interactive)
  (unless vellumory--source-file (user-error "Only documents visited from a local file can be edited"))
  (let ((source (current-buffer))
        (buffer (get-buffer-create (format "*Metadata: %s*" (file-name-nondirectory vellumory--source-file)))))
    (vellumory--rpc "info" nil
                    (lambda (reply)
                      (if-let* ((error (alist-get 'error reply)))
                          (message "Vellumory: %s" error)
                        (with-current-buffer buffer
                          (vellumory-metadata-mode)
                          (setq vellumory-metadata--source source)
                          (vellumory-metadata--render (alist-get 'result reply)))
                        (pop-to-buffer buffer))))))

(provide 'vellumory-metadata)
;;; vellumory-metadata.el ends here
