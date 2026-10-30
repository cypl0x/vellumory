;;; vellumory-dired.el --- Batch work on marked documents -*- lexical-binding: t; -*-

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
;; In Dired, `;' (`SPC m v' in Doom) acts on the marked documents:
;;   ; v  validate them in one veraPDF run
;;   ; e  apply one recorded, undoable edit to each (compress, OCR, language…)
;;   ; i  summarize format, claims, tagging and language
;;   ; m  merge them, in Dired order, into a new document
;;   ; o  list the lines matching a regexp in all of them, by document and page
;;   ; V  start a virtual document reading them in order
;; Results appear in a table; RET opens a document, `v' shows its full
;; validation report, `g' runs the batch again.

;;; Code:
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'cl-lib)
(require 'dired)
(require 'tabulated-list)

(defvar vellumory-program)
(defvar vellumory-validation-profiles)
(declare-function vellumory--cli "vellumory-edit" (args callback &optional buffer input))
(declare-function vellumory-open "vellumory" (file &optional no-select))
(declare-function vellumory-occur-documents "vellumory-search" (regexp files))
(declare-function vellumory-virtual-create "vellumory-virtual" (files &optional name))
(declare-function vellumory-validate "vellumory-validate" (flavour))
(declare-function vellumory--standards "vellumory-details" (info))
(declare-function vellumory--size-string "vellumory-details" (bytes))

(defvar-local vellumory-batch--rerun nil "Function that runs this batch again.")
(defvar-local vellumory-batch--flavour nil "The veraPDF profile of a validation batch.")

(defconst vellumory-dired-operations
  '(("Compress losslessly" . (lambda () '((op . "compress"))))
    ("Linearize (fast web view)" . (lambda () '((op . "linearize"))))
    ("Recognize text (OCR)" .
     (lambda () `((op . "ocr") (languages . ,(read-string "Tesseract languages: " "eng")) (mode . "skip"))))
    ("Set language" .
     (lambda () `((op . "metadata") (set (language . ,(read-string "Language tag (e.g. en-GB): ")))
                  (remove . []) (date . ,(format-time-string "%FT%T%:z")))))
    ("Set author" .
     (lambda () `((op . "metadata") (set (author . ,(read-string "Author: ")))
                  (remove . []) (date . ,(format-time-string "%FT%T%:z")))))
    ("Show the title in viewers' title bars" .
     (lambda () `((op . "metadata") (set (display-title . "true")) (remove . [])
                  (date . ,(format-time-string "%FT%T%:z")))))
    ("Remove page labels" . (lambda () '((op . "remove-labels")))))
  "Whole-document edits for marked files.
Each is a name and a function that returns the operation.")

(defun vellumory-dired--files ()
  "Return the marked files that are regular files.
Signal a user error when there are none."
  (or (seq-filter #'file-regular-p (dired-get-marked-files))
      (user-error "No documents marked")))

(defun vellumory-batch--show (title columns rows rerun)
  "Display ROWS (lists of FILE and column strings) under TITLE.
COLUMNS are the `tabulated-list-format' entries after the Document
column; RERUN is a function that runs the batch again.  Return the
buffer."
  (let ((buffer (get-buffer-create "*Vellumory batch*")))
    (with-current-buffer buffer
      (vellumory-batch-mode)
      (setq tabulated-list-format (vconcat (cons '("Document" 28 t) columns))
            tabulated-list-entries (mapcar (lambda (row) (list (car row) (vconcat (cons (file-name-nondirectory (car row))
                                                                                          (cdr row)))))
                                           rows)
            vellumory-batch--rerun rerun
            header-line-format nil)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (setq-local mode-line-process (format " %s" title)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun vellumory-dired-validate (flavour &optional files)
  "Validate the marked FILES against FLAVOUR in one veraPDF run."
  (interactive (progn (require 'vellumory)
                      (list (cdr (assoc (completing-read "Validate against: " vellumory-validation-profiles nil t)
                                        vellumory-validation-profiles)))))
  (require 'vellumory)
  (let ((files (or files (vellumory-dired--files))))
    (message "Validating %d documents…" (length files))
    (vellumory--cli (append (list "batch-validate" "-f" flavour) files)
                    (lambda (result)
                      (let ((buffer (vellumory-batch--show
                                     "validation"
                                     '(("Profile" 26 t) ("Result" 14 t) ("Failed rules" 12 t))
                                     (mapcar (lambda (r)
                                               (list (alist-get 'file r)
                                                     (or (alist-get 'profile r) "")
                                                     (cond ((alist-get 'error r)
                                                            (propertize "error" 'help-echo (alist-get 'error r)
                                                                        'face 'error))
                                                           ((eq (alist-get 'compliant r) t)
                                                            (propertize "compliant" 'face 'success))
                                                           (t (propertize "not compliant" 'face 'warning)))
                                                     (format "%s" (or (alist-get 'failed_rules r) "—"))))
                                             (alist-get 'results result))
                                     (lambda () (vellumory-dired-validate flavour files)))))
                        (with-current-buffer buffer (setq vellumory-batch--flavour flavour))
                        (message "Validated %d documents" (length (alist-get 'results result))))))))

;;;###autoload
(defun vellumory-dired-edit (name &optional files operation)
  "Apply the edit NAME (see `vellumory-dired-operations') to each marked file.
Every edit is recorded in that document's own history and can be undone there.
Non-nil FILES replace the marked files, and non-nil OPERATION the
operation NAME would build; running the batch again passes both."
  (interactive (list (completing-read "Edit marked documents: " vellumory-dired-operations nil t)))
  (require 'vellumory)
  (let* ((files (or files (vellumory-dired--files)))
         (operation (or operation (funcall (cdr (assoc name vellumory-dired-operations)))))
         (json (json-serialize operation))
         (rows nil) (pending files))
    (cl-labels ((next ()
                  (if (null pending)
                      (vellumory-batch--show name '(("Result" 50 t)) (nreverse rows)
                                             (lambda () (vellumory-dired-edit name files operation)))
                    (let ((file (pop pending)))
                      (message "%s: %s (%d left)" name (file-name-nondirectory file) (length pending))
                      (vellumory-dired--edit-one file json
                                                 (lambda (row) (push row rows) (next)))))))
      (next))))

(defun vellumory-dired--edit-one (file json done)
  "Run one edit of FILE, the serialized operation JSON.
Call DONE with its result row, success or not."
  ;; Standard error apart: MuPDF warnings must not mix with the JSON result.
  (let ((output (generate-new-buffer " *vellumory-batch*"))
        (errors (generate-new-buffer " *vellumory-batch-errors*")))
    (let ((default-directory (vellumory--existing-directory)))
      (make-process
       :name "vellumory-batch" :buffer output :command (list vellumory-program "edit" file json)
       :connection-type 'pipe :noquery t :stderr errors
       :sentinel
       (lambda (process _event)
         (unless (process-live-p process)
           (let* ((ok (zerop (process-exit-status process)))
                  (text (with-current-buffer (if ok output errors) (string-trim (buffer-string))))
                  (result (ignore-errors (json-parse-string (car (last (split-string text "\n" t)))
                                                            :object-type 'alist :null-object nil
                                                            :false-object nil))))
             (kill-buffer output)
             (when (buffer-live-p errors) (kill-buffer errors))
             (funcall done
                      (list file
                            (cond ((not (zerop (process-exit-status process)))
                                   (propertize (or (alist-get 'error result) text) 'face 'error))
                                  ((alist-get 'changed result)
                                   (let ((node (alist-get 'node result)))
                                     (format "state %s: %s" (alist-get 'index node) (alist-get 'label node))))
                                  (t "unchanged")))))))))))

;;;###autoload
(defun vellumory-dired-info (&optional files)
  "Summarize format, claimed standards, tagging and language of marked FILES."
  (interactive)
  (require 'vellumory)
  (require 'vellumory-details)
  (let* ((files (or files (vellumory-dired--files))) (rows nil) (pending files))
    (cl-labels ((next ()
                  (if (null pending)
                      (vellumory-batch--show "summary"
                                             '(("Pages" 6 t) ("Format" 9 t) ("Claims" 14 t) ("Tagged" 7 t)
                                               ("Language" 9 t) ("Size" 10 t))
                                             (nreverse rows) (lambda () (vellumory-dired-info files)))
                    (let ((file (pop pending)))
                      (vellumory--cli (list "info" file)
                                      (lambda (info)
                                        (push (list file (format "%s" (alist-get 'pages info))
                                                    (or (alist-get 'format info) "")
                                                    (string-join (vellumory--standards info) ", ")
                                                    (if (eq (alist-get 'tagged info) t) "yes" "no")
                                                    (or (alist-get 'language info) "")
                                                    (vellumory--size-string (alist-get 'bytes info)))
                                              rows)
                                        (next)))))))
      (next))))

;;;###autoload
(defun vellumory-dired-merge (output &optional files)
  "Merge the marked FILES, in Dired order, into the new document OUTPUT."
  (interactive (list (read-file-name "Merge into: " nil nil nil "merged.pdf")))
  (require 'vellumory)
  (let ((files (or files (vellumory-dired--files))) (output (expand-file-name output)))
    (when (member output files) (user-error "The output must be a new file"))
    (vellumory--cli (append (list "merge" output) files)
                    (lambda (_)
                      (message "Merged %d documents into %s" (length files) (abbreviate-file-name output))
                      (when (derived-mode-p 'dired-mode) (revert-buffer))
                      (display-buffer (vellumory-open output t))))))

(defun vellumory-batch--file () "Return the document on this line." (or (tabulated-list-get-id) (user-error "No document on this line")))

(defun vellumory-batch-visit ()
  "Open the document on this line."
  (interactive)
  (pop-to-buffer (vellumory-open (vellumory-batch--file))))

(defun vellumory-batch-report ()
  "Show the full validation report of the document on this line."
  (interactive)
  (let ((flavour (or vellumory-batch--flavour "0")))
    (with-current-buffer (vellumory-open (vellumory-batch--file) t)
      (vellumory-validate flavour))))

(defun vellumory-batch-rerun ()
  "Run this batch again."
  (interactive)
  (if vellumory-batch--rerun (funcall vellumory-batch--rerun) (user-error "Nothing to run again")))

(defvar vellumory-batch-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'vellumory-batch-visit)
    (keymap-set map "v" #'vellumory-batch-report)
    (keymap-set map "g" #'vellumory-batch-rerun)
    map))

(define-derived-mode vellumory-batch-mode tabulated-list-mode "PDF Batch"
  "Results of a batch over documents.
\\{vellumory-batch-mode-map}")

(defvar vellumory-dired-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "v" #'vellumory-dired-validate)
    (keymap-set map "e" #'vellumory-dired-edit)
    (keymap-set map "i" #'vellumory-dired-info)
    (keymap-set map "m" #'vellumory-dired-merge)
    (keymap-set map "o" #'vellumory-occur-documents)
    (keymap-set map "V" #'vellumory-virtual-create)
    map)
  "Batch commands on marked documents, under `;' in Dired.")
(fset 'vellumory-dired-map vellumory-dired-map)

(provide 'vellumory-dired)
;;; vellumory-dired.el ends here
