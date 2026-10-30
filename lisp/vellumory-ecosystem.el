;;; vellumory-ecosystem.el --- consult, embark and project integration -*- lexical-binding: t; -*-

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
;; Every part is optional and loads only with its package.
;;
;; `vellumory-consult-line' (M-s l, SPC s s in Doom) is consult-line for a
;; document: every line of its text with its page label; moving through the
;; candidates shows their pages, choosing one shows the line highlighted.
;; Without consult it is a plain completing read.  consult-buffer gains a
;; hidden source of open documents (narrow with d SPC).
;;
;; embark (`C-.' / SPC a) acts on the selection (search it, list its
;; occurrences, copy it) or, without one, on the page in view (go to, extract,
;; rotate, delete, keep, present from it).
;;
;; `vellumory-project-documents' offers the current project's PDFs.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar vellumory--hash)
(defvar vellumory--current-page)
(defvar vellumory--file)
(declare-function vellumory--with-document-text "vellumory-search" (callback))
(declare-function vellumory--page-description "vellumory-navigation" (page))
(declare-function vellumory--page-string "vellumory" ())
(declare-function vellumory--reading-position "vellumory" (&optional window))
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--search-begin "vellumory-search" (query sensitive origin))
(declare-function vellumory-search-clear "vellumory-search" ())
(declare-function vellumory-selection-text "vellumory-select" ())
(declare-function vellumory-open "vellumory" (file &optional no-select))
(declare-function consult--read "consult" (table &rest options))
(declare-function consult--lookup-member "consult" (selected candidates &rest _))
(declare-function consult--buffer-state "consult" ())
(declare-function vellumory-present "vellumory-present" (&optional from-start))
(declare-function vellumory-page-text "vellumory" ())
(declare-function vellumory-extract-pages "vellumory-edit" (spec output))
(declare-function vellumory-goto-page "vellumory" (page))
(declare-function vellumory-occur "vellumory-search" (regexp))
(declare-function vellumory-search "vellumory-search" (&optional sensitive))
(declare-function vellumory-keep-pages "vellumory-edit" (spec))
(declare-function vellumory-delete-pages "vellumory-edit" (&optional arg))
(declare-function vellumory-rotate-pages "vellumory-edit" (&optional arg))
(declare-function vellumory-rotate-pages-counterclockwise "vellumory-edit" (&optional arg))
(declare-function project-root "project" (project))
(declare-function project-files "project" (project &optional dirs))

;;;; Lines of the document

(defun vellumory--document-lines (pages)
  "Candidates for every non-empty line of PAGES, each carrying its page."
  (let (lines)
    (dotimes (page (length pages))
      (let ((label (vellumory--page-description page)))
        (dolist (line (split-string (aref pages page) "\n" t "[ \t]+"))
          (push (concat (propertize (format "%-9s" label) 'face 'shadow)
                        (propertize line 'vellumory-line (cons page line)))
                lines))))
    (nreverse lines)))

(defun vellumory--line-of (candidate)
  "(PAGE . TEXT) of CANDIDATE."
  (get-text-property (or (next-single-property-change 0 'vellumory-line candidate) 0)
                     'vellumory-line candidate))

(defun vellumory--show-line (line)
  "Show LINE, (PAGE . TEXT), highlighted from the top of its page."
  (vellumory-search-clear)
  (vellumory--search-begin (cdr line) t `((page . ,(car line)) (y . 0))))

(defun vellumory--line-preview (source origin)
  "A consult state function showing candidates' pages in SOURCE.
Leaving without a choice goes back to ORIGIN; choosing a candidate
shows its line highlighted."
  (lambda (action candidate)
    (with-current-buffer source
      (pcase action
        ('preview (if-let* ((line (and candidate (vellumory--line-of candidate))))
                      (progn (vellumory--restore-position `((page . ,(car line)) (y . 0)))
                             (vellumory--refresh t))
                    (vellumory--restore-position origin)))
        ('exit (vellumory--restore-position origin) (vellumory--refresh t))
        ('return (when-let* ((line (and candidate (vellumory--line-of candidate))))
                   (vellumory--show-line line)))))))

(defun vellumory--document-text-now ()
  "Every page's text, waiting for the extraction the first time (C-g stops)."
  (let (pages)
    (vellumory--with-document-text (lambda (text) (setq pages text)))
    (unless pages (message "Extracting the document's text…"))
    (while (not pages) (accept-process-output nil 0.05))
    pages))

;;;###autoload
(defun vellumory-consult-line ()
  "Go to a line of the document's text, choosing among all of them."
  (interactive)
  (unless (derived-mode-p 'vellumory-mode) (user-error "Not a document"))
  (let ((lines (vellumory--document-lines (vellumory--document-text-now)))
        (prompt (format "Line of %s: " (buffer-name))))
    (when (null lines) (user-error "No text in this document (OCR adds one: e o)"))
    (if (require 'consult nil t)
        (consult--read lines :prompt prompt :category 'vellumory-line :sort nil
                       :require-match t :lookup #'consult--lookup-member
                       :state (vellumory--line-preview (current-buffer) (vellumory--reading-position)))
      (let ((chosen (completing-read prompt lines nil t)))
        (when-let* ((line (vellumory--line-of (or (car (member chosen lines)) chosen))))
          (vellumory--show-line line))))))

;;;; consult-buffer: open documents

(defun vellumory--document-buffers ()
  "Names of the buffers showing documents."
  (mapcar #'buffer-name
          (seq-filter (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'vellumory-mode)))
                      (buffer-list))))

(defun vellumory--document-annotation (name)
  "The page in view of the document in buffer NAME, as a candidate annotation."
  (when-let* ((buffer (get-buffer name)))
    (with-current-buffer buffer
      (format "  page %s" (vellumory--page-string)))))

(defvar vellumory-consult-source-documents
  (list :name "Documents" :narrow ?d :hidden t :category 'buffer :face 'consult-buffer
        :history 'buffer-name-history :items #'vellumory--document-buffers
        :annotate #'vellumory--document-annotation
        :state (lambda () (consult--buffer-state)))
  "Open documents for `consult-buffer', shown when narrowed with d.")

(with-eval-after-load 'consult
  (defvar consult-buffer-sources)
  (add-to-list 'consult-buffer-sources 'vellumory-consult-source-documents 'append))

;;;; embark: the selection or the page in view

(defun vellumory-embark-target ()
  "In a document: its selected text, or else the page in view."
  (when (derived-mode-p 'vellumory-mode)
    (if-let* ((text (vellumory-selection-text)))
        (list 'vellumory-text text)
      (list 'vellumory-page (number-to-string (1+ (or vellumory--current-page 0)))))))

(with-eval-after-load 'embark
  (defvar embark-general-map)
  (defvar embark-target-finders)
  (defvar embark-keymap-alist)
  (defvar-keymap vellumory-embark-text-map
    :doc "Actions on text selected in a document."
    :parent embark-general-map
    "s" #'vellumory-search
    "o" #'vellumory-occur)
  (defvar-keymap vellumory-embark-page-map
    :doc "Actions on the page in view."
    :parent embark-general-map
    "RET" #'vellumory-goto-page
    "x" #'vellumory-extract-pages
    "r" #'vellumory-rotate-pages
    "R" #'vellumory-rotate-pages-counterclockwise
    "d" #'vellumory-delete-pages
    "k" #'vellumory-keep-pages
    "t" #'vellumory-page-text
    "p" #'vellumory-present)
  ;; First: the other finders read the buffer's placeholder text.
  (add-to-list 'embark-target-finders #'vellumory-embark-target)
  (add-to-list 'embark-keymap-alist '(vellumory-text vellumory-embark-text-map))
  (add-to-list 'embark-keymap-alist '(vellumory-page vellumory-embark-page-map)))

;;;; Projects

;;;###autoload
(defun vellumory-project-documents ()
  "Open one of the current project's PDFs."
  (interactive)
  (require 'project)
  (let* ((project (or (project-current) (user-error "Not in a project")))
         (root (expand-file-name (project-root project)))
         (files (seq-filter (lambda (file) (string-match-p "\\.pdf\\'" file)) (project-files project))))
    (unless files (user-error "No PDFs in %s" (abbreviate-file-name root)))
    (vellumory-open (expand-file-name
                     (completing-read "Document: " (mapcar (lambda (file) (file-relative-name file root)) files)
                                      nil t)
                     root))))

(provide 'vellumory-ecosystem)
;;; vellumory-ecosystem.el ends here
