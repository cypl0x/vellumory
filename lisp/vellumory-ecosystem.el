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
;; `vellumory-consult-heading' (i, SPC m . in Doom) is consult-org-heading
;; for the document's contents: entries by their path of titles, narrowed by
;; level, each one's place shown while moving through them.  It also stands
;; in for consult-outline, consult-imenu and consult-org-heading.
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
(defvar vellumory--outline)
(defvar consult--narrow)
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
(declare-function consult--tofu-encode "consult" (n))
(declare-function consult--annotate-align "consult" (cand ann))
(declare-function consult--lookup-prop "consult" (prop selected candidates &rest _))
(declare-function vellumory--navigation-ready "vellumory-navigation" ())
(declare-function vellumory--outline-title "vellumory-navigation" (entry))
(declare-function vellumory--follow-outline "vellumory-navigation" (source hash entry))
(declare-function vellumory-goto-heading "vellumory-navigation" ())
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

;;;; Contents of the document

(defvar vellumory--heading-history nil "Input history of `vellumory-consult-heading'.")

(defconst vellumory--heading-faces
  [outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7 outline-8]
  "Faces of the titles of a heading's path, by level, as Org's for its headings.")

(defun vellumory--heading-candidates ()
  "Candidates for the contents entries, as `consult-org-heading' shows headings.
Each is its entry's path of titles, every title in its level's face, made
unique by a hidden suffix.  The entry, its level and its page's label ride
on the first character: the labels are this buffer's, and annotations are
made in the minibuffer."
  (let (path candidates (index 0))
    (cl-loop for entry across vellumory--outline do
      (let* ((depth (alist-get 'depth entry))
             (page (alist-get 'page entry))
             (uri (alist-get 'uri entry))
             (where (cond ((natnump page) (vellumory--page-description page))
                          ((and (stringp uri) (not (string-empty-p uri))) uri)
                          (t ""))))
        (setq path (append (take depth path)
                           (list (propertize (vellumory--outline-title entry) 'face
                                             (aref vellumory--heading-faces (% depth 8))))))
        (let ((candidate (concat (string-join path "/") (consult--tofu-encode index))))
          (add-text-properties 0 1 `(vellumory-heading ,entry vellumory-heading-level ,(1+ depth)
                                     vellumory-heading-where ,where)
                               candidate)
          (push candidate candidates))
        (cl-incf index)))
    (nreverse candidates)))

(defun vellumory--heading-annotation (candidate)
  "The page label of CANDIDATE's entry, or its link, aligned after the candidates."
  (consult--annotate-align
   candidate (propertize (get-text-property 0 'vellumory-heading-where candidate)
                         'face 'completions-annotations)))

(defun vellumory--heading-preview (source origin)
  "A consult state function showing in SOURCE the place of the entry previewed.
Leaving without a choice goes back to ORIGIN; choosing an entry follows it."
  (let ((hash (buffer-local-value 'vellumory--hash source)))
    (lambda (action entry)
      (with-current-buffer source
        (pcase action
          ('preview (let ((page (alist-get 'page entry)))
                      (vellumory--restore-position
                       (if (natnump page) `((page . ,page) (y . ,(max 0 (alist-get 'y entry)))) origin))
                      (vellumory--refresh t)))
          ('exit (vellumory--restore-position origin) (vellumory--refresh t))
          ('return (when entry (vellumory--follow-outline source hash entry))))))))

;;;###autoload
(defun vellumory-consult-heading (&optional level)
  "Go to an entry of the document's contents, showing each one's place.
As `consult-org-heading' in Org: every entry by its path of titles, its
page label beside it; 1 to 9 and a space narrow to that many levels, and a
numeric prefix LEVEL starts so narrowed.  Without consult it is
`vellumory-goto-heading'."
  (interactive (list (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (unless (derived-mode-p 'vellumory-mode) (user-error "Not a document"))
  (if (not (require 'consult nil t))
      (vellumory-goto-heading)
    (vellumory--navigation-ready)
    (consult--read
     (or (vellumory--heading-candidates) (user-error "This PDF has no document outline"))
     :prompt "Go to heading: " :category 'vellumory-heading :sort nil :require-match t
     :history '(:input vellumory--heading-history)
     :narrow (list :predicate (lambda (candidate)
                                (<= (get-text-property 0 'vellumory-heading-level candidate)
                                    (- consult--narrow ?0)))
                   :keys (mapcar (lambda (key) (cons key (format "Level %c" key))) (number-sequence ?1 ?9)))
     :initial-narrow (and level (max ?1 (min ?9 (+ level ?0))))
     :annotate #'vellumory--heading-annotation
     :lookup (apply-partially #'consult--lookup-prop 'vellumory-heading)
     :state (vellumory--heading-preview (current-buffer) (vellumory--reading-position)))))

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
