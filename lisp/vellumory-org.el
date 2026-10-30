;;; vellumory-org.el --- Org links to exact places in documents -*- lexical-binding: t; -*-

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
;; `vellumory:FILE::PAGE::Y' links to page PAGE (one-based) at Y points from
;; the page top.  `org-store-link' in a reader stores the exact position; the
;; description is the selected text when there is a selection, otherwise the
;; nearest heading of the document outline and the page label.  Export
;; produces "#page=N" links for HTML and LaTeX.
;;
;; For notes written with org-pdftools, `pdf:FILE::PAGE++FRACTION' links open
;; here too when pdf-tools has not defined that link type.

;;; Code:
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'ol)
(require 'cl-lib)

;; Bound dynamically by the exports: without this, compiled lets would bind it lexically.
(defvar org-export-filter-final-output-functions)
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))
(defvar vellumory--outline)
(defvar vellumory--source-file)
(defvar vellumory--file)
(defvar vellumory--pages)
(defvar vellumory--pending-position)
(defvar vellumory-program)
(declare-function vellumory-open "vellumory" (file &optional no-select))
(declare-function vellumory--reading-position "vellumory" (&optional window))
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--page-description "vellumory-navigation" (page))
(declare-function vellumory-selection-text "vellumory-select" ())
(declare-function vellumory--remember-position "vellumory-select" ())

(defun vellumory-org--parse (path)
  "(FILE PAGE Y) from a link PATH; PAGE is zero-based, Y in points or nil."
  (let* ((parts (split-string path "::"))
         (file (car parts))
         (page (and (cadr parts) (string-match-p "\\`[0-9]+\\'" (cadr parts))
                    (1- (max 1 (string-to-number (cadr parts))))))
         (y (and (nth 2 parts) (string-to-number (nth 2 parts)))))
    (list file page y)))

(defun vellumory-org--visit (file page y)
  "Show FILE at zero-based PAGE, Y points from the page top; return its buffer.
A nil PAGE or Y counts as 0.  A laid-out document first remembers the
position left for `vellumory-history-back'; one still opening goes there
once it is laid out."
  (let ((buffer (vellumory-open (expand-file-name file))))
    (with-current-buffer buffer
      (let ((go (lambda ()
                  (vellumory--remember-position)
                  (vellumory--restore-position `((page . ,(or page 0)) (y . ,(or y 0))))
                  (vellumory--refresh t))))
        (if vellumory--pages (funcall go)
          ;; The document is still opening: go there once it is laid out.
          (setq-local vellumory--pending-position `((page . ,(or page 0)) (y . ,(or y 0)))))))
    buffer))

(defun vellumory-org-follow (path _arg)
  "Open the document position in a `vellumory:' link PATH."
  (apply #'vellumory-org--visit (vellumory-org--parse path)))

(defun vellumory-org--heading (page y)
  "Title of the last outline entry at or before PAGE and Y."
  (let (best)
    (when (vectorp vellumory--outline)
      (cl-loop for entry across vellumory--outline
               for p = (alist-get 'page entry)
               when (and p (or (< p page) (and (= p page) (<= (or (alist-get 'y entry) 0) (+ y 1)))))
               do (setq best (alist-get 'title entry))))
    best))

(defun vellumory-org-store ()
  "Store a link to the exact reading position."
  (when (derived-mode-p 'vellumory-mode)
    (let* ((position (vellumory--reading-position))
           (page (alist-get 'page position)) (y (alist-get 'y position))
           (file (abbreviate-file-name (or vellumory--source-file vellumory--file)))
           (selected (vellumory-selection-text))
           (heading (vellumory-org--heading page y))
           (description (cond (selected (format "“%s”" (truncate-string-to-width
                                                        (replace-regexp-in-string "\\s-+" " " selected)
                                                        120 nil nil "…")))
                              (heading (format "%s (p. %s)" heading (vellumory--page-description page)))
                              (t (format "%s, p. %s" (file-name-nondirectory file)
                                         (vellumory--page-description page))))))
      (org-link-store-props :type "vellumory"
                            :link (format "vellumory:%s::%d::%.1f" file (1+ page) y)
                            :description description))))

(defun vellumory-org-export (path description backend _info)
  "Export a `vellumory:' link PATH for BACKEND as a link to its page.
DESCRIPTION, or else PATH, is the link text.  Backends other than HTML,
Markdown and LaTeX get the text alone."
  (pcase-let ((`(,file ,page ,_) (vellumory-org--parse path)))
    (let ((target (if page (format "%s#page=%d" file (1+ page)) file))
          (text (or description path)))
      (pcase backend
        ((or 'html 'md) (format "<a href=\"%s\">%s</a>" target text))
        ('latex (format "\\href{%s}{%s}" target text))
        (_ text)))))

(defun vellumory-org-complete (&optional _arg)
  "Complete a `vellumory:' link to a document and page."
  (let ((file (read-file-name "Document: " nil nil t)))
    (format "vellumory:%s::%d" (abbreviate-file-name file) (read-number "Page: " 1))))

(org-link-set-parameters "vellumory"
                         :follow #'vellumory-org-follow
                         :store #'vellumory-org-store
                         :export #'vellumory-org-export
                         :complete #'vellumory-org-complete)

;;;; org-pdftools compatibility

(defun vellumory-org-follow-pdftools (path _arg)
  "Open an org-pdftools `pdf:FILE::PAGE++FRACTION' link PATH.
Annotation (;;) and search (??) suffixes open the page."
  (let* ((parts (split-string path "::"))
         (file (car parts))
         (place (or (cadr parts) ""))
         (page (and (string-match "\\`\\([0-9]+\\)" place) (1- (string-to-number (match-string 1 place)))))
         (fraction (and (string-match "\\+\\+\\([0-9.]+\\)" place) (string-to-number (match-string 1 place))))
         (buffer (vellumory-org--visit file page 0)))
    (when (and fraction page)
      (with-current-buffer buffer
        (when vellumory--pages
          (let ((height (alist-get 'height (aref vellumory--pages page))))
            (vellumory--restore-position `((page . ,page) (y . ,(* fraction height))))
            (vellumory--refresh t)))))))

(unless (assoc "pdf" org-link-parameters)
  (org-link-set-parameters "pdf" :follow #'vellumory-org-follow-pdftools))

;;;; Export to accessible, archival PDF

(defvar org-latex-compiler)
(defvar org-latex-default-packages-alist)
(defvar org-latex-hyperref-template)
(defvar org-export-default-language)
(declare-function org-latex-export-to-pdf "ox-latex" (&optional async subtreep visible-only body-only ext-plist))
(declare-function org-collect-keywords "org" (keywords &optional unique directory))
(declare-function org-export-define-derived-backend "ox" (child parent &rest body))
(declare-function vellumory--cli "vellumory-edit" (args callback &optional buffer input))
(declare-function vellumory-validate "vellumory-validate" (flavour))

(defcustom vellumory-org-export-standards '("ua-2" "a-4f")
  "Standards an Org export targets, as LaTeX `\\DocumentMetadata' names them.
The default is PDF 2.0 that is accessible (PDF/UA-2, tagged, with MathML for
formulas under LuaLaTeX) and archival (PDF/A-4f, which also allows embedded
files).  Every export is validated against each with veraPDF."
  :type '(repeat string) :group 'vellumory)

(defcustom vellumory-org-export-embed-source t
  "Whether an export carries its Org text inside the PDF.
It becomes the document's associated file with the relationship Source, which
PDF/A-3, PDF/A-4f and PDF/A-4e allow; other PDF/A targets leave it out.  The
attachment is an edit like any other: it is in the history and can be undone."
  :type 'boolean :group 'vellumory)

(defvar vellumory-org-export-results nil
  "Results of the latest export: file and (STANDARD . COMPLIANT) pairs.")

(defun vellumory-org--source-allowed-p (standards)
  "Whether every PDF/A part among STANDARDS allows a non-PDF/A attachment."
  (seq-every-p (lambda (standard)
                 (or (not (string-prefix-p "a-" standard))
                     (string-match-p "\\`a-\\(?:3\\|4[ef]\\)" standard)))
               standards))

(defun vellumory-org--source ()
  "The Org text to embed and its name: the whole buffer, saved or not."
  (cons (file-name-nondirectory (or buffer-file-name (concat (buffer-name) ".org")))
        (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))

(defun vellumory-org--embed-source (pdf source standards)
  "Attach SOURCE, a (NAME . TEXT) pair, to PDF as its Source, if STANDARDS allow."
  (when (and vellumory-org-export-embed-source (vellumory-org--source-allowed-p standards))
    (let* ((directory (make-temp-file "vellumory-source-" t))
           (file (expand-file-name (car source) directory)))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (cdr source) nil file nil 'silent))
            (with-temp-buffer
              (unless (zerop (let ((default-directory (vellumory--existing-directory)))
                               (call-process vellumory-program nil t nil "edit" pdf
                                             (json-serialize `((op . "attach") (file . ,file) (key . ,(car source))
                                                               (description . "Org source of this document")
                                                               (relationship . "Source"))))))
                (error "Embedding the Org source failed: %s" (string-trim (buffer-string))))))
        (delete-directory directory t)))))

(defun vellumory-org--flavour (standard)
  "Return the veraPDF profile name for a `\\DocumentMetadata' STANDARD.
For example, a-4f becomes 4f and ua-2 becomes ua2."
  (replace-regexp-in-string "\\`a-" "" (replace-regexp-in-string "\\`ua-" "ua" standard)))

(defun vellumory-org--document-metadata (language)
  "Return the LaTeX `\\DocumentMetadata' line for LANGUAGE.
It turns tagging on and names each of `vellumory-org-export-standards'."
  (format "\\DocumentMetadata{lang=%s, %s, tagging=on}" language
          (mapconcat (lambda (standard) (concat "pdfstandard=" standard)) vellumory-org-export-standards ", ")))

(defun vellumory-org--validate-export (pdf reader)
  "Check PDF against every target standard; show the report of any it misses.
READER is the buffer showing PDF; the checks and reports run from it.  The
results go to `vellumory-org-export-results'."
  (setq vellumory-org-export-results (list pdf))
  (dolist (standard vellumory-org-export-standards)
    (vellumory--cli (list "validate" "-f" (vellumory-org--flavour standard) pdf)
                    (lambda (report)
                      (let ((compliant (eq (alist-get 'compliant report) t)))
                        (setq vellumory-org-export-results
                              (append vellumory-org-export-results (list (cons standard compliant))))
                        (message "%s: %s" (file-name-nondirectory pdf)
                                 (mapconcat (lambda (r) (format "%s %s" (upcase (car r)) (if (cdr r) "✓" "✗")))
                                            (cdr vellumory-org-export-results) ", "))
                        (when (and (not compliant) (buffer-live-p reader))
                          (with-current-buffer reader (vellumory-validate (vellumory-org--flavour standard))))))
                    reader)))

;;;###autoload
(defun vellumory-org-export-pdf (&optional subtreep visible-only)
  "Export this Org buffer to an accessible, archival PDF, validate it and show it.
The LaTeX exporter runs LuaLaTeX with `\\DocumentMetadata' for the standards in
`vellumory-org-export-standards' and the document language (#+LANGUAGE); a
#+LATEX_CLASS_PRE line in the file replaces that.  The Org text travels inside
the PDF (`vellumory-org-export-embed-source').  With a prefix argument
SUBTREEP, export the subtree at point; VISIBLE-ONLY exports only the
visible text."
  (interactive "P")
  (require 'ox-latex)
  (require 'vellumory)
  (unless (executable-find "lualatex") (user-error "Export needs LuaLaTeX (TeX Live 2025 or newer)"))
  (when (version< (org-version) "9.7")
    (user-error "Export to accessible PDF needs Org 9.7 or newer (this is %s): Emacs 30, or Org from GNU ELPA"
                (org-version)))
  (let* ((language (or (cadr (assoc "LANGUAGE" (org-collect-keywords '("LANGUAGE")))) org-export-default-language))
         (language (if (string-match-p "\\`[A-Za-z]\\{1,8\\}\\(?:-[A-Za-z0-9]\\{1,8\\}\\)*\\'" language) language
                     (user-error "#+LANGUAGE must be a language tag such as en-GB, not %s" language)))
         (org-latex-compiler "lualatex")
         ;; Org's %L is the capitalized babel name ("English"); /Lang must be a
         ;; language tag, as \\DocumentMetadata already sets it.
         (org-latex-hyperref-template
          (replace-regexp-in-string "pdflang={%L}" (concat "pdflang={" language "}")
                                    org-latex-hyperref-template t t))
         ;; Org loads rotating for sideways floats only; some TeX Live builds lack it.
         (org-latex-default-packages-alist
          (if (and (executable-find "kpsewhich") (car (let ((default-directory (vellumory--existing-directory)))
                                                        (process-lines-ignore-status "kpsewhich" "rotating.sty"))))
              org-latex-default-packages-alist
            (seq-remove (lambda (entry) (equal (nth 1 entry) "rotating")) org-latex-default-packages-alist)))
         (metadata (vellumory-org--document-metadata language))
         ;; Org 9.8 places the line by :latex-class-pre; before it there is no
         ;; such option, and this puts the line first (else a no-op).
         (org-export-filter-final-output-functions
          (cons (lambda (output backend _info)
                  (if (and (org-export-derived-backend-p backend 'latex)
                           (not (string-match-p "\\\\DocumentMetadata" output)))
                      (concat metadata "\n" output)
                    output))
                org-export-filter-final-output-functions))
         (source (vellumory-org--source))
         (pdf (expand-file-name
               (org-latex-export-to-pdf nil subtreep visible-only nil
                                        `(:latex-class-pre ,metadata))))
         (reader (progn (vellumory-org--embed-source pdf source vellumory-org-export-standards)
                        (vellumory-open pdf t))))
    (pop-to-buffer reader)
    (vellumory-org--validate-export pdf reader)
    pdf))

;;;; The Typst route

(declare-function org-typst-export-to-pdf "ox-typst" (&optional async subtreep visible-only body-only ext-plist))
(defvar org-typst-process)
(defcustom vellumory-org-typst-standards '("a-3a" "ua-1")
  "Standards a Typst export targets, as `typst compile --pdf-standard' names them.
PDF/A-3a (archival and tagged, with the Org source inside) and PDF/UA-1
\(accessible): Typst's PDF/UA-1 cannot be combined with PDF/A-4.  a-2a instead
of a-3a forbids the embedded source.  Every export is validated against each
with veraPDF."
  :type '(repeat string) :group 'vellumory)

(defun vellumory-org--typst-language (output backend _info)
  "Split the language tag in OUTPUT of the typst BACKEND into its parts.
Typst takes a language and a region apart: `en-GB' is lang en, region GB.
Other OUTPUT is returned unchanged."
  (if (and (eq backend 'typst) (stringp output))
      (replace-regexp-in-string
       "#set text(lang: \"\\([A-Za-z]\\{2,3\\}\\)-\\([A-Za-z]\\{2\\}\\|[0-9]\\{3\\}\\)\")"
       "#set text(lang: \"\\1\", region: \"\\2\")" output t)
    output))

;;;###autoload
(defun vellumory-org-export-typst (&optional subtreep visible-only)
  "Export with Typst to PDF/A-3a and PDF/UA-1, then validate and show the PDF.
The standards are `vellumory-org-typst-standards'; the document language comes
from #+LANGUAGE, and the Org text travels inside the PDF
\(`vellumory-org-export-embed-source').  With a prefix argument SUBTREEP,
export the subtree at point; VISIBLE-ONLY exports only the visible text."
  (interactive "P")
  (unless (require 'ox-typst nil t) (user-error "Export needs the ox-typst package"))
  (require 'vellumory)
  (unless (executable-find "typst") (user-error "Export needs Typst 0.14 or newer"))
  (let* ((org-typst-process (format "typst compile --pdf-standard %s \"%%s\""
                                    (string-join vellumory-org-typst-standards ",")))
         (org-export-filter-final-output-functions
          (cons #'vellumory-org--typst-language org-export-filter-final-output-functions))
         (source (vellumory-org--source))
         (pdf (expand-file-name (org-typst-export-to-pdf nil subtreep visible-only)))
         (reader (progn (vellumory-org--embed-source pdf source vellumory-org-typst-standards)
                        (vellumory-open pdf t))))
    (pop-to-buffer reader)
    (let ((vellumory-org-export-standards vellumory-org-typst-standards))
      (vellumory-org--validate-export pdf reader))
    pdf))

(with-eval-after-load 'ox-typst
  (org-export-define-derived-backend 'vellumory-typst 'typst
    :menu-entry '(?y 1 ((?v "As PDF/A-3a and PDF/UA-1 (Vellumory, validated)"
                            (lambda (_async subtreep visible-only _body-only)
                              (vellumory-org-export-typst subtreep visible-only)))))))

(with-eval-after-load 'ox-latex
  (org-export-define-derived-backend 'vellumory-pdf 'latex
    :menu-entry '(?l 1 ((?v "As accessible, archival PDF (Vellumory, validated)"
                            (lambda (_async subtreep visible-only _body-only)
                              (vellumory-org-export-pdf subtreep visible-only)))))))

(provide 'vellumory-org)
;;; vellumory-org.el ends here
