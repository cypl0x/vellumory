;;; tools/vellumory/doctor.el -*- lexical-binding: t; -*-

;; `doom doctor' reports what M-x vellumory-doctor checks: the same checks,
;; errors as errors and warnings as warnings.
(when (modulep! :tools pdf)
  (warn! ":tools pdf is also enabled; Vellumory takes precedence for PDF files."))
(if (not (require 'vellumory-doctor nil t))
    (error! "The vellumory package is not installed (doom sync installs it).")
  (pcase-dolist (`(,heading . ,checks) (vellumory-doctor--sections))
    (dolist (check checks)
      (let ((text (format "%s: %s — %s" heading (plist-get check :title) (plist-get check :detail))))
        (pcase (plist-get check :status)
          ('error (error! "%s" text))
          ('warn (warn! "%s" text)))))))
(when (and (executable-find "kpsewhich")
           (not (car (process-lines-ignore-status "kpsewhich" "pdfmanagement-testphase.sty"))))
  (warn! "TeX Live lacks the LaTeX tagging code (pdfmanagement-testphase, tagpdf, latex-lab): Org export cannot produce tagged PDF."))
