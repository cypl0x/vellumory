;;; vellumory-details.el --- Document properties, standards and attachments -*- lexical-binding: t; -*-

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
;; `vellumory-details' shows what a document is made of: format and version,
;; security, standards it claims (PDF/A, PDF/UA from XMP), tagging, language,
;; metadata, fonts (embedded or not, Unicode mapping), images, annotations,
;; forms, JavaScript, output intents, the current page's boxes and resources,
;; and embedded files, which open with RET.  Problems are highlighted.
;; Claims are reported as claims: validation is `vellumory-validate'.

;;; Code:
(require 'cl-lib)
(require 'button)

(defvar vellumory--hash)
(defvar vellumory--current-page)
(defvar vellumory--file)
(defvar vellumory--source-file)
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory-open "vellumory" (file &optional no-select))
(declare-function vellumory--page-description "vellumory-navigation" (page))
(declare-function vellumory-attach-file "vellumory-edit" (file key description &optional relationship))
(declare-function vellumory-remove-attachment "vellumory-edit" (key))

(defface vellumory-details-label '((t :inherit font-lock-keyword-face)) "Field names." :group 'vellumory)
(defface vellumory-details-good '((t :inherit success)) "Good properties." :group 'vellumory)
(defface vellumory-details-warning '((t :inherit warning)) "Properties worth attention." :group 'vellumory)
(defface vellumory-details-heading '((t :inherit bold :height 1.1)) "Section headings." :group 'vellumory)

(defun vellumory--pdf-date (date)
  "Return a readable form of DATE, a PDF or XMP date string.
PDF dates look like D:20080918111951-07\\='00\\=', XMP (ISO 8601) dates like
2008-09-18T11:19:51-07:00.  Any other DATE is returned unchanged."
  (if (and date (string-match (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
                                      "\\(?:T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\(?::\\([0-9]\\{2\\}\\)\\)?[.0-9]*"
                                      "\\(Z\\|\\([-+]\\)\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)?")
                              date))
      (let ((part (lambda (n default) (or (match-string n date) default))))
        (format "%s-%s-%s %s:%s:%s%s" (match-string 1 date) (match-string 2 date) (match-string 3 date)
                (funcall part 4 "00") (funcall part 5 "00") (funcall part 6 "00")
                (cond ((equal (match-string 7 date) "Z") " UTC")
                      ((match-string 8 date) (format " %s%s:%s" (if (equal (match-string 8 date) "-") "−" "+")
                                                     (match-string 9 date) (match-string 10 date)))
                      (t ""))))
  (if (and date (string-match "\\`D?:?\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)?\\([0-9]\\{2\\}\\)?\\([0-9]\\{2\\}\\)?\\([0-9]\\{2\\}\\)?\\([0-9]\\{2\\}\\)?\\([Z+-]\\)?\\([0-9]\\{2\\}\\)?'?\\([0-9]\\{2\\}\\)?" date))
      (let ((part (lambda (n default) (or (match-string n date) default))))
        (format "%s-%s-%s %s:%s:%s%s" (funcall part 1 "") (funcall part 2 "01") (funcall part 3 "01")
                (funcall part 4 "00") (funcall part 5 "00") (funcall part 6 "00")
                (pcase (match-string 7 date)
                  ("Z" " UTC")
                  ((and sign (or "+" "-")) (format " %s%s:%s" (if (equal sign "-") "−" "+")
                                                   (funcall part 8 "00") (funcall part 9 "00")))
                  (_ ""))))
    date)))

(defun vellumory--xmp-property (xmp name)
  "Value of XMP property NAME (e.g. \"pdfaid:part\"), as attribute or element."
  (when (and xmp (or (string-match (concat (regexp-quote name) "=[\"']\\([^\"']*\\)[\"']") xmp)
                     (string-match (concat "<" (regexp-quote name) ">\\([^<]*\\)<") xmp)))
    (string-trim (match-string 1 xmp))))

(defun vellumory--standards (info)
  "Return the standards the XMP metadata in INFO claims, as strings."
  (let* ((xmp (alist-get 'xmp info))
         (a-part (vellumory--xmp-property xmp "pdfaid:part"))
         (a-level (vellumory--xmp-property xmp "pdfaid:conformance"))
         (ua (vellumory--xmp-property xmp "pdfuaid:part"))
         (x (vellumory--xmp-property xmp "pdfxid:GTS_PDFXVersion")))
    (delq nil (list (and a-part (format "PDF/A-%s%s" a-part (downcase (or a-level ""))))
                    (and ua (format "PDF/UA-%s" ua))
                    (and x x)))))

(defun vellumory--size-string (bytes)
  "Return BYTES as a human-readable size, or \"?\" when it is not a number."
  (if (numberp bytes) (file-size-human-readable bytes 'iec " ") "?"))

(defun vellumory--info-line (label value &optional face field raw)
  "Insert a report line: LABEL, then VALUE in FACE.
A nil or empty VALUE shows as a dash.  With FIELD, a metadata field
name, the value is a button that edits that field in place, starting
from RAW (else VALUE)."
  (insert (propertize (format "  %-15s" label) 'face 'vellumory-details-label))
  (let ((shown (if (or (null value) (equal value "")) "—" (format "%s" value))))
    (if (not field)
        (insert (propertize shown 'face face))
      (insert-text-button shown 'face (or face 'default) 'mouse-face 'highlight 'follow-link t
                          'help-echo (format "Edit the %s (RET or click; recorded, undoable)" (downcase label))
                          'action (let ((current (or raw value)))
                                    (lambda (_) (vellumory--details-edit-field field label current))))))
  (insert "\n"))

(defvar vellumory--info-source)
(declare-function vellumory--edit "vellumory-edit" (operation))

(defun vellumory--details-edit-field (field label value)
  "Set metadata FIELD, shown as LABEL, after editing VALUE in the minibuffer.
The change is a recorded, reversible edit of the document."
  (let ((new (read-string (format "%s: " label) (if (stringp value) value ""))))
    (unless (equal new (or value ""))
      (with-current-buffer vellumory--info-source
        (vellumory--edit `((op . "metadata") (set . ((,field . ,new)))))))))

(defun vellumory--details-refresh-buffers ()
  "Refresh the details shown for this document, after an edit."
  (let ((source (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and (eq (buffer-local-value 'major-mode buffer) 'vellumory-details-mode)
                 (eq (buffer-local-value 'vellumory--info-source buffer) source))
        (with-current-buffer buffer (vellumory-details-refresh))))))

(defun vellumory--info-source-note (field value info)
  "VALUE, noted as coming from XMP when the Info dictionary lacks FIELD."
  (if (and value (seq-contains-p (alist-get 'from_xmp info) (symbol-name field)))
      (concat value (propertize " (XMP)" 'face 'shadow))
    value))

(defun vellumory--signer-name (subject)
  "The common name in an X.509 SUBJECT such as /CN=A. Person/O=Org, or SUBJECT."
  (if (and subject (string-match "CN=\\([^/]+\\)" subject)) (match-string 1 subject) subject))

(defun vellumory--info-signatures (signatures)
  "Insert a section on SIGNATURES, unless there are none.
For each signature: who signed, whether the signed bytes are intact,
whether the signer is trusted, and what the signature covers."
  (when (> (length signatures) 0)
    (vellumory--info-heading (format "Signatures (%d)" (length signatures)))
    (seq-doseq (s signatures)
      (let ((field (or (alist-get 'field s) "Signature")))
        (if (not (eq (alist-get 'signed s) t))
            (vellumory--info-line field "empty signature field" 'shadow)
          (vellumory--info-line field (or (vellumory--signer-name (alist-get 'signer s)) (alist-get 'name s)))
          (vellumory--info-line "  Integrity"
                                (if (eq (alist-get 'intact s) t) "intact: the signed bytes are unchanged"
                                  (format "broken: %s" (alist-get 'error s)))
                                (if (eq (alist-get 'intact s) t) 'vellumory-details-good 'vellumory-details-warning))
          (vellumory--info-line "  Signer"
                                (cond ((eq (alist-get 'trusted s) t) "trusted (chains to the system's certificates)")
                                      ((not (eq (alist-get 'intact s) t)) "not checked: the signature does not match")
                                      (t (format "not trusted: %s" (alist-get 'trust_error s))))
                                (if (eq (alist-get 'trusted s) t) 'vellumory-details-good 'vellumory-details-warning))
          (vellumory--info-line "  Covers"
                                (cond ((eq (alist-get 'covers_whole_file s) t) "the whole file")
                                      ((eq (alist-get 'changed_after_signing s) t)
                                       "the file as signed; it was changed afterwards")
                                      (t "part of the file")))
          (vellumory--info-line "  Signed" (vellumory--pdf-date (alist-get 'time s)))
          (dolist (key '((reason . "  Reason") (location . "  Location")))
            (let ((value (alist-get (car key) s)))
              (when (and value (not (string-empty-p value)))
                (vellumory--info-line (cdr key) value)))))))))

(defun vellumory--info-heading (text)
  "Insert TEXT as a section heading."
  (insert "\n" (propertize text 'face 'vellumory-details-heading) "\n"))

(defun vellumory--permissions (info)
  "Return a description of the permissions INFO lists as denied."
  (let* ((perms (alist-get 'permissions info))
         (denied (cl-loop for (key . value) in perms unless value
                          collect (replace-regexp-in-string "_" " " (symbol-name key)))))
    (if denied (format "not permitted: %s" (string-join denied ", ")) "everything permitted")))

(defun vellumory--info-render (source info page)
  "Fill this buffer with the report INFO on the document in buffer SOURCE.
PAGE, when non-nil, is the report on the current page."
  (let* ((inhibit-read-only t)
         (fonts (alist-get 'fonts info))
         (missing (cl-remove-if (lambda (f) (alist-get 'embedded f)) fonts))
         (standards (vellumory--standards info))
         (encrypted (not (member (alist-get 'encryption info) '(nil "None"))))
         (tagged (alist-get 'tagged info)))
    (erase-buffer)
    (insert (propertize (let ((file (or (buffer-file-name source) (buffer-local-value 'vellumory--source-file source)
                                        (buffer-local-value 'vellumory--file source))))
                          (if file (abbreviate-file-name file) (buffer-name source)))
                        'face 'bold)
            "\n")
    (vellumory--info-heading "Document")
    (vellumory--info-line "Format" (format "%s · %d pages · %s · %s objects · %s revision%s"
                                           (alist-get 'format info) (alist-get 'pages info)
                                           (vellumory--size-string (alist-get 'bytes info))
                                           (or (alist-get 'objects info) "?")
                                           (or (alist-get 'revisions info) 1)
                                           (if (eql (alist-get 'revisions info) 1) "" "s")))
    (vellumory--info-line "Encryption" (if encrypted (format "%s — %s" (alist-get 'encryption info)
                                                            (vellumory--permissions info))
                                         "none")
                          (when encrypted 'vellumory-details-warning))
    (vellumory--info-line "Claims" (if standards (string-join standards ", ") "no PDF/A or PDF/UA claim")
                          (if standards 'vellumory-details-good 'shadow))
    (vellumory--info-line "Tagged" (cond ((and tagged (alist-get 'structure_tree info)) "yes, with a structure tree")
                                         (tagged "marked, but no structure tree")
                                         (t "no (inaccessible to screen readers)"))
                          (if tagged 'vellumory-details-good 'vellumory-details-warning))
    (let ((language (alist-get 'language info)))
      (vellumory--info-line "Language"
                            (cond ((member language '(nil "")) nil)
                                  ((string-match-p "\\`[A-Za-z]\\{1,8\\}\\(?:-[A-Za-z0-9]\\{1,8\\}\\)*\\'" language) language)
                                  (t (format "%s (not a language tag such as en-GB)" language)))
                            (unless (and language (string-match-p "\\`[A-Za-z]\\{1,8\\}\\(?:-[A-Za-z0-9]\\{1,8\\}\\)*\\'" language))
                              'vellumory-details-warning)
                            (quote language) language))
    (vellumory--info-line "Linearized" (if (alist-get 'linearized info) "yes (fast web view)" "no"))
    (when (alist-get 'repaired info)
      (vellumory--info-line "Repaired" "MuPDF had to repair the file structure" 'vellumory-details-warning))
    (vellumory--info-heading "Metadata")
    (insert (propertize (substitute-command-keys
                         "  Click a value, or press RET on it, to change it; \\<vellumory-details-mode-map>\\[vellumory-details-edit-metadata] edits several at once.\n")
                        'face 'shadow))
    (dolist (field '((title . "Title") (author . "Author") (subject . "Subject") (keywords . "Keywords")
                     (creator . "Creator") (producer . "Producer")))
      (vellumory--info-line (cdr field) (vellumory--info-source-note (car field) (alist-get (car field) info) info)
                            nil (car field) (alist-get (car field) info)))
    (vellumory--info-line "Created" (vellumory--info-source-note 'created (vellumory--pdf-date (alist-get 'created info)) info))
    (vellumory--info-line "Modified" (vellumory--info-source-note 'modified (vellumory--pdf-date (alist-get 'modified info)) info))
    (vellumory--info-line "XMP" (if (alist-get 'xmp info)
                                    (format "%s packet" (vellumory--size-string (length (alist-get 'xmp info))))
                                  "none"))
    (vellumory--info-heading "Content")
    (vellumory--info-line "Fonts" (format "%d · %d embedded · %d with Unicode maps"
                                          (or (alist-get 'font_count info) 0)
                                          (or (alist-get 'fonts_embedded info) 0)
                                          (or (alist-get 'fonts_with_unicode info) 0))
                          (when missing 'vellumory-details-warning))
    (seq-doseq (font missing)
      (insert (propertize (format "                 not embedded: %s (%s)\n"
                                  (alist-get 'name font) (alist-get 'type font))
                          'face 'vellumory-details-warning)))
    (vellumory--info-line "Images" (alist-get 'images info))
    (vellumory--info-line "Annotations" (format "%d · %d links · %d form widgets · %d markup"
                                                (or (alist-get 'annotations info) 0) (or (alist-get 'links info) 0)
                                                (or (alist-get 'widgets info) 0)
                                                (or (alist-get 'markup_annotations info) 0)))
    (vellumory--info-line "Forms" (cond ((alist-get 'xfa info) "XFA (dynamic forms; not rendered by MuPDF)")
                                        ((> (or (alist-get 'form_fields info) 0) 0)
                                         (format "%d fields" (alist-get 'form_fields info)))
                                        (t "none"))
                          (when (alist-get 'xfa info) 'vellumory-details-warning))
    (vellumory--info-line "JavaScript" (if (alist-get 'javascript info) "present (never executed here)" "none")
                          (when (alist-get 'javascript info) 'vellumory-details-warning))
    (vellumory--info-line "Transparency" (format "%d pages with transparency groups"
                                                 (or (alist-get 'transparency_pages info) 0)))
    (vellumory--info-line "Output intent"
                          (if (> (length (alist-get 'output_intents info)) 0)
                              (mapconcat (lambda (i) (format "%s %s%s" (alist-get 'subtype i) (alist-get 'condition i)
                                                             (if (alist-get 'profile i) " (ICC)" "")))
                                         (alist-get 'output_intents info) ", ")
                            "none"))
    (vellumory--info-line "Navigation" (format "%s outline · %s page labels · opens %s"
                                               (if (> (or (alist-get 'outline_entries info) 0) 0) "has" "no")
                                               (if (alist-get 'page_labels info) "custom" "default")
                                               (let ((mode (alist-get 'page_mode info)))
                                                 (if (equal mode "") "normally" (format "with %s" mode)))))
    (vellumory--info-signatures (alist-get 'signatures info))
    (vellumory--info-heading (format "Attachments (%d)" (length (alist-get 'attachments info))))
    (if (zerop (length (alist-get 'attachments info)))
        (insert (propertize "  none\n" 'face 'shadow))
      (cl-loop for attachment across (alist-get 'attachments info) do
               (insert "  ")
               (insert-text-button (or (alist-get 'filename attachment) "attachment")
                                   'action (let ((a attachment)) (lambda (_) (vellumory--open-attachment source a)))
                                   'help-echo "Open this embedded file")
               (insert (propertize (format "  %s · %s%s%s\n" (vellumory--size-string (alist-get 'size attachment))
                                           (or (alist-get 'mimetype attachment) "")
                                           ;; An associated file (PDF/A-3, -4f) names its relation to the document.
                                           (let ((relationship (alist-get 'relationship attachment)))
                                             (if (and (eq (alist-get 'associated attachment) t)
                                                      (not (member relationship '(nil ""))))
                                                 (format " · %s of the document" (downcase relationship))
                                               ""))
                                           (if (member (alist-get 'description attachment) '(nil ""))
                                               "" (concat " · " (alist-get 'description attachment))))
                                   'face 'shadow))))
    (when page
      (vellumory--info-heading (format "Page %s" (with-current-buffer source
                                                   (vellumory--page-description (alist-get 'page page)))))
      (dolist (box '((media_box . "MediaBox") (crop_box . "CropBox") (bleed_box . "BleedBox")
                     (trim_box . "TrimBox") (art_box . "ArtBox")))
        (when-let* ((rect (alist-get (car box) page)))
          (vellumory--info-line (cdr box) (format "[%s]  %.1f × %.1f mm"
                                                  (mapconcat (lambda (v) (format "%g" v)) rect " ")
                                                  (* 25.4 (/ (- (aref rect 2) (aref rect 0)) 72.0))
                                                  (* 25.4 (/ (- (aref rect 3) (aref rect 1)) 72.0))))))
      (vellumory--info-line "Rotate" (format "%d° · UserUnit %g · object %s" (alist-get 'rotate page)
                                             (alist-get 'user_unit page) (alist-get 'object page)))
      (vellumory--info-line "Fonts" (mapconcat (lambda (f) (format "%s=%s%s" (alist-get 'resource f) (alist-get 'name f)
                                                                   (if (alist-get 'embedded f) "" " (not embedded)")))
                                               (alist-get 'fonts page) ", "))
      (vellumory--info-line "Images" (mapconcat (lambda (i) (format "%s %d×%d %s %s" (alist-get 'resource i)
                                                                    (alist-get 'width i) (alist-get 'height i)
                                                                    (alist-get 'colorspace i) (alist-get 'filter i)))
                                                (alist-get 'images page) ", "))
      (vellumory--info-line "Annotations" (mapconcat #'identity (alist-get 'annotations page) ", ")))
    (insert "\n" (propertize (substitute-command-keys
                              "\\<vellumory-details-mode-map>\\[vellumory-details-refresh] refresh  \\[vellumory-details-attach] attach  \\[vellumory-details-detach] remove attachment  \\[vellumory-details-edit-metadata] edit metadata  \\[quit-window] quit")
                             'face 'shadow))
    (goto-char (point-min))))

(defun vellumory--open-attachment (source attachment)
  "Extract ATTACHMENT from the document in buffer SOURCE and visit it."
  (with-current-buffer source
    (vellumory--rpc "attachment" `((index . ,(alist-get 'index attachment))
                                   (filename . ,(or (alist-get 'filename attachment) "")))
                    (lambda (reply)
                      (if-let* ((error (alist-get 'error reply))) (message "Vellumory: %s" error)
                        (let ((path (alist-get 'path (alist-get 'result reply))))
                          ;; Documents open in the reader; everything else in its own mode.
                          (if (string-match-p "\\.\\(?:pdf\\|xps\\|cbz\\|fb2\\)\\'" path)
                              (vellumory-open path)
                            (find-file-read-only path))))))))

(defvar-local vellumory--info-source nil)
(declare-function vellumory-edit-metadata "vellumory-metadata" ())

(defun vellumory-details-refresh ()
  "Reload the document report."
  (interactive)
  (let ((info-buffer (current-buffer)) (source vellumory--info-source))
    (unless (buffer-live-p source) (user-error "The document has been closed"))
    (with-current-buffer source
      (let ((page (or vellumory--current-page 0)))
        (vellumory--rpc "info" nil
                        (lambda (reply)
                          (if-let* ((error (alist-get 'error reply))) (message "Vellumory: %s" error)
                            (let ((info (alist-get 'result reply)))
                              (vellumory--rpc
                               "page-info" `((page . ,page))
                               (lambda (page-reply)
                                 (vellumory--rpc
                                  "signatures" nil
                                  (lambda (signatures)
                                    (setf (alist-get 'signatures info) (alist-get 'result signatures))
                                    (when (buffer-live-p info-buffer)
                                      (with-current-buffer info-buffer
                                        (vellumory--info-render source info
                                                                (alist-get 'result page-reply))))))))))))))))

(defun vellumory-details-attach ()
  "Attach a file to the document (a recorded, reversible edit)."
  (interactive)
  (with-current-buffer vellumory--info-source (call-interactively #'vellumory-attach-file)))

(defun vellumory-details-edit-metadata ()
  "Edit the title, author, language and other metadata."
  (interactive)
  (with-current-buffer vellumory--info-source (vellumory-edit-metadata)))

(defun vellumory-details-detach ()
  "Remove an embedded file (a recorded, reversible edit)."
  (interactive)
  (with-current-buffer vellumory--info-source (call-interactively #'vellumory-remove-attachment)))

(defvar vellumory-details-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "g" #'vellumory-details-refresh)
    (keymap-set map "a" #'vellumory-details-attach)
    (keymap-set map "D" #'vellumory-details-detach)
    (keymap-set map "e" #'vellumory-details-edit-metadata)
    (keymap-set map "TAB" #'forward-button)
    (keymap-set map "<backtab>" #'backward-button)
    map))

(define-derived-mode vellumory-details-mode special-mode "PDF Info"
  "Technical details of a document.  RET on an attachment opens it.
\\{vellumory-details-mode-map}"
  (setq-local truncate-lines t revert-buffer-function (lambda (&rest _) (vellumory-details-refresh))))

;;;###autoload
(defun vellumory-details ()
  "Show the document's format, standards, metadata, fonts and attachments."
  (interactive)
  (let ((source (current-buffer))
        (buffer (get-buffer-create (format "*Details: %s*" (buffer-name)))))
    (with-current-buffer buffer
      (vellumory-details-mode)
      (setq vellumory--info-source source)
      (vellumory-details-refresh))
    (pop-to-buffer buffer)))

(provide 'vellumory-details)
;;; vellumory-details.el ends here
