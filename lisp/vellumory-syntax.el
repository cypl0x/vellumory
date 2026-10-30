;;; vellumory-syntax.el --- View and edit a document's PDF syntax -*- lexical-binding: t; -*-

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
;; `vellumory-syntax' shows the document in qpdf's QDF form: normalized,
;; uncompressed, one object per block, one dictionary key per line.  RET or
;; `M-.' on "12 0 R" visits object 12; Imenu lists objects by /Type and pages.
;; The view is read-only; \\[read-only-mode] makes it editable, and
;; \\[vellumory-syntax-commit] adopts the edited syntax as a recorded,
;; undoable state: fix-qdf recomputes offsets and stream lengths, qpdf writes
;; a normal PDF, and the result must open before it replaces the file.

;;; Code:
(require 'cl-lib)
(require 'imenu)
(require 'xref)

(defvar vellumory--source-file)
(declare-function vellumory--cli "vellumory-edit" (args callback &optional buffer input))
(declare-function vellumory-reload "vellumory" ())
(declare-function vellumory--history-refresh-buffers "vellumory-edit" ())

(defgroup vellumory-syntax nil "PDF syntax view." :group 'vellumory)

(defcustom vellumory-syntax-size-warning (* 64 1024 1024)
  "Ask before showing QDF renderings larger than this many bytes."
  :type 'natnum)

(defconst vellumory-syntax-font-lock-keywords
  `(("^%%.*$" . font-lock-doc-face)
    ("^\\([0-9]+ [0-9]+\\) \\(obj\\)\\_>" (1 font-lock-function-name-face) (2 font-lock-keyword-face))
    (,(regexp-opt '("endobj" "stream" "endstream" "xref" "trailer" "startxref") 'symbols)
     . font-lock-keyword-face)
    ("\\_<\\([0-9]+ [0-9]+ R\\)\\_>" 1 'link)
    ("^\\s-*\\(/[^][ \t\n()<>{}/%]+\\)" 1 font-lock-variable-name-face)
    ("/[^][ \t\n()<>{}/%]+" . font-lock-constant-face)
    ("<<\\|>>" . font-lock-builtin-face)
    ("(\\(?:\\\\.\\|[^\\)\n]\\)*)" . font-lock-string-face)
    ("<[0-9A-Fa-f \t]+>" . font-lock-string-face)
    (,(regexp-opt '("true" "false" "null") 'symbols) . font-lock-constant-face))
  "Highlighting for QDF/PDF syntax.")

(defvar-local vellumory--syntax-source-file nil)
(defvar-local vellumory--syntax-qdf nil)

(defun vellumory-syntax-follow ()
  "Visit the object referenced at point, such as \"12 0 R\"."
  (interactive)
  (let ((reference (save-excursion
                     (skip-chars-backward "0-9 R")
                     (and (looking-at "\\s-*\\([0-9]+\\) \\([0-9]+\\) R\\_>")
                          (cons (match-string 1) (match-string 2))))))
    (unless reference (user-error "No object reference at point"))
    (let ((target (save-excursion
                    (goto-char (point-min))
                    (re-search-forward (format "^%s %s obj\\_>" (car reference) (cdr reference)) nil t))))
      (unless target (user-error "Object %s %s is not in this document" (car reference) (cdr reference)))
      (xref-push-marker-stack)
      (goto-char target)
      (beginning-of-line)
      (when (eq (window-buffer) (current-buffer)) (recenter 0)))))

(defun vellumory--syntax-imenu ()
  "Objects by number and /Type, and the QDF page markers."
  (let (objects pages)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9]+ [0-9]+\\) obj\\_>" nil t)
        (let* ((start (line-beginning-position)) (name (match-string 1))
               (end (save-excursion (or (re-search-forward "^endobj" nil t) (point-max))))
               (type (save-excursion
                       (when (re-search-forward "^\\s-*/Type /\\([A-Za-z]+\\)" (min end (+ (point) 4000)) t)
                         (match-string 1)))))
          (push (cons (if type (format "%s %s" name type) name) start) objects)))
      (goto-char (point-min))
      (while (re-search-forward "^%% Page \\([0-9]+\\)" nil t)
        (push (cons (concat "Page " (match-string 1)) (line-beginning-position)) pages)))
    (append (and pages (list (cons "Pages" (nreverse pages)))) (nreverse objects))))

(defun vellumory-syntax-commit ()
  "Adopt the edited syntax as the document's new, recorded state."
  (interactive)
  (unless (buffer-modified-p) (user-error "Nothing changed"))
  (let ((temporary (make-temp-file "vellumory-syntax-" nil ".qdf.pdf"))
        (file vellumory--syntax-source-file) (buffer (current-buffer)))
    (let ((coding-system-for-write 'no-conversion))
      (write-region nil nil temporary nil 'silent))
    (vellumory--cli (list "edit" file (json-serialize `((op . "syntax") (qdf . ,temporary))))
                    (lambda (result)
                      (delete-file temporary)
                      (with-current-buffer buffer
                        (set-buffer-modified-p nil)
                        (setq buffer-read-only t))
                      (when-let* ((reader (find-buffer-visiting file)))
                        (with-current-buffer reader
                          (vellumory-reload)
                          (vellumory--history-refresh-buffers)))
                      (message "%s · state %s" (alist-get 'label (alist-get 'node result))
                               (alist-get 'index (alist-get 'node result)))))))

(defun vellumory-syntax-discard ()
  "Throw away unsaved edits and show the document's syntax again."
  (interactive)
  (when (or (not (buffer-modified-p)) (yes-or-no-p "Discard your edits? "))
    (let ((inhibit-read-only t) (qdf vellumory--syntax-qdf))
      (erase-buffer)
      (insert-file-contents-literally qdf)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))))

(defconst vellumory-syntax-quit
  '(menu-item "" quit-window :filter (lambda (command) (and buffer-read-only command)))
  "The binding of q: close the view while it is read-only.
While editing, the binding filters itself away and q is what it is
elsewhere: a letter, or evil's macro recording in normal state.")

(defvar vellumory-syntax-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "q" vellumory-syntax-quit)
    (keymap-set map "RET" #'vellumory-syntax-follow)
    (keymap-set map "M-." #'vellumory-syntax-follow)
    (keymap-set map "C-c C-c" #'vellumory-syntax-commit)
    (keymap-set map "C-c C-k" #'vellumory-syntax-discard)
    map))

(define-derived-mode vellumory-syntax-mode prog-mode "PDF Syntax"
  "A document's objects in qpdf's QDF form.
\\[read-only-mode] to edit, \\[vellumory-syntax-commit] to adopt the edit
\(recorded and undoable), \\[vellumory-syntax-discard] to throw it away.
\\{vellumory-syntax-mode-map}"
  (setq-local font-lock-defaults '(vellumory-syntax-font-lock-keywords t)
              comment-start "%" comment-end ""
              imenu-create-index-function #'vellumory--syntax-imenu
              buffer-file-coding-system 'no-conversion
              truncate-lines t))

;;;###autoload
(defun vellumory-syntax ()
  "Show this document's PDF syntax (QDF), editable and recorded on commit."
  (interactive)
  (let ((file (or vellumory--source-file (user-error "Only local documents have a syntax view"))))
    (vellumory--cli
     (list "qdf" file)
     (lambda (result)
       (let* ((qdf (alist-get 'path result))
              (size (file-attribute-size (file-attributes qdf)))
              (buffer (get-buffer-create (format "*Syntax: %s*" (file-name-nondirectory file)))))
         (when (or (< size vellumory-syntax-size-warning)
                   (yes-or-no-p (format "The syntax view is %s; show it? " (file-size-human-readable size))))
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer)
               (set-buffer-multibyte nil)
               (insert-file-contents-literally qdf))
             (vellumory-syntax-mode)
             (setq vellumory--syntax-source-file file vellumory--syntax-qdf qdf buffer-read-only t)
             (set-buffer-modified-p nil)
             (goto-char (point-min)))
           (pop-to-buffer buffer)))))))

(provide 'vellumory-syntax)
;;; vellumory-syntax.el ends here
