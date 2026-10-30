;;; vellumory-install.el --- Register documents with Vellumory -*- lexical-binding: t; -*-

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
;; The small part of Vellumory needed before any document is visited: file
;; associations, a handler that visits documents without reading their bytes
;; into the buffer, and the large-file exemption.  Loading this does not load
;; the reader; `vellumory-mode' is autoloaded on the first visit.

;;; Code:
(declare-function Info-find-file "info" (filename &optional noerror no-pop-to-dir))

(require 'seq)

(declare-function vellumory-sync-select-viewer "vellumory-sync" ())
;; Called by `vellumory-install', which must work without the generated autoloads.
(autoload 'vellumory-converted-extensions "vellumory-convert")
(autoload 'vellumory-converted-mode "vellumory-convert" nil t)

(defgroup vellumory nil "Native document reading." :group 'multimedia
  :prefix "vellumory-")

(defcustom vellumory-dired-key ";"
  "Dired prefix for batch commands on marked documents, or nil for none."
  :type '(choice (const :tag "None" nil) key) :group 'vellumory)

(defcustom vellumory-file-extensions '("pdf" "xps" "oxps" "cbz" "fb2" "epub")
  "File extensions `vellumory-install' associates with `vellumory-mode'."
  :type '(repeat string))

(defcustom vellumory-converted-not-visited '("ps" "eps")
  "Converted formats `vellumory-install' does not visit as documents.
PostScript is a program, usually edited in `ps-mode'."
  :type '(repeat string))

(defcustom vellumory-visit-without-reading t
  "Visit local documents without reading their bytes into the buffer.
The worker reads the file itself; `vellumory-mode' would erase the bytes
immediately.  This saves time and memory proportional to the file size."
  :type 'boolean)

;;;###autoload
(defun vellumory--extension-regexp ()
  "A regexp matching file names with an extension in `vellumory-file-extensions'."
  (concat "\\." (regexp-opt vellumory-file-extensions) "\\'"))

;;;###autoload
(defun vellumory--large-file-advice (fn size op-type filename &optional offer-raw)
  "Skip FN's large-file question for documents: their buffers never keep the bytes.
SIZE, OP-TYPE, FILENAME and OFFER-RAW are passed to FN otherwise."
  (unless (and (equal op-type "open") (stringp filename)
               (let ((case-fold-search t)) (string-match-p (vellumory--extension-regexp) filename)))
    (funcall fn size op-type filename offer-raw)))

;;;###autoload
(defun vellumory--visit-without-reading-p (file)
  "Non-nil if FILE is to be visited without reading its bytes.
That is when `vellumory-visit-without-reading' is set and FILE is a
local, readable file that `auto-mode-alist' visits in `vellumory-mode'."
  (and vellumory-visit-without-reading (not (file-remote-p file))
       (let ((mode (let ((case-fold-search t))
                     (assoc-default (file-name-sans-versions file) auto-mode-alist #'string-match-p))))
         (eq (if (consp mode) (car mode) mode) 'vellumory-mode))
       (let ((inhibit-file-name-handlers (cons #'vellumory--file-handler inhibit-file-name-handlers))
             (inhibit-file-name-operation 'file-readable-p))
         (file-readable-p file))))

;;;###autoload
(defun vellumory--file-handler (operation &rest args)
  "Visit documents without inserting their contents; delegate everything else.
As a file name handler, run OPERATION with ARGS.  Only a whole-file,
non-literal `insert-file-contents' with VISIT set is intercepted.  It
does what that primitive documents for VISIT: set the visited file name
and modtime and mark the buffer unmodified."
  (if (and (eq operation 'insert-file-contents)
           (nth 1 args) (not (nth 2 args)) (not (nth 3 args)) (not (nth 4 args))
           (not (eq coding-system-for-read 'no-conversion))
           (vellumory--visit-without-reading-p (expand-file-name (car args))))
      (let ((file (expand-file-name (car args))))
        (setq buffer-file-name file)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil)
        (list file 0))
    (let ((inhibit-file-name-handlers
           (cons #'vellumory--file-handler
                 (and (eq inhibit-file-name-operation operation) inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))

;;;###autoload
(defun vellumory-install ()
  "Visit `vellumory-file-extensions' and \"%PDF\" files in `vellumory-mode'."
  (interactive)
  (add-to-list 'auto-mode-alist (cons (vellumory--extension-regexp) 'vellumory-mode))
  (add-to-list 'magic-mode-alist '("%PDF" . vellumory-mode))
  (add-to-list 'file-name-handler-alist (cons (vellumory--extension-regexp) #'vellumory--file-handler))
  ;; Formats DocView reads through converters, those installed; PostScript
  ;; stays in ps-mode, where it is edited (M-x vellumory-converted-mode reads it).
  (when-let* ((extensions (seq-difference (vellumory-converted-extensions) vellumory-converted-not-visited)))
    (add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt extensions) "\\'") 'vellumory-converted-mode)))
  (advice-add 'abort-if-file-too-large :around #'vellumory--large-file-advice)
  ;; Org links work before any document is open; this loads no reader code.
  (with-eval-after-load 'ol (require 'vellumory-org))
  ;; Runs after all viewer setup (Doom's :lang latex too), in TeX buffers.
  (add-hook 'TeX-mode-hook #'vellumory-sync-select-viewer)
  ;; org-noter sessions on documents, once org-noter is loaded.
  (with-eval-after-load 'org-noter-core (require 'vellumory-noter))
  ;; Batch commands on marked documents; the code loads on first use.
  (when vellumory-dired-key
    (autoload 'vellumory-dired-map "vellumory-dired" nil nil 'keymap)
    (with-eval-after-load 'dired
      (defvar dired-mode-map)
      (keymap-set dired-mode-map vellumory-dired-key 'vellumory-dired-map))))

;;;###autoload
(defun vellumory-uninstall ()
  "Undo `vellumory-install'."
  (interactive)
  (setq auto-mode-alist (rassq-delete-all 'vellumory-converted-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all 'vellumory-mode auto-mode-alist)
        magic-mode-alist (rassq-delete-all 'vellumory-mode magic-mode-alist)
        file-name-handler-alist (rassq-delete-all #'vellumory--file-handler file-name-handler-alist))
  (advice-remove 'abort-if-file-too-large #'vellumory--large-file-advice)
  (remove-hook 'TeX-mode-hook #'vellumory-sync-select-viewer)
  (when (and vellumory-dired-key (boundp 'dired-mode-map))
    (keymap-unset dired-mode-map vellumory-dired-key t)))


;;;###autoload
(defun vellumory--existing-directory ()
  "`default-directory' if it exists, else the home directory.
Processes start there: none can start in a deleted directory (Emacs 29
refuses outright), and Vellumory passes its programs absolute file names."
  (if (file-accessible-directory-p default-directory) default-directory (expand-file-name "~/")))

(defvar vellumory-program)

(defun vellumory-info--file ()
  "The manual's file, or nil.
It is beside the Lisp (as the Nix package installs it), beside the worker
program's package (when the Lisp comes from elsewhere, as with straight.el
and a Nix-installed worker), or in Info's path."
  (let* ((program (executable-find (or (bound-and-true-p vellumory-program) "vellumory")))
         (prefix (and program (file-name-directory (directory-file-name
                                                     (file-name-directory (file-truename program))))))
         (candidates (list (expand-file-name "vellumory.info"
                                             (file-name-directory (or (locate-library "vellumory-install") "")))
                           (and prefix (expand-file-name "share/info/vellumory.info" prefix))
                           (and prefix (expand-file-name "share/emacs/site-lisp/vellumory/vellumory.info" prefix)))))
    (or (seq-find (lambda (file) (and file (file-exists-p file))) candidates)
        (progn (require 'info) (ignore-errors (Info-find-file "vellumory" t))))))

(defun vellumory-info ()
  "Read the Vellumory manual in Info."
  (interactive)
  (info (or (vellumory-info--file) "vellumory")))

(provide 'vellumory-install)
;;; vellumory-install.el ends here
