;;; vellumory-convert.el --- Documents read through a conversion to PDF -*- lexical-binding: t; -*-

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
;; The formats DocView reads through converters: DVI (dvipdfmx), PostScript
;; (ps2pdf), DjVu (ddjvu) and office documents (LibreOffice).  The conversion is
;; cached by the original's content and the converter, under
;; `vellumory-cache-directory', so a document converts once; then it is read
;; like any PDF: search, occur, selection, links, its reading position.  The
;; original is watched: a recompiled DVI converts again and is shown at the
;; same place, as DocView's auto-revert does.  While converting, the buffer
;; says so without changing its text, which stays the original's.

;;; Code:
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'cl-lib)
(require 'subr-x)
(require 'filenotify)

(defvar vellumory--materialized)
(defvar vellumory--file)
(defvar vellumory--source-file)
(defvar vellumory--status)
(defvar vellumory--original-file)
(defvar vellumory-cache-directory)
(defvar vellumory-watch-file)
(declare-function vellumory-mode "vellumory" ())
(declare-function vellumory-reload "vellumory" ())

(defcustom vellumory-converters
  '((("dvi") "dvipdfmx" "-q" "-o" output input)
    (("ps" "eps") "ps2pdf" input output)
    (("djvu" "djv") "ddjvu" "-format=pdf" input output)
    (("odt" "ods" "odp" "odg" "doc" "docx" "xls" "xlsx" "ppt" "pptx" "rtf")
     "soffice" "--headless" "--convert-to" "pdf" "--outdir" directory input))
  "How documents become PDF: (EXTENSIONS PROGRAM ARGUMENT…).
Among the arguments `input' and `output' stand for the files, and `directory'
for one the program writes INPUT's name with the extension .pdf into.  An
entry whose program is not installed is skipped."
  :type '(repeat (cons (repeat string) (cons string (repeat sexp)))) :group 'vellumory)

(defvar-local vellumory--original-watch nil "Watch on the original, to convert it again.")
(defvar-local vellumory--reconvert-timer nil)

(defun vellumory--converter (file)
  "The entry of `vellumory-converters' for FILE whose program is installed."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (seq-find (lambda (entry) (and (member extension (car entry)) (executable-find (cadr entry))))
              vellumory-converters)))

(defun vellumory--converted-file (file entry)
  "Where FILE converted by ENTRY is cached: by its content and the converter."
  (let* ((content (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally file)
                    (secure-hash 'sha256 (current-buffer))))
         (hash (secure-hash 'sha256 (concat content (prin1-to-string (cdr entry))))))
    (expand-file-name (format "converted/%s/%s.pdf" (substring hash 0 2) hash) vellumory-cache-directory)))

(defun vellumory--convert (file entry target callback)
  "Convert FILE with ENTRY into TARGET; then CALLBACK with nil, or what failed."
  (let* ((directory (make-temp-file "vellumory-convert-" t))
         (output (expand-file-name (concat (file-name-base file) ".pdf") directory))
         (arguments (mapcar (lambda (argument)
                              (pcase argument
                                ('input (expand-file-name file)) ('output output) ('directory directory)
                                (_ argument)))
                            (cddr entry)))
         (log (generate-new-buffer " *vellumory-convert*"))
         ;; Relative references (a DVI's figures) resolve from the original's place.
         (default-directory (file-name-directory (expand-file-name file))))
    (let ((default-directory (vellumory--existing-directory)))
      (make-process
       :name "vellumory-convert" :buffer log :noquery t :connection-type 'pipe
       :command (cons (cadr entry) arguments)
       :sentinel
       (lambda (process _event)
         (unless (process-live-p process)
           (let ((failure (cond ((not (zerop (process-exit-status process)))
                                 (format "%s failed: %s" (cadr entry)
                                         (with-current-buffer log (string-trim (buffer-string)))))
                                ((not (file-exists-p output)) (format "%s wrote no PDF" (cadr entry))))))
             (unless failure
               (make-directory (file-name-directory target) t)
               (rename-file output target t))
             (kill-buffer log)
             (delete-directory directory t)
             (funcall callback failure))))))))

(defun vellumory--show-converted (target file)
  "Read TARGET, the conversion of FILE, in this buffer."
  (remove-overlays (point-min) (point-max) 'vellumory-converting t)
  (let ((vellumory--materialized target)) (vellumory-mode))
  (setq vellumory--original-file file)
  (setq-local mode-name (format "Vellumory %s" (upcase (or (file-name-extension file) ""))))
  (vellumory--watch-original file))

(defun vellumory--watch-original (file)
  "Convert FILE again when it changes, and show the new conversion."
  (when (and vellumory-watch-file (not vellumory--original-watch))
    (let ((buffer (current-buffer)))
      (setq vellumory--original-watch
            (ignore-errors
              (file-notify-add-watch
               (file-name-directory file) '(change)
               (lambda (event)
                 (when (and (buffer-live-p buffer) (member file (cddr event))
                            (memq (cadr event) '(changed created renamed attribute-changed)))
                   (with-current-buffer buffer
                     ;; A compiler writes in bursts: convert once it rests.
                     (when (timerp vellumory--reconvert-timer) (cancel-timer vellumory--reconvert-timer))
                     (setq vellumory--reconvert-timer
                           (run-with-timer 0.5 nil (lambda ()
                                                     (when (buffer-live-p buffer)
                                                       (with-current-buffer buffer (vellumory--reconvert))))))))))))
      (add-hook 'kill-buffer-hook
                (lambda () (when vellumory--original-watch (file-notify-rm-watch vellumory--original-watch)))
                nil t))))

(defun vellumory--reconvert ()
  "Show the conversion of the original as it is now."
  (let* ((file vellumory--original-file) (entry (vellumory--converter file)) (buffer (current-buffer)))
    (when (and entry (file-readable-p file))
      (let ((target (vellumory--converted-file file entry)))
        (cl-flet ((show () (with-current-buffer buffer
                             (setq vellumory--file target vellumory--source-file target)
                             (vellumory-reload))))
          (if (file-exists-p target) (show)
            (setq vellumory--status "Converting…")
            (force-mode-line-update)
            (vellumory--convert file entry target
                                (lambda (failure)
                                  (when (buffer-live-p buffer)
                                    (if failure (message "Vellumory: %s" failure) (show)))))))))))

;;;###autoload
(defun vellumory-converted-mode ()
  "Read this file (DVI, PostScript, DjVu, an office document) converted to PDF.
The converter is the first installed one of `vellumory-converters' for its
extension; the conversion is cached and renewed when the file changes."
  (interactive)
  (require 'vellumory)
  (let* ((file (or buffer-file-name (user-error "Only files are converted")))
         (entry (or (vellumory--converter file)
                    (user-error "No converter for %s is installed (see `vellumory-converters')"
                                (file-name-nondirectory file))))
         (target (vellumory--converted-file file entry))
         (buffer (current-buffer)))
    (if (file-exists-p target)
        (vellumory--show-converted target file)
      ;; Said, not written: the buffer's text stays the original's.
      (let ((overlay (make-overlay (point-min) (point-max))))
        (overlay-put overlay 'vellumory-converting t)
        (overlay-put overlay 'display (propertize (format "Converting %s with %s…"
                                                          (file-name-nondirectory file) (cadr entry))
                                                  'face 'shadow)))
      (setq buffer-read-only t)
      (vellumory--convert file entry target
                          (lambda (failure)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (if (not failure) (vellumory--show-converted target file)
                                  (remove-overlays (point-min) (point-max) 'vellumory-converting t)
                                  (message "Vellumory: %s" failure)))))))))

;;;###autoload
(defun vellumory-converted-extensions ()
  "Extensions some installed converter reads."
  (cl-loop for (extensions program) in vellumory-converters
           when (executable-find program) append extensions))

(provide 'vellumory-convert)
;;; vellumory-convert.el ends here
