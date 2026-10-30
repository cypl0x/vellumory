;;; vellumory-doctor.el --- Check everything Vellumory needs -*- lexical-binding: t; -*-

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
;; `vellumory-doctor' checks everything Vellumory relies on, and says for
;; each problem what it affects and how to fix it: the Emacs build and its
;; image support, the worker program (found, running, speaking the expected
;; protocol, rendering a page), the tools behind particular features (qpdf,
;; veraPDF, Tesseract, LuaLaTeX, Typst, converters), which mode really opens
;; PDF files, evil and Doom integration, fonts, and the directories it
;; writes.  Fixes Emacs can make are buttons.  `vellumory-doctor-batch'
;; prints the same report for scripts and CI and exits non-zero on errors;
;; Doom's `doom doctor' runs these checks through the module's doctor.el.

;;; Code:
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'cl-lib)
(require 'subr-x)
(require 'vellumory-install)

(defvar vellumory-program)
(defvar vellumory-state-directory)
(defvar vellumory-cache-directory)
(defvar vellumory-mode-map)
(defvar nerd-icons-font-family)
(declare-function vellumory-install "vellumory-install" ())
(declare-function vellumory-evil-setup "vellumory-evil" ())
(declare-function vellumory-info--file "vellumory-install" ())
(declare-function evil-get-auxiliary-keymap "evil-core" (map state &optional create ignore-parent))

(defconst vellumory-doctor--protocol 3 "The worker protocol this Lisp speaks.")

(defconst vellumory-doctor--tools
  '(("qpdf" "every edit and the structure check" error
     "Install qpdf from your distribution (it is in every major one).")
    ("verapdf" "PDF/A, PDF/UA and WTPDF validation" warn
     "Install veraPDF (https://verapdf.org/software/) and put its verapdf launcher on PATH.")
    ("tesseract" "recognizing text (OCR)" warn "Install Tesseract and the languages you need.")
    ("ocrmypdf" "recognizing text (OCR) into the document" warn "Install OCRmyPDF.")
    ("lualatex" "exporting Org to accessible PDF with LaTeX" info "Install TeX Live 2025 or newer.")
    ("typst" "exporting Org to accessible PDF with Typst" info "Install Typst 0.14 or newer.")
    ("dvipdfmx" "reading DVI files" info "Part of TeX Live.")
    ("ps2pdf" "reading PostScript" info "Part of Ghostscript.")
    ("ddjvu" "reading DjVu" info "Part of DjVuLibre.")
    ("soffice" "reading office documents" info "Part of LibreOffice."))
  "External programs, as (PROGRAM PURPOSE SEVERITY-WHEN-MISSING HOW-TO-GET).")

(defun vellumory-doctor--check (title status detail &optional fix-label fix)
  "A check result: TITLE, STATUS (`ok', `warn', `error' or `info'), DETAIL.
FIX-LABEL and FIX, a command, offer a remedy."
  (list :title title :status status :detail detail :fix-label fix-label :fix fix))

(defun vellumory-doctor--worker-hello (program)
  "Run PROGRAM\\='s worker and ask it hello.
Return its reply, an alist, or a string saying what went wrong."
  (condition-case err
      (let* ((output "")
             (process (let ((default-directory (vellumory--existing-directory)))
                        (make-process :name "vellumory-doctor" :command (list program "serve")
                                      :connection-type 'pipe :noquery t
                                      :filter (lambda (_ chunk) (setq output (concat output chunk)))
                                      :stderr (get-buffer-create " *vellumory-doctor-stderr*"))))
             (deadline (+ (float-time) 10)))
        (unwind-protect
            (progn
              (process-send-string process "{\"id\":1,\"method\":\"hello\"}\n")
              (while (and (< (float-time) deadline) (not (string-search "\n" output)))
                (accept-process-output process 0.1))
              (if (string-search "\n" output)
                  (alist-get 'result (json-parse-string (car (split-string output "\n"))
                                                        :object-type 'alist :null-object nil))
                "no answer within 10 seconds"))
          (when (process-live-p process) (delete-process process))))
    (error (error-message-string err))))

(defun vellumory-doctor--emacs ()
  "Checks of the Emacs build."
  (list
   (if (version<= "29.1" emacs-version)
       (vellumory-doctor--check (format "Emacs %s" emacs-version) 'ok "29.1 or newer is needed.")
     (vellumory-doctor--check (format "Emacs %s" emacs-version) 'error
                              "Vellumory needs Emacs 29.1 or newer."))
   (if (or (display-graphic-p) (seq-some #'display-graphic-p (frame-list)) noninteractive)
       (vellumory-doctor--check "A graphical frame" (if (display-graphic-p) 'ok 'info)
                                (if (display-graphic-p) "Pages are drawn as images."
                                  "This frame is a terminal; documents open in graphical frames."))
     (vellumory-doctor--check "A graphical frame" 'error
                              "Documents are images: Vellumory needs a graphical frame (not a terminal)."))
   (if (image-type-available-p 'pbm)
       (vellumory-doctor--check "Image support" 'ok "PBM images, as the worker sends them.")
     (vellumory-doctor--check "Image support" 'error
                              "This Emacs cannot show PBM images, which the pages are sent as."))
   (if (image-type-available-p 'png)
       (vellumory-doctor--check "PNG images" 'ok "Used for previews of some documents.")
     (vellumory-doctor--check "PNG images" 'warn "This Emacs has no PNG support."))
   (if (and (eq window-system 'pgtk) (getenv "DISPLAY") (not (getenv "WAYLAND_DISPLAY")))
       (vellumory-doctor--check "Display" 'warn
                                (concat "A pure-GTK Emacs on X: Emacs calls this unsupported, and every "
                                        "redraw waits for GTK's frame clock (about 8 ms a scroll step). "
                                        "Use pgtk under Wayland, or an X build (--with-x-toolkit) on X."))
     (vellumory-doctor--check "Display" 'ok (format "%s" (or window-system "none"))))
   (if (bound-and-true-p gcmh-mode)
       (vellumory-doctor--check "Garbage collection" 'ok "gcmh collects when idle; Vellumory leaves it to it.")
     (vellumory-doctor--check "Garbage collection" 'ok
                              "Vellumory holds collections back while reading and collects when it rests."))))

(defvar vellumory-doctor--worker-tools nil
  "Programs the worker said it runs, as (NAME . FILE): absolute in a Nix build.")

(defun vellumory-doctor--worker ()
  "Checks of the worker program; it also notes the programs the worker runs."
  (let* ((program (and (boundp 'vellumory-program) vellumory-program))
         (path (and program (if (file-name-absolute-p program)
                                (and (file-executable-p program) program)
                              (executable-find program)))))
    (if (not path)
        (list (vellumory-doctor--check
               "Worker program" 'error
               (format (concat "`%s' was not found.  It renders the pages: build it from a source "
                               "checkout with `lake build', install it with Nix (nix profile install "
                               "github:cypl0x/vellumory), or set `vellumory-program' to its path.")
                       (or program "vellumory"))
               "Set vellumory-program…" (lambda () (interactive) (customize-variable 'vellumory-program))))
      (let ((hello (vellumory-doctor--worker-hello path)))
        (unless (stringp hello) (setq vellumory-doctor--worker-tools (alist-get 'tools hello)))
        (cons (vellumory-doctor--check "Worker program" 'ok (abbreviate-file-name path))
              (if (stringp hello)
                  (list (vellumory-doctor--check "Worker answers" 'error
                                                 (format "It did not answer as expected: %s" hello)))
                (list (if (eql (alist-get 'protocol hello) vellumory-doctor--protocol)
                          (vellumory-doctor--check "Worker protocol" 'ok
                                                   (format "version %d, as this Lisp expects"
                                                           vellumory-doctor--protocol))
                        (vellumory-doctor--check "Worker protocol" 'error
                                                 (format "The worker speaks version %s, this Lisp %d: install matching versions."
                                                         (alist-get 'protocol hello) vellumory-doctor--protocol)))
                      (vellumory-doctor--check "Rendering engine" 'ok
                                               (format "MuPDF %s · %s render threads on %s cores"
                                                       (alist-get 'mupdf hello) (alist-get 'threads hello)
                                                       (alist-get 'cores hello))))))))))

(defun vellumory-doctor--tools ()
  "Checks of the external programs features rely on."
  (mapcar (pcase-lambda (`(,program ,purpose ,severity ,how))
            (let ((own (alist-get (intern (if (equal program "tesseract") "ocrmypdf" program))
                                  vellumory-doctor--worker-tools)))
              ;; The worker's own OCRmyPDF brings its Tesseract.
              (if (and own (file-name-absolute-p own) (file-executable-p own))
                  (vellumory-doctor--check program 'ok (format "for %s (the worker's own: %s)" purpose
                                                              (abbreviate-file-name own)))
                (vellumory-doctor--tool program (or own program) purpose severity how))))
          vellumory-doctor--tools))

(defun vellumory-doctor--tool (program name purpose severity how)
  "The check of PROGRAM, found on PATH as NAME, used for PURPOSE.
When it is missing the check has SEVERITY and says HOW to get it."
  (if-let* ((path (executable-find name)))
      (vellumory-doctor--check program 'ok (format "for %s (%s)" purpose (abbreviate-file-name path)))
    (vellumory-doctor--check program severity (format "Missing: no %s.  %s" purpose how))))

(defun vellumory-doctor--opening ()
  "Which mode opens PDF files, and what might take them over."
  (let* ((by-name (let ((case-fold-search t)) (assoc-default "document.pdf" auto-mode-alist #'string-match-p)))
         (by-name (if (consp by-name) (car by-name) by-name))
         (by-content (cdr (seq-find (lambda (entry) (and (stringp (car entry)) (string-prefix-p (car entry) "%PDF")))
                                    magic-mode-alist)))
         (mode (or by-content by-name)))
    (list
     (if (eq mode 'vellumory-mode)
         (vellumory-doctor--check "PDF files open in Vellumory" 'ok
                                  (format "by %s" (if by-content "their contents (magic-mode-alist)" "their name")))
       (vellumory-doctor--check "PDF files open in Vellumory" 'warn
                                (format "They open in %s.  `vellumory-install' makes Vellumory their reader%s."
                                        (or mode "the default mode")
                                        (if (memq mode '(pdf-view-mode doc-view-mode))
                                            " (after pdf-tools or DocView set themselves up)" ""))
                                "Make Vellumory the reader" #'vellumory-install))
     (if (and (featurep 'pdf-tools) (bound-and-true-p pdf-tools-enabled-modes)
              (not (eq mode 'vellumory-mode)))
         (vellumory-doctor--check "pdf-tools" 'info
                                  "pdf-tools is loaded and may claim PDF files when it installs itself.")
       (vellumory-doctor--check "Other PDF readers" 'ok "None takes PDF files over.")))))

(defun vellumory-doctor--integration ()
  "Checks of evil, Doom, fonts and companions."
  (delq nil
        (list
         (when (featurep 'evil)
           (if (and (boundp 'vellumory-mode-map)
                    (let ((map (evil-get-auxiliary-keymap vellumory-mode-map 'normal)))
                      (and map (lookup-key map "j"))))
               (vellumory-doctor--check "evil" 'ok "Normal-state keys are set up (j, k, gg, G, /, …).")
             (vellumory-doctor--check "evil" 'warn
                                      "evil is loaded but Vellumory's keys are not: call `vellumory-evil-setup'."
                                      "Set them up now" (lambda () (interactive)
                                                          (require 'vellumory-evil)
                                                          (vellumory-evil-setup)))))
         (when (featurep 'doom)
           (vellumory-doctor--check "Doom Emacs" (if (fboundp '+vellumory-escape-h) 'ok 'warn)
                                    (if (fboundp '+vellumory-escape-h)
                                        "The :tools vellumory module is active."
                                      "Enable the :tools vellumory module for Doom's keys and popups.")))
         (when (boundp 'nerd-icons-font-family)
           (if (find-font (font-spec :family nerd-icons-font-family))
               (vellumory-doctor--check "Icon font" 'ok (format "%s is installed." nerd-icons-font-family))
             (vellumory-doctor--check "Icon font" 'warn
                                      (format "%s is missing: the mode line shows boxes instead of icons."
                                              nerd-icons-font-family)
                                      "Install it" 'nerd-icons-install-fonts)))
         (vellumory-doctor--check "Manual" (if (vellumory-info--file) 'ok 'info)
                                  (if (vellumory-info--file)
                                      "M-x vellumory-info reads it."
                                    "The Info manual is not installed with this copy.")))))

(defun vellumory-doctor--directories ()
  "Checks of the directories Vellumory writes."
  (cl-loop for (label . variable) in '(("Reading positions" . vellumory-state-directory)
                                       ("Cache" . vellumory-cache-directory))
           for directory = (and (boundp variable) (symbol-value variable))
           when directory
           collect (let ((existing (locate-dominating-file directory #'file-directory-p)))
                     (if (and existing (file-writable-p existing))
                         (vellumory-doctor--check label 'ok (abbreviate-file-name directory))
                       (vellumory-doctor--check label 'error
                                                (format "%s cannot be written." (abbreviate-file-name directory)))))))

(defun vellumory-doctor--sections ()
  "Every check, as (HEADING . CHECKS)."
  (require 'vellumory nil t)
  (setq vellumory-doctor--worker-tools nil)
  (list (cons "Emacs" (vellumory-doctor--emacs))
        (cons "Worker" (vellumory-doctor--worker))
        (cons "Opening PDF files" (vellumory-doctor--opening))
        (cons "Programs for particular features" (vellumory-doctor--tools))
        (cons "Integration" (vellumory-doctor--integration))
        (cons "Directories" (vellumory-doctor--directories))))

(defconst vellumory-doctor--marks
  '((ok "✓" success) (info "·" shadow) (warn "!" warning) (error "✗" error))
  "How each status is shown: (STATUS MARK FACE).")

(defun vellumory-doctor--count (sections status)
  "How many checks of SECTIONS have STATUS."
  (cl-loop for (_ . checks) in sections sum (cl-count status checks :key (lambda (c) (plist-get c :status)))))

;;;###autoload
(defun vellumory-doctor ()
  "Check everything Vellumory relies on and show what to fix."
  (interactive)
  (let ((sections (vellumory-doctor--sections))
        (buffer (get-buffer-create "*Vellumory doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Vellumory doctor" 'face 'bold) "\n"
                (let ((errors (vellumory-doctor--count sections 'error))
                      (warnings (vellumory-doctor--count sections 'warn)))
                  (propertize (cond ((> errors 0) (format "%d problem%s to fix, %d warning%s" errors
                                                          (if (= errors 1) "" "s") warnings (if (= warnings 1) "" "s")))
                                    ((> warnings 0) (format "Ready; %d warning%s" warnings (if (= warnings 1) "" "s")))
                                    (t "Ready: everything Vellumory uses is in place"))
                              'face (if (> errors 0) 'error (if (> warnings 0) 'warning 'success))))
                "\n")
        (pcase-dolist (`(,heading . ,checks) sections)
          (insert "\n" (propertize heading 'face 'bold) "\n")
          (dolist (check checks)
            (pcase-let ((`(,_ ,mark ,face) (assq (plist-get check :status) vellumory-doctor--marks)))
              (insert "  " (propertize mark 'face face) " " (plist-get check :title)
                      (propertize (concat "  " (plist-get check :detail)) 'face 'shadow))
              (when-let* ((fix (plist-get check :fix)))
                (insert "  ")
                (insert-text-button (plist-get check :fix-label) 'follow-link t
                                    'action (lambda (_) (call-interactively fix) (vellumory-doctor))))
              (insert "\n"))))
        (insert "\n" (propertize (substitute-command-keys
                                  "\\<vellumory-doctor-mode-map>\\[vellumory-doctor] checks again · \\[vellumory-doctor-copy] copies the report for a bug report · \\[quit-window] closes")
                                 'face 'shadow)
                "\n")
        (goto-char (point-min)))
      (vellumory-doctor-mode))
    (pop-to-buffer buffer)))

(defun vellumory-doctor-copy ()
  "Copy the report as plain text, for a bug report."
  (interactive)
  (kill-new (concat (buffer-substring-no-properties (point-min) (point-max))
                    (format "\n%s\n" (emacs-version))))
  (message "Copied the report"))

(defvar vellumory-doctor-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "g" #'vellumory-doctor)
    (keymap-set map "w" #'vellumory-doctor-copy)
    (keymap-set map "TAB" #'forward-button)
    (keymap-set map "<backtab>" #'backward-button)
    map)
  "Keys of the doctor's report.")

(define-derived-mode vellumory-doctor-mode special-mode "Vellumory-Doctor"
  "The report of `vellumory-doctor'.
\\{vellumory-doctor-mode-map}"
  (setq-local truncate-lines nil word-wrap t))

;;;###autoload
(defun vellumory-doctor-batch ()
  "Print the doctor's report and exit: 1 when a check failed, else 0.
For `emacs --batch -l vellumory-doctor -f vellumory-doctor-batch'."
  (let ((sections (vellumory-doctor--sections)))
    (pcase-dolist (`(,heading . ,checks) sections)
      (princ (format "%s\n" heading))
      (dolist (check checks)
        (princ (format "  %s %s  %s\n" (nth 1 (assq (plist-get check :status) vellumory-doctor--marks))
                       (plist-get check :title) (plist-get check :detail)))))
    (kill-emacs (if (> (vellumory-doctor--count sections 'error) 0) 1 0))))

(provide 'vellumory-doctor)
;;; vellumory-doctor.el ends here
