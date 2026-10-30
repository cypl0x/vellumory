;;; screen-bench.el --- The same reading, timed on the screen -*- lexical-binding: t; -*-

;;; Commentary:
;; One script for every viewer, recorded from the X display by the review
;; harness (Review.lean screenbench): open a document, jump to pages 5, 12 and
;; 3, then scroll in thirty reading steps.  Each action's start is logged; the
;; harness finds on the recording when the page area first changes after it
;; and when it last changes before the next.  Viewers drawing outside Emacs'
;; display engine (EAF's Qt window) are measured exactly like those inside.
;; Desktop readers (Papers, Evince, Okular) get the same reading as X input
;; from the harness; with --keys, so do the viewers in Emacs, after
;; `screen-bench-open'.

;;; Code:
(require 'cl-lib)

(defvar screen-bench--marks nil "(LABEL . TIME) of each action, newest first.")
(defvar eaf--buffer-id)
(defvar pdf-info-current-backend)
(defvar pdf-view-display-size)
(defvar pdf-tools-enabled-modes)
(defvar doc-view-continuous)
(declare-function eaf-open "eaf" (url &optional app-name args always-new))
(declare-function eaf-call-async "eaf" (method &rest args))
(declare-function eaf-call-sync "eaf" (method &rest args))

(defun screen-bench--pause (seconds)
  "Let SECONDS pass with redisplay and process output, as an idle Emacs would."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end) (sit-for 0.005))))

(defun screen-bench-load-doc-tools (paths)
  "Load doc-tools from PATHS, \"TOOLS:MUPDF:POPPLER\" directories: its MuPDF
backend without the loads of its author's own paths."
  (pcase-let ((`(,tools ,mupdf ,poppler) (split-string paths ":")))
    (load (expand-file-name "doc-mupdf.el" mupdf) nil t)
    (load (expand-file-name "doc-poppler.el" poppler) nil t)
    (load (expand-file-name "doc-scroll.el" tools) nil t)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "doc-backend-mupdf.el" tools))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (unless (memq (car-safe form) '(load load-file)) (eval form t))))
        (end-of-file nil)))))

(defun screen-bench-load-vimura (paths tablist home)
  "Load pdf-tools from its vimura fork with the vimura server: PATHS is
\"FORK-LISP:PYTHON-BIN:USER-BASE:IMAGE-ROLL\", TABLIST tablist's directory.  The server
writes vimura.log and vimura_err.log into ~, so HOME becomes HOME."
  (pcase-let ((`(,lisp ,python ,base ,image-roll) (split-string paths ":")))
    (setenv "PYTHONUSERBASE" base)
    ;; Nix's Python setup hooks turn the user site off and add their own path.
    (setenv "PYTHONNOUSERSITE" nil)
    (setenv "PYTHONPATH" nil)
    (setenv "PATH" (concat python ":" (getenv "PATH")))
    (push python exec-path)
    (make-directory home t)
    (setenv "HOME" home)
    (add-to-list 'load-path lisp)
    (add-to-list 'load-path tablist)
    (add-to-list 'load-path image-roll)
    ;; Its source has no autoloads file; a package install would make one.
    (let ((autoloads (expand-file-name "pdf-tools-autoloads.el" home)))
      (loaddefs-generate lisp autoloads)
      (load autoloads nil t))
    (require 'pdf-tools)
    ;; Its pdf-annot.el (2022) does not load in Emacs 31 (a misplaced t clause,
    ;; fixed upstream since); reading needs no annotations.
    (setq pdf-tools-enabled-modes (remq 'pdf-annot-minor-mode pdf-tools-enabled-modes))
    (setq pdf-info-current-backend 'vimura pdf-view-display-size 'fit-width)
    (pdf-tools-install-noverify)))

(defun screen-bench--ops (viewer)
  "How VIEWER opens a document, goes to a page and takes a reading step."
  (let ((lines (lambda (pixels) (max 1 (round pixels (frame-char-height))))))
    (pcase viewer
      ('vellumory
       (list :open (lambda (file) (switch-to-buffer (vellumory-open file t)))
             :jump (lambda (page) (vellumory-goto-page page))
             :step (lambda () (vellumory-scroll 96))))
      ((or 'pdf-tools 'pdf-tools-roll 'pdf-tools-vimura)
       (list :open (lambda (file)
                     (switch-to-buffer
                      (let ((auto-mode-alist (cons '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist))
                            (magic-mode-alist nil))
                        (find-file-noselect file)))
                     (when (eq viewer 'pdf-tools-roll) (pdf-view-roll-minor-mode 1))
                     (pdf-view-fit-width-to-window))
             :jump (lambda (page) (pdf-view-goto-page page))
             :step (if (eq viewer 'pdf-tools-roll)
                       (lambda () (pdf-roll-scroll-forward 96 nil t))
                     (lambda () (pdf-view-next-line-or-next-page (funcall lines 96))))))
      ('doc-view
       ;; Continuous, so that reading steps go on to the next page (an option
       ;; read as each step runs, not only while the file opens).
       (setq doc-view-continuous t)
       (list :open (lambda (file)
                     (switch-to-buffer
                      (let ((auto-mode-alist (cons '("\\.pdf\\'" . doc-view-mode) auto-mode-alist))
                            (magic-mode-alist nil))
                        (find-file-noselect file))))
             :jump (lambda (page) (doc-view-goto-page page))
             :step (lambda () (doc-view-next-line-or-next-page (funcall lines 96)))))
      ('reader
       (list :open (lambda (file)
                     (switch-to-buffer
                      (let ((auto-mode-alist (cons '("\\.pdf\\'" . reader-mode) auto-mode-alist))
                            (magic-mode-alist nil))
                        (find-file-noselect file)))
                     (reader-fit-to-width))
             :jump (lambda (page) (reader-goto-page page))
             :step (lambda () (reader--non-queue-scroll-down-or-next-page (funcall lines 96)))))
      ('doc-tools
       (list :open (lambda (file) (switch-to-buffer (find-file-noselect file)))
             :jump (lambda (page) (doc-scroll-goto-page page))
             :step (lambda () (doc-scroll-scroll-forward))))
      ('eaf
       ;; Its commands go to the Python process with the EAF buffer's id.
       (cl-flet ((in-eaf (fn) (when-let* ((buffer (seq-find (lambda (b) (with-current-buffer b (derived-mode-p 'eaf-mode)))
                                                          (buffer-list))))
                               (with-current-buffer buffer (funcall fn)))))
         (list :open (lambda (file) (eaf-open file))
               :jump (lambda (page)
                       (in-eaf (lambda () (eaf-call-async "execute_function_with_args" eaf--buffer-id
                                                          "jump_to_page_with_num" (format "%s" page)))))
               ;; As its j key does.
               :step (lambda () (in-eaf (lambda () (eaf-call-async "eval_function" eaf--buffer-id "scroll_up" "j"))))))))))

(defun screen-bench--start ()
  "Begin a reading: one window on an empty buffer, then a second's rest."
  (setq screen-bench--marks nil)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*screen-bench*"))
  (redisplay t)
  (screen-bench--pause 1))

(defun screen-bench--mark (label)
  "Log LABEL's start now.
With the garbage collections so far: a pause in a step may be one."
  (push (cons (format "%s (gc %d)" label gcs-done) (float-time)) screen-bench--marks))

(defun screen-bench--write (log)
  "Write the actions' times to LOG."
  (with-temp-file log
    (dolist (mark (reverse screen-bench--marks))
      (insert (format "%s\t%.6f\n" (car mark) (cdr mark))))))

(defun screen-bench-run (viewer file log)
  "Read FILE in VIEWER as the script says; write the actions' times to LOG."
  (screen-bench--start)
  (let ((ops (screen-bench--ops viewer)))
    (cl-flet ((act (label fn) (screen-bench--mark label) (funcall fn)))
      (act "open" (lambda () (funcall (plist-get ops :open) file)))
      (screen-bench--pause 4)
      (dolist (page '(5 12 3))
        (act (format "jump %d" page) (lambda () (funcall (plist-get ops :jump) page)))
        (screen-bench--pause 2))
      (dotimes (_ 30)
        (act "step" (plist-get ops :step))
        (screen-bench--pause 0.3))))
  (screen-bench--write log)
  (length screen-bench--marks))

(defun screen-bench-open (viewer file log)
  "Open FILE in VIEWER as `screen-bench-run' does; write its time to LOG.
The rest of the reading comes as X input from the harness
\(Review.lean screenbench --keys), as for the desktop readers it is
compared with: the viewer's page prompt on M-g g, Down and the wheel."
  ;; The pointer rests on the page for the wheel steps: help goes to the
  ;; echo area, never into a tooltip over the recorded region.
  (tooltip-mode -1)
  (screen-bench--start)
  (screen-bench--mark "open")
  (funcall (plist-get (screen-bench--ops viewer) :open) file)
  (screen-bench--write log)
  t)

(provide 'screen-bench)
;;; screen-bench.el ends here
