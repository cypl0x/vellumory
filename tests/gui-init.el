;;; gui-init.el --- Isolated reader review -*- lexical-binding: t; -*-
(let ((root (getenv "VELLUMORY_REVIEW_ROOT")))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (setq load-prefer-newer t
        ;; Never block an unattended review on prompts: ERT visits test
        ;; sources to report failures, and large corpus files are expected.
        revert-without-query '(".*") large-file-warning-threshold nil)
  (require 'vellumory)
  ;; As the README sets it up: documents visited with find-file open here.
  (vellumory-install)
  (setq vellumory-program (expand-file-name ".lake/build/bin/vellumory" root)
        ;; A run that must start from no saved positions names its own.
        vellumory-state-directory (or (getenv "VELLUMORY_REVIEW_STATE")
                                      (expand-file-name "target/review/state" root)))
  (set-frame-name "Vellumory review")
  (set-frame-size (selected-frame) 110 45)
  (set-face-attribute 'default nil :height 120)
  (tool-bar-mode -1)
  ;; Cocoa can leave the tool bar shown when it is turned off while the first
  ;; frame settles (the macOS runner did, once in five runs): the pixel tests
  ;; read a frame without one.  An unchanged parameter is not set again, so
  ;; it is turned on and off until the frame reports no tool bar.
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (float-time) deadline) (> (or (cdr (alist-get 'tool-bar-size (frame-geometry))) 0) 0))
      (message "The tool bar is still shown: hiding it again")
      (set-frame-parameter nil 'tool-bar-lines 1)
      (redisplay t)
      (set-frame-parameter nil 'tool-bar-lines 0)
      (let ((until (+ (float-time) 0.5)))
        (while (< (float-time) until) (accept-process-output nil 0.05)))
      (redisplay t)))
  (menu-bar-mode 1)
  (vellumory-open (expand-file-name "target/review/reader.pdf" root))
  (delete-other-windows))
