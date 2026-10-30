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
  (menu-bar-mode 1)
  (vellumory-open (expand-file-name "target/review/reader.pdf" root))
  (delete-other-windows))
