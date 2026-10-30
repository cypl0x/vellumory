;; -*- no-byte-compile: t; -*-
;;; tools/vellumory/packages.el

;; nix-doom-emacs-unstraightened provides `unstraightened' before reading this
;; file; there Vellumory comes from `extraPackages' (the flake's Lisp output).
;; Otherwise straight.el installs it from GitHub, or from the checkout
;; VELLUMORY_SOURCE names.
(cond ((featurep 'unstraightened)
       (package! vellumory :ignore t))
      ((getenv "VELLUMORY_SOURCE")
       (package! vellumory
         :recipe `(:local-repo ,(getenv "VELLUMORY_SOURCE") :files ("lisp/*.el"))))
      (t
       (package! vellumory
         :recipe (:host github :repo "cypl0x/vellumory" :files ("lisp/*.el")))))
