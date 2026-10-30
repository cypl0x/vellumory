;;; init.el -*- lexical-binding: t; -*-
;; Review profile: the parts of the user's Doom configuration that shape how a
;; document buffer behaves (evil, completion, popups, workspaces, modeline), and
;; :lang latex, whose viewer setup Vellumory must take over.
(doom! :completion
       (corfu +orderless)
       (vertico +icons)
       :ui
       doom
       hl-todo
       modeline
       (popup +defaults)
       workspaces
       :editor
       (evil +everywhere)
       :emacs
       dired
       undo
       :tools
       vellumory
       :lang
       emacs-lisp
       latex
       (org +noter)
       :config
       (default +bindings +smartparens))
