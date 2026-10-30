;;; vellumory-evil.el --- Evil integration for Vellumory -*- lexical-binding: t; -*-

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
;; Normal-state bindings modelled on evil-collection's pdf keys (j/k, C-d/C-u,
;; gg/G with counts, zi/zo, / ? n N, o, y, C-o/C-i, m and ' registers, q, ZZ),
;; plus Vellumory's own commands.  Mouse events are rebound in the state map:
;; evil's global `down-mouse-1' (region dragging) would otherwise shadow text
;; selection, since evil state maps take precedence over major-mode maps.
;; Call `vellumory-evil-setup' after evil has loaded.

;;; Code:
(require 'vellumory)
(require 'register)

(defvar evil-normal-state-cursor)
(defvar evil-motion-state-cursor)
(defvar evil-visual-state-cursor)
(defvar vellumory-annotate-map)
(defvar vellumory-caret-mode-map)
(defvar vellumory-evil--views)
(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(declare-function vellumory--row-width "vellumory" ())
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))

(defun vellumory-evil-goto-first-page (&optional count)
  "Go to page COUNT, or to the first page."
  (interactive "P")
  (vellumory-goto-page (if count (prefix-numeric-value count) 1)))

(defun vellumory-evil-goto-last-page (&optional count)
  "Go to page COUNT, or to the last page."
  (interactive "P")
  (vellumory-goto-page (if count (prefix-numeric-value count) (length vellumory--pages))))

(defun vellumory-evil-scroll-line-down (&optional count)
  "Scroll COUNT lines forward." (interactive "p")
  (vellumory-scroll (* (or count 1) (frame-char-height))))

(defun vellumory-evil-scroll-line-up (&optional count)
  "Scroll COUNT lines backward." (interactive "p")
  (vellumory-scroll (- (* (or count 1) (frame-char-height)))))

(defun vellumory-evil-search-backward ()
  "Find text, then visit matches backward from here."
  (interactive)
  (vellumory-search)
  (unless (string-empty-p vellumory--search-query) (vellumory-search-previous)))

(defun vellumory-evil-hscroll-start ()
  "Show the left edge of the page." (interactive)
  (set-window-hscroll nil 0) (vellumory--refresh))

(defun vellumory-evil-hscroll-end ()
  "Show the right edge of the page." (interactive)
  (set-window-hscroll nil (max 0 (ceiling (- (+ (vellumory--row-width) vellumory--margin) (window-body-width nil t))
                                           (frame-char-width))))
  (vellumory--refresh))

;;;; Registers: positions that `jump-to-register' and evil marks understand

(cl-defstruct (vellumory-register (:constructor vellumory-register-create)) buffer position)

(cl-defmethod register-val-jump-to ((value vellumory-register) _arg)
  "Show the document of VALUE at its stored position."
  (let ((buffer (vellumory-register-buffer value)))
    (unless (buffer-live-p buffer) (user-error "That document is no longer open"))
    (pop-to-buffer-same-window buffer)
    (vellumory--remember-position)
    (vellumory--restore-position (vellumory-register-position value))
    (vellumory--refresh t)))

(cl-defmethod register-val-describe ((value vellumory-register) _verbose)
  "Print the buffer and page of the position in VALUE."
  (princ (format "%s, page %d" (buffer-name (vellumory-register-buffer value))
                 (1+ (alist-get 'page (vellumory-register-position value))))))

(defun vellumory-point-to-register (register)
  "Store the exact reading position in REGISTER."
  (interactive (list (register-read-with-preview "Position to register: ")))
  (set-register register (vellumory-register-create :buffer (current-buffer)
                                                    :position (vellumory--reading-position))))

(defun vellumory-jump-to-register (register)
  "Jump to the position in REGISTER."
  (interactive (list (register-read-with-preview "Jump to register: ")))
  (jump-to-register register))

;;;; Setup

(defun vellumory-evil--buffer-setup ()
  "Adapt evil to this document buffer.
Hide evil's state cursors and keep an active mark from entering visual
state."
  ;; A box cursor over page images only flickers.
  (setq-local evil-normal-state-cursor (list nil)
              evil-motion-state-cursor (list nil)
              evil-visual-state-cursor (list nil))
  ;; Mouse selection is Vellumory's own; it must not start evil visual state.
  (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t))

;;;###autoload
(defun vellumory-evil-setup ()
  "Bind Vellumory commands in evil's normal state."
  (evil-set-initial-state 'vellumory-mode 'normal)
  (evil-set-initial-state 'vellumory-outline-mode 'normal)
  (evil-set-initial-state 'vellumory-thumbnails-mode 'normal)
  ;; Presenting takes every key itself: Space, digits, b and w mean pages and screens.
  (evil-set-initial-state 'vellumory-presentation-mode 'emacs)
  (evil-define-key* 'normal vellumory-mode-map
    "j" #'vellumory-scroll-down "k" #'vellumory-scroll-up
    (kbd "<down>") #'vellumory-scroll-down (kbd "<up>") #'vellumory-scroll-up
    (kbd "C-e") #'vellumory-evil-scroll-line-down (kbd "C-y") #'vellumory-evil-scroll-line-up
    (kbd "C-d") #'vellumory-half-screen-down (kbd "C-u") #'vellumory-half-screen-up
    (kbd "C-f") #'vellumory-next-screen (kbd "C-b") #'vellumory-previous-screen
    (kbd "SPC") #'vellumory-next-screen (kbd "S-SPC") #'vellumory-previous-screen
    (kbd "DEL") #'vellumory-previous-screen
    (kbd "<next>") #'vellumory-next-screen (kbd "<prior>") #'vellumory-previous-screen
    "J" #'vellumory-next-page "K" #'vellumory-previous-page
    "gj" #'vellumory-next-page "gk" #'vellumory-previous-page
    "]]" #'vellumory-next-page "[[" #'vellumory-previous-page
    "gg" #'vellumory-evil-goto-first-page "G" #'vellumory-evil-goto-last-page
    "gl" #'vellumory-goto-page-label "gp" #'vellumory-goto-page
    "h" #'vellumory-scroll-left "l" #'vellumory-scroll-right
    (kbd "<left>") #'vellumory-scroll-left (kbd "<right>") #'vellumory-scroll-right
    "0" #'vellumory-recenter "^" #'vellumory-evil-hscroll-start
    "$" #'vellumory-evil-hscroll-end
    "zi" #'vellumory-zoom-in "zo" #'vellumory-zoom-out "z0" #'vellumory-fit-width
    "+" #'vellumory-zoom-in "=" #'vellumory-zoom-in "-" #'vellumory-zoom-out
    "zw" #'vellumory-fit-width "W" #'vellumory-fit-width
    "zp" #'vellumory-fit-page "P" #'vellumory-fit-page "H" #'vellumory-fit-page
    "zc" #'vellumory-fit-content-width
    "zd" #'vellumory-night-mode "zm" #'vellumory-cycle-theme "zs" #'vellumory-cycle-spread
    "gP" #'vellumory-present "g?" #'vellumory-dispatch
    "v" #'vellumory-visual
    "/" #'vellumory-search "?" #'vellumory-evil-search-backward
    "n" #'vellumory-search-next "N" #'vellumory-search-previous
    "f" #'vellumory-follow-link "F" #'vellumory-follow-link
    "o" #'vellumory-outline "O" #'vellumory-goto-heading "s" #'vellumory-sidebar-switch
    "t" #'vellumory-thumbnails "T" #'vellumory-page-text "I" #'vellumory-details "V" #'vellumory-validate "gS" #'vellumory-syntax "D" #'vellumory-diff
    "y" #'vellumory-copy-selection
    "m" #'vellumory-point-to-register "'" #'vellumory-jump-to-register
    "`" #'vellumory-jump-to-register
    (kbd "C-o") #'vellumory-history-back (kbd "C-i") #'vellumory-history-forward
    (kbd "<tab>") #'vellumory-history-forward
    "u" #'vellumory-undo (kbd "C-r") #'vellumory-redo "U" #'vellumory-history
    "e" vellumory-edit-map "a" vellumory-annotate-map
    "gr" #'vellumory-reload "r" #'vellumory-reload
    "q" #'quit-window "ZZ" #'quit-window "ZQ" #'kill-current-buffer "Q" #'kill-current-buffer
    [down-mouse-1] #'vellumory-mouse-press [mouse-1] #'ignore [drag-mouse-1] #'ignore
    [double-mouse-1] #'vellumory-mouse-select-word [triple-mouse-1] #'vellumory-mouse-select-line
    [C-down-mouse-1] #'ignore [C-mouse-1] #'vellumory-sync-backward)
  (evil-define-key* 'normal vellumory-outline-mode-map
    (kbd "RET") #'vellumory-outline-activate "go" #'vellumory-outline-activate
    (kbd "TAB") #'outline-cycle (kbd "<backtab>") #'outline-cycle-buffer
    "j" #'outline-next-visible-heading "k" #'outline-previous-visible-heading
    "q" #'quit-window "o" #'quit-window "s" #'vellumory-sidebar-switch)
  (evil-define-key* 'normal vellumory-thumbnails-mode-map
    (kbd "RET") #'vellumory-thumbnail-visit
    "j" #'vellumory-thumbnail-next "k" #'vellumory-thumbnail-previous
    "q" #'quit-window "t" #'quit-window "s" #'vellumory-sidebar-switch
    ;; Marking pages for export, as in the plain map.
    "m" #'vellumory-thumbnail-mark-page "u" #'vellumory-thumbnail-unmark-page
    "U" #'vellumory-thumbnail-unmark-all "x" #'vellumory-thumbnail-export-marked)
  ;; The caret's keys outrank the reader's normal-state ones (j k h l scroll there).
  (let (bindings)
    (map-keymap (lambda (event command) (push (vector event) bindings) (push command bindings))
                vellumory-caret-mode-map)
    (apply #'evil-define-key* 'normal vellumory-caret-mode-map (nreverse bindings)))
  (add-hook 'vellumory-caret-mode-hook #'evil-normalize-keymaps)
  (add-hook 'vellumory-mode-hook #'vellumory-evil--buffer-setup)
  ;; Each view's own keys outrank evil's normal state once its code loads.
  (pcase-dolist (`(,feature . ,map) vellumory-evil--views)
    (with-eval-after-load feature (vellumory-evil--view-keys (symbol-value map))))
  (evil-set-initial-state 'vellumory-reflow-mode 'normal)
  (evil-define-key* 'normal vellumory-mode-map "zr" #'vellumory-reflow)
  (with-eval-after-load 'vellumory-reflow (vellumory-evil--reflow-keys)))

(defvar vellumory-reflow-mode-map)
(declare-function vellumory-reflow-quit "vellumory-reflow" ())
(declare-function vellumory-reflow-menu "vellumory-reflow" ())
(declare-function vellumory-reflow-reset-size "vellumory-reflow" ())
(declare-function vellumory-reflow-cycle-colours "vellumory-reflow" ())
(declare-function vellumory-reflow-outline "vellumory-reflow" ())
(declare-function vellumory-reflow-next-page "vellumory-reflow" (count))
(declare-function vellumory-reflow-previous-page "vellumory-reflow" (count))
(declare-function shr-next-link "shr" ())
(declare-function shr-previous-link "shr" ())

(defun vellumory-evil--reflow-keys ()
  "Reading mode's keys in normal state; motions stay evil's, it being text."
  (evil-define-key* 'normal vellumory-reflow-mode-map
    "q" #'vellumory-reflow-quit "zr" #'vellumory-reflow "g?" #'vellumory-reflow-menu "?" #'vellumory-reflow-menu
    "+" #'text-scale-increase "-" #'text-scale-decrease "=" #'vellumory-reflow-reset-size
    "C" #'vellumory-reflow-cycle-colours "gO" #'vellumory-reflow-outline
    "]]" #'vellumory-reflow-next-page "[[" #'vellumory-reflow-previous-page
    (kbd "TAB") #'shr-next-link (kbd "<backtab>") #'shr-previous-link))

(defconst vellumory-evil--views
  '((vellumory-edit . vellumory-history-mode-map)
    (vellumory-validate . vellumory-validation-mode-map)
    (vellumory-diff . vellumory-diff-mode-map)
    (vellumory-details . vellumory-details-mode-map)
    (vellumory-search . vellumory-occur-mode-map)
    (vellumory-syntax . vellumory-syntax-mode-map)
    (vellumory-dired . vellumory-batch-mode-map)
    (vellumory-annotate . vellumory-annotation-list-mode-map))
  "Views whose keys are their own commands, as (FEATURE . KEYMAP).")

(defun vellumory-evil--view-keys (map)
  "Give MAP's own bindings precedence over evil's normal state in its buffers.
In a view c, d, v, n, p, b and RET are the view's commands (in the history
tree: make current, compare, view), not evil's change, delete, visual state,
search or newline; the syntax view's q closes it while it is read-only."
  (let (bindings)
    (map-keymap-internal (lambda (event command) (push (vector event) bindings) (push command bindings)) map)
    (when bindings (apply #'evil-define-key* 'normal map (nreverse bindings)))))

;; evil-snipe's s, S, f, F, t and T outrank a major mode's evil keys; in a
;; document they mean the sidebar, links and previews, not finding characters.
(defconst vellumory-evil--snipe-free-modes '(vellumory-mode vellumory-thumbnails-mode vellumory-outline-mode))
(with-eval-after-load 'evil-snipe
  (defvar evil-snipe-disabled-modes)
  (dolist (mode vellumory-evil--snipe-free-modes) (add-to-list 'evil-snipe-disabled-modes mode)))

(defvar org-noter-doc-mode-map)
(declare-function org-noter-insert-note "org-noter-core" (&optional toggle-no-questions precise-info))
(declare-function org-noter-insert-note-toggle-no-questions "org-noter-core" ())
(declare-function org-noter-kill-session "org-noter-core" (&optional session))

(defun vellumory-evil-noter-setup ()
  "Give org-noter's document keys precedence in normal state during a session.
A session's minor-mode keymap is consulted before the reader's, but evil's
normal-state bindings for the reader would otherwise shadow `i' and `q'."
  (evil-define-key* 'normal org-noter-doc-mode-map
    "i" #'org-noter-insert-note
    (kbd "C-i") #'org-noter-insert-note-toggle-no-questions
    "q" #'org-noter-kill-session))

;; Only with evil: without it there are no normal-state keys to give way.
(with-eval-after-load 'evil
  (with-eval-after-load 'org-noter-core (vellumory-evil-noter-setup)))

(provide 'vellumory-evil)
;;; vellumory-evil.el ends here
