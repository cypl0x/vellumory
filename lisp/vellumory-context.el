;;; vellumory-context.el --- Right-click menus everywhere -*- lexical-binding: t; -*-

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
;; A right click offers what can be done where it lands: on a link, follow
;; or copy it; on a selection, copy, mark or search it; on a page, turn,
;; delete or present it; on a page preview, the same for that page; on a
;; heading of the contents, go there or fold; in the history tree, the
;; validation report and the other views, their own commands for the line
;; clicked.  Every menu ends with the view, panels and the menu of all
;; commands (`vellumory-dispatch').
;;
;; With `context-menu-mode' on, these entries are its menu in Vellumory's
;; buffers (`context-menu-functions'); without it, down-mouse-3 shows them.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar vellumory--selection)
(defvar vellumory--pages)
(defvar vellumory--thumbnail-source)
(defvar vellumory--marked-pages)
(declare-function vellumory--export "vellumory-edit" (pages output))
(declare-function vellumory--export-default "vellumory-edit" (pages))
(declare-function vellumory-mark-page "vellumory-edit" (&optional page))
(declare-function vellumory-unmark-all "vellumory-edit" ())
(declare-function vellumory-export-marked-pages "vellumory-edit" (output))
(declare-function vellumory--event-location "vellumory-select" (posn))
(declare-function vellumory--link-at "vellumory-select" (page x y))
(declare-function vellumory--visit-link "vellumory-select" (link))
(declare-function vellumory--edit "vellumory-edit" (operation))
(declare-function vellumory--search-begin "vellumory-search" (query sensitive origin &optional regexp))
(declare-function vellumory-goto-page "vellumory" (page))
(declare-function vellumory-present "vellumory-present" (&optional from-start))
(declare-function outline-hide-sublevels "outline" (levels))
(declare-function outline-cycle "outline" (&optional event))
(declare-function outline-show-all "outline" ())
(declare-function vellumory-outline-activate "vellumory-navigation" ())
(declare-function vellumory-sidebar-switch "vellumory-navigation" (&optional event))
(declare-function vellumory-outline "vellumory-navigation" ())
(declare-function vellumory-thumbnails "vellumory-thumbnails" ())
(declare-function vellumory-add-note "vellumory-annotate" (location contents))
(declare-function vellumory-highlight "vellumory-annotate" (&optional comment))
(declare-function vellumory-underline "vellumory-annotate" (&optional comment))
(declare-function vellumory-strike-out "vellumory-annotate" (&optional comment))
(declare-function vellumory-squiggly "vellumory-annotate" (&optional comment))
(declare-function vellumory-copy-selection "vellumory-select" ())
(declare-function vellumory-undo "vellumory-edit" ())
(declare-function vellumory-redo "vellumory-edit" ())
(declare-function vellumory-history "vellumory-edit" ())
(declare-function vellumory-syntax "vellumory-syntax" ())
(declare-function vellumory-validate "vellumory-validate" (flavour))
(declare-function vellumory-details "vellumory-details" ())
(declare-function vellumory-dispatch "vellumory-dispatch" ())
(declare-function vellumory-zoom-in "vellumory" ())
(declare-function vellumory-zoom-out "vellumory" ())
(declare-function vellumory-fit-width "vellumory" ())
(declare-function vellumory-fit-page "vellumory" ())
(declare-function vellumory-fit-content-width "vellumory" ())
(declare-function vellumory-cycle-spread "vellumory" ())
(declare-function vellumory-reflow "vellumory-reflow" (&optional window automatic))
(declare-function vellumory-cycle-theme "vellumory" (&optional choose))
(declare-function vellumory-set-theme "vellumory" (theme))
(declare-function vellumory--this-document "vellumory" ())
(defvar vellumory--themes)
(declare-function vellumory-night-mode "vellumory" (&optional arg))

(defconst vellumory-context--maps
  '((vellumory . vellumory-mode-map)
    (vellumory-thumbnails . vellumory-thumbnails-mode-map)
    (vellumory-navigation . vellumory-outline-mode-map)
    (vellumory-edit . vellumory-history-mode-map)
    (vellumory-validate . vellumory-validation-mode-map)
    (vellumory-diff . vellumory-diff-mode-map)
    (vellumory-details . vellumory-details-mode-map)
    (vellumory-search . vellumory-occur-mode-map)
    (vellumory-annotate . vellumory-annotation-list-mode-map)
    (vellumory-dired . vellumory-batch-mode-map)
    (vellumory-reflow . vellumory-reflow-mode-map))
  "Buffers with a right-click menu, as (FEATURE . KEYMAP) of their modes.")

(defvar vellumory-context--count 0 "Entries made, for unique menu keys.")

(defun vellumory-context--item (menu label command &optional enable)
  "Add LABEL running COMMAND to MENU, active while ENABLE (a form) holds."
  (define-key-after menu (vector (intern (format "vellumory-%d" (cl-incf vellumory-context--count))))
    `(menu-item ,label ,command ,@(and enable (list :enable enable)))))

(defun vellumory-context--title (menu label)
  "Add LABEL to MENU as an inactive heading."
  (define-key-after menu (vector (intern (format "vellumory-%d" (cl-incf vellumory-context--count))))
    `(menu-item ,label ignore :enable nil :keys "")))

(defun vellumory-context--separator (menu)
  "Add a separator line to MENU."
  (define-key-after menu (vector (intern (format "vellumory-sep-%d" (cl-incf vellumory-context--count))))
    '(menu-item "--")))

(defun vellumory-context--selection-text ()
  "The selected text, its pages joined by newlines."
  (mapconcat (lambda (selection) (nth 1 selection)) vellumory--selection "\n"))

(defun vellumory-context--page-edit (source operation)
  "A command applying OPERATION (an alist) to SOURCE's document."
  (lambda () (interactive) (with-current-buffer source (vellumory--edit operation))))

(defun vellumory-context--page-items (menu source page)
  "Add to MENU what can be done with zero-based PAGE of SOURCE's document."
  (let ((number (1+ page)))
    (vellumory-context--title menu (format "Page %d" number))
    (vellumory-context--item menu "Rotate clockwise"
                             (vellumory-context--page-edit source `((op . "rotate") (pages . [,number]) (degrees . 90))))
    (vellumory-context--item menu "Rotate counterclockwise"
                             (vellumory-context--page-edit source `((op . "rotate") (pages . [,number]) (degrees . -90))))
    (let ((kept (with-current-buffer source
                  (vconcat (cl-loop for p from 1 to (length vellumory--pages) unless (= p number) collect p)))))
      (vellumory-context--item menu "Delete this page"
                               (vellumory-context--page-edit source `((op . "select") (pages . ,kept)))
                               (> (length kept) 0)))
    (vellumory-context--item menu "Present from here"
                             (lambda () (interactive)
                               (with-current-buffer source (vellumory-goto-page number) (vellumory-present))))
    (vellumory-context--separator menu)
    (vellumory-context--item menu "Export this page…"
                             (lambda () (interactive)
                               (with-current-buffer source
                                 (vellumory--export
                                  (list number)
                                  (read-file-name (format "Export page %d to: " number) nil nil nil
                                                  (file-name-nondirectory (vellumory--export-default (list number))))))))
    (let ((marked (buffer-local-value 'vellumory--marked-pages source)))
      (vellumory-context--item menu (if (memq number marked) "Unmark for export" "Mark for export")
                               (lambda () (interactive) (with-current-buffer source (vellumory-mark-page number))))
      (when marked
        (vellumory-context--item menu (format "Export the marked pages (%d)…" (length marked))
                                 (lambda () (interactive)
                                   (with-current-buffer source (call-interactively #'vellumory-export-marked-pages))))
        (vellumory-context--item menu "Clear the marks"
                                 (lambda () (interactive) (with-current-buffer source (vellumory-unmark-all))))))))

(defun vellumory-context--colours (menu)
  "Add to MENU a submenu choosing the page colours of this document."
  (when-let* ((document (ignore-errors (vellumory--this-document))))
    (let ((colours (make-sparse-keymap "Page colours"))
          (current (buffer-local-value 'vellumory-theme document)))
      (pcase-dolist (`(,theme . ,name) vellumory--themes)
        (define-key-after colours (vector (intern (format "vellumory-%d" (cl-incf vellumory-context--count))))
          `(menu-item ,(capitalize name) ,(lambda () (interactive) (vellumory-set-theme theme))
                      :button (:radio . ,(eq theme current)))))
      (vellumory-context--separator colours)
      (define-key-after colours [vellumory-night]
        `(menu-item "Night mode" vellumory-night-mode
                    :button (:toggle . ,(buffer-local-value 'vellumory-night-mode document))))
      (define-key-after menu (vector (intern (format "vellumory-%d" (cl-incf vellumory-context--count))))
        (cons "Page colours" colours)))))

(defun vellumory-context--common (menu)
  "Add the view, the panels and the menu of all commands to MENU."
  (vellumory-context--separator menu)
  (vellumory-context--item menu "Zoom in" #'vellumory-zoom-in)
  (vellumory-context--item menu "Zoom out" #'vellumory-zoom-out)
  (vellumory-context--item menu "Fit width" #'vellumory-fit-width)
  (vellumory-context--item menu "Fit page" #'vellumory-fit-page)
  (vellumory-context--item menu "Fit the content" #'vellumory-fit-content-width)
  (vellumory-context--item menu "Two pages" #'vellumory-cycle-spread)
  (vellumory-context--item menu "Reading mode (reflowed)" #'vellumory-reflow)
  (vellumory-context--colours menu)
  (vellumory-context--separator menu)
  (vellumory-context--item menu "Page previews" #'vellumory-thumbnails)
  (vellumory-context--item menu "Contents" #'vellumory-outline)
  (vellumory-context--item menu "Details" #'vellumory-details)
  (vellumory-context--item menu "Validate…" #'vellumory-validate)
  (vellumory-context--item menu "PDF syntax" #'vellumory-syntax)
  (vellumory-context--item menu "Edit history" #'vellumory-history)
  (vellumory-context--separator menu)
  (vellumory-context--item menu "Undo" #'vellumory-undo)
  (vellumory-context--item menu "Redo" #'vellumory-redo)
  (vellumory-context--item menu "All commands…" #'vellumory-dispatch))

(defun vellumory-context--reader (menu click)
  "Fill MENU for CLICK on a document's page."
  (let* ((location (vellumory--event-location (event-start click)))
         (link (and location (apply #'vellumory--link-at location))))
    (when link
      (vellumory-context--item menu "Follow the link" (lambda () (interactive) (vellumory--visit-link link)))
      (when-let* ((uri (alist-get 'uri link)))
        (vellumory-context--item menu "Copy the address"
                                 (lambda () (interactive) (kill-new uri) (message "Copied %s" uri))))
      (vellumory-context--separator menu))
    (when vellumory--selection
      (let ((text (vellumory-context--selection-text)))
        (vellumory-context--item menu "Copy" #'vellumory-copy-selection)
        (vellumory-context--item menu "Highlight" #'vellumory-highlight)
        (vellumory-context--item menu "Underline" #'vellumory-underline)
        (vellumory-context--item menu "Strike out" #'vellumory-strike-out)
        (vellumory-context--item menu "Squiggly underline" #'vellumory-squiggly)
        (vellumory-context--item menu "Add a note…" #'vellumory-add-note)
        (vellumory-context--item menu (format "Search for “%s”" (truncate-string-to-width text 30 nil nil "…"))
                                 (lambda () (interactive) (vellumory--search-begin text nil nil))))
      (vellumory-context--separator menu))
    (when location (vellumory-context--page-items menu (current-buffer) (car location)))
    (vellumory-context--common menu)))

(defun vellumory-context--thumbnails (menu click)
  "Fill MENU for CLICK on a page preview."
  (let* ((pos (posn-point (event-start click)))
         (page (and (integer-or-marker-p pos) (get-text-property pos 'vellumory-thumbnail-page)))
         (source vellumory--thumbnail-source))
    (when (and page (buffer-live-p source))
      (vellumory-context--item menu (format "Go to page %d" (1+ page))
                               (lambda () (interactive) (with-current-buffer source (vellumory-goto-page (1+ page)))))
      (vellumory-context--page-items menu source page)
      (vellumory-context--separator menu))
    (vellumory-context--item menu "Contents instead" #'vellumory-sidebar-switch)
    (vellumory-context--colours menu)
    (vellumory-context--item menu "Close the sidebar" #'quit-window)))

(defun vellumory-context--outline (menu click)
  "Fill MENU for CLICK on the contents."
  (vellumory-context--point click)
  (vellumory-context--item menu "Go to this heading" #'vellumory-outline-activate)
  (vellumory-context--item menu "Fold or unfold" #'outline-cycle)
  (vellumory-context--item menu "Show everything" #'outline-show-all)
  (vellumory-context--item menu "Fold everything" (lambda () (interactive) (outline-hide-sublevels 1)))
  (vellumory-context--separator menu)
  (vellumory-context--item menu "Page previews instead" #'vellumory-sidebar-switch)
  (vellumory-context--item menu "Close the sidebar" #'quit-window))

(defun vellumory-context--point (click)
  "Move point where CLICK landed, when that was in this buffer's window."
  (let ((window (posn-window (event-start click))))
    (when (and (windowp window) (eq (window-buffer window) (current-buffer)))
      (mouse-set-point click))))

(defun vellumory-context--label (command)
  "A menu label for COMMAND: the first sentence of its documentation."
  (let ((doc (or (ignore-errors (documentation command t)) (symbol-name command))))
    (string-trim-right (car (split-string doc "\n")) "\\.")))

(defun vellumory-context--view (menu click)
  "Fill MENU for CLICK in a view: its own commands, for the line clicked."
  (vellumory-context--point click)
  (map-keymap-internal
   (lambda (_event command)
     (when (and (symbolp command) (commandp command) (string-prefix-p "vellumory" (symbol-name command))
                (not (lookup-key menu (vector command))))
       (define-key-after menu (vector command) `(menu-item ,(vellumory-context--label command) ,command))))
   (current-local-map))
  (vellumory-context--separator menu)
  (vellumory-context--item menu "Close" #'quit-window))

(defun vellumory--context-menu (menu click)
  "Fill MENU, a keymap, with what can be done where CLICK landed.
For `context-menu-functions'; returns MENU."
  (cond ((derived-mode-p 'vellumory-mode) (vellumory-context--reader menu click))
        ((derived-mode-p 'vellumory-thumbnails-mode) (vellumory-context--thumbnails menu click))
        ((derived-mode-p 'vellumory-outline-mode) (vellumory-context--outline menu click))
        (t (vellumory-context--view menu click)))
  menu)

(defun vellumory-context-menu (event)
  "Show a menu of what can be done where EVENT, a mouse click, landed."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window) (select-window window))
    (popup-menu (vellumory--context-menu (make-sparse-keymap) event) event)))

(defun vellumory-context--setup ()
  "Make this buffer's context menu Vellumory's (for `context-menu-mode')."
  (setq-local context-menu-functions (list #'vellumory--context-menu)))

(defun vellumory-context-install ()
  "Give every Vellumory buffer its right-click menu, as each mode loads."
  (pcase-dolist (`(,feature . ,map) vellumory-context--maps)
    (with-eval-after-load feature
      (let ((keymap (symbol-value map)))
        (keymap-set keymap "<down-mouse-3>" #'vellumory-context-menu)
        ;; The release of the click that opened the menu does nothing.
        (keymap-set keymap "<mouse-3>" #'ignore))
      (add-hook (intern (concat (string-remove-suffix "-map" (symbol-name map)) "-hook"))
                #'vellumory-context--setup))))

(provide 'vellumory-context)
;;; vellumory-context.el ends here
