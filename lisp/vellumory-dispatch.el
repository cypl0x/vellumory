;;; vellumory-dispatch.el --- One menu for every Vellumory command -*- lexical-binding: t; -*-

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
;; `vellumory-dispatch' is a transient menu, in the manner of Magit's
;; dispatch, that reaches every command: moving, finding, viewing, the
;; sidebars, inspecting, annotating, the reversible edits, exporting and
;; the settings.  In a document it is on ?, C-c C-v and (evil) g?; in
;; other buffers it offers what applies there: opening a document, batch
;; work in Dired, exporting Org.  Every menu entry is also an ordinary
;; command with its own key, shown in the menu so the keys can be learnt.

;;; Code:
(require 'transient)
;; Every suffix must be defined or autoloaded when the menu opens.
(require 'vellumory)

(defvar vellumory-smooth-scroll)
(defvar vellumory-header-line)
(defvar vellumory-watch-file)
(defvar vellumory-gc-idle-delay)

(defun vellumory-dispatch--document-p ()
  "Whether the current buffer reads a document."
  (derived-mode-p 'vellumory-mode))

(transient-define-prefix vellumory-dispatch-edit ()
  "Reversible edits of the document; every one is recorded and undoable."
  [["Pages"
    ("r" "Rotate clockwise" vellumory-rotate-pages)
    ("R" "Rotate counterclockwise" vellumory-rotate-pages-counterclockwise)
    ("d" "Delete pages" vellumory-delete-pages)
    ("m" "Move a page" vellumory-move-page)
    ("k" "Keep or reorder pages" vellumory-keep-pages)
    ("a" "Append documents" vellumory-append-documents)
    ("l" "Page labels" vellumory-set-page-labels)]
   ["Document"
    ("i" "Metadata" vellumory-edit-metadata)
    ("A" "Attach a file" vellumory-attach-file)
    ("D" "Remove an attachment" vellumory-remove-attachment)
    ("f" "Fill in the form" vellumory-fill-form)
    ("w" "Stamp or watermark" vellumory-stamp)
    ("o" "Recognize text (OCR)" vellumory-ocr)]
   ["Bytes"
    ("c" "Compress losslessly" vellumory-compress)
    ("L" "Linearize (fast web view)" vellumory-linearize)
    ("E" "Encrypt" vellumory-encrypt)
    ("P" "Remove encryption" vellumory-decrypt)]
   ["History"
    ("u" "Undo" vellumory-undo)
    ("U" "Redo" vellumory-redo)
    ("h" "History tree" vellumory-history)]])

(transient-define-prefix vellumory-dispatch-annotate ()
  "Annotations on the selection, and the list of all of them."
  [["Mark the selection"
    ("h" "Highlight" vellumory-highlight)
    ("u" "Underline" vellumory-underline)
    ("s" "Strike out" vellumory-strike-out)
    ("q" "Squiggly underline" vellumory-squiggly)]
   ["Notes"
    ("n" "Add a note" vellumory-add-note)
    ("l" "List annotations" vellumory-annotations)]])

(transient-define-prefix vellumory-dispatch-export ()
  "Write pages or text out of the document; the document stays as it is."
  [["Pages"
    ("p" "This page…" vellumory-export-page)
    ("x" "Extract pages…" vellumory-extract-pages)
    ("s" "Split" vellumory-split)]
   ["Marked pages"
    ("m" "Mark this page (or unmark)" vellumory-mark-page :transient t)
    ("e" "Export the marked pages…" vellumory-export-marked-pages)
    ("U" "Clear the marks" vellumory-unmark-all :transient t)]
   ["Text"
    ("t" "Export text" vellumory-export-text)
    ("T" "Text of this page" vellumory-page-text)]])

;;;###autoload (autoload 'vellumory-dispatch "vellumory-dispatch" nil t)
(transient-define-prefix vellumory-dispatch-reflow ()
  "Reading mode: the document reflowed to the window, and its settings."
  [["Read"
    ("n" "Next page" vellumory-reflow-next-page)
    ("p" "Previous page" vellumory-reflow-previous-page)
    ("g" "Page…" vellumory-reflow-goto-page)
    ("o" "Contents…" vellumory-reflow-outline)
    ("q" "Back to the pages" vellumory-reflow-quit)]
   ["Text"
    ("+" "Larger" text-scale-increase :transient t)
    ("-" "Smaller" text-scale-decrease :transient t)
    ("0" "Normal size" vellumory-reflow-reset-size :transient t)
    ("{" "Shorter lines" vellumory-reflow-narrower :transient t)
    ("}" "Longer lines" vellumory-reflow-wider :transient t)]
   ["Show"
    ("C" "Cycle the colours" vellumory-reflow-cycle-colours :transient t)
    ("c" "Colours…" vellumory-reflow-set-colours)
    ("I" "Figures as pictures or text" vellumory-reflow-toggle-images :transient t)
    ("H" "Running heads, page numbers" vellumory-reflow-toggle-running-text :transient t)
    ("B" "Where pages begin" vellumory-reflow-toggle-page-breaks :transient t)
    ("," "Customize…" vellumory-reflow-customize)]])

(transient-define-prefix vellumory-dispatch-colours ()
  "Cycle the page colours, or choose them; the page previews follow."
  [[:description (lambda ()
                   (format "Page colours: now %s"
                           (alist-get (buffer-local-value 'vellumory-theme (vellumory--this-document))
                                      vellumory--themes)))
    ("c" "Cycle" vellumory-cycle-theme :transient t)
    ("o" "Original" vellumory-theme-original :transient t)
    ("t" "Theme colours" vellumory-theme-tint :transient t)
    ("i" "Inverted" vellumory-theme-invert :transient t)
    ("a" "Inverted with a dark theme" vellumory-theme-auto :transient t)
    ("n" "Night mode" vellumory-night-mode :transient t)]])

(transient-define-prefix vellumory-dispatch ()
  "Every Vellumory command, grouped; each shows its own key too."
  [:if vellumory-dispatch--document-p
   ["Go"
    ("g" "Page" vellumory-goto-page)
    ("l" "Page label" vellumory-goto-page-label)
    ("h" "Heading" vellumory-goto-heading)
    ("<" "First page" vellumory-first-page)
    (">" "Last page" vellumory-last-page)
    ("[" "Back" vellumory-history-back)
    ("]" "Forward" vellumory-history-forward)]
   ["Find"
    ("s" "Search" vellumory-search)
    ("r" "Search (regexp)" vellumory-search-regexp)
    ("o" "All matches (occur)" vellumory-occur)
    ("S" "Clear the search" vellumory-search-clear)]
   ["View"
    ("+" "Zoom in" vellumory-zoom-in :transient t)
    ("-" "Zoom out" vellumory-zoom-out :transient t)
    ("0" "Fitted, or back to the zoom" vellumory-recenter)
    ("w" "Fit width" vellumory-fit-width)
    ("p" "Fit page" vellumory-fit-page)
    ("c" "Fit the content" vellumory-fit-content-width)
    ("2" "Two pages" vellumory-cycle-spread)
    ("m" "Reading mode (reflowed)" vellumory-reflow)
    ("C" "Page colours…" vellumory-dispatch-colours)
    ("N" "Night mode" vellumory-night-mode :transient t)]
   ["Around"
    ("t" "Page previews" vellumory-thumbnails)
    ("O" "Contents" vellumory-outline)
    ("TAB" "Switch the sidebar" vellumory-sidebar-switch)
    ("P" "Present" vellumory-present)
    ("F" "Follow across windows" vellumory-follow-split)]]
  [:if vellumory-dispatch--document-p
   ["Inspect"
    ("i" "Details" vellumory-details)
    ("v" "Validate (PDF/A, PDF/UA)" vellumory-validate)
    ("d" "PDF syntax" vellumory-syntax)
    ("=" "Compare versions" vellumory-diff)
    ("H" "Edit history" vellumory-history)]
   ["Text"
    ("V" "Select with the keyboard" vellumory-visual)
    ("y" "Copy the selection" vellumory-copy-selection)
    ("f" "Follow a link" vellumory-follow-link)]
   ["More"
    ("e" "Edit (reversible)…" vellumory-dispatch-edit)
    ("a" "Annotate…" vellumory-dispatch-annotate)
    ("x" "Export…" vellumory-dispatch-export)
    ("R" "Reload" vellumory-reload)
    ("M" "The manual" vellumory-info)
    ("!" "Check the setup (doctor)" vellumory-doctor)]
   ["Settings"
    (";s" "Smooth scrolling" vellumory-dispatch--smooth-scroll)
    (";h" "Header line" vellumory-dispatch--header-line)
    (";W" "Reload when the file changes" vellumory-dispatch--watch-file)
    (";l" "Preview links on hover" vellumory-dispatch--link-preview)
    ("," "Customize…" vellumory-dispatch--customize)]]
  [:if-not vellumory-dispatch--document-p
   ["Open"
    ("o" "A document" vellumory-open)
    ("v" "Several as one (virtual)" vellumory-virtual-create)]
   ["Here"
    ("V" "Validate the marked documents" vellumory-dired-validate :if-derived dired-mode)
    ("E" "Edit each marked document" vellumory-dired-edit :if-derived dired-mode)
    ("I" "Summarize the marked documents" vellumory-dired-info :if-derived dired-mode)
    ("M" "Merge them into a new one" vellumory-dired-merge :if-derived dired-mode)
    ("e" "Org to accessible PDF (LaTeX)" vellumory-org-export-pdf :if-derived org-mode)
    ("y" "Org to accessible PDF (Typst)" vellumory-org-export-typst :if-derived org-mode)]
   ["Setup"
    ("i" "Read documents in Vellumory" vellumory-install)
    ("u" "Stop reading them in Vellumory" vellumory-uninstall)
    ("m" "The manual" vellumory-info)
    ("!" "Check the setup (doctor)" vellumory-doctor)
    ("," "Customize…" vellumory-dispatch--customize)]])

(transient-define-infix vellumory-dispatch--link-preview ()
  "Toggle `vellumory-link-preview' for this session."
  :class 'transient-lisp-variable
  :variable 'vellumory-link-preview
  :reader (lambda (&rest _) (not vellumory-link-preview)))

(transient-define-infix vellumory-dispatch--smooth-scroll ()
  "Toggle `vellumory-smooth-scroll' for this session."
  :class 'transient-lisp-variable
  :variable 'vellumory-smooth-scroll
  :reader (lambda (&rest _) (not vellumory-smooth-scroll)))

(transient-define-infix vellumory-dispatch--header-line ()
  "Toggle `vellumory-header-line' for this session."
  :class 'transient-lisp-variable
  :variable 'vellumory-header-line
  :reader (lambda (&rest _) (not vellumory-header-line)))

(transient-define-infix vellumory-dispatch--watch-file ()
  "Toggle `vellumory-watch-file' for this session."
  :class 'transient-lisp-variable
  :variable 'vellumory-watch-file
  :reader (lambda (&rest _) (not vellumory-watch-file)))

(defun vellumory-dispatch--customize ()
  "Show every Vellumory option."
  (interactive)
  (customize-group 'vellumory))

(provide 'vellumory-dispatch)
;;; vellumory-dispatch.el ends here
