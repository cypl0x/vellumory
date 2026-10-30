;;; vellumory-navigation.el --- PDF contents and labels -*- lexical-binding: t; -*-

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
;; The document's contents (outline) and page labels, loaded from the worker
;; for each snapshot.  Pages are named by their labels, with the physical
;; number where the two differ.  `vellumory-goto-heading' and imenu jump to a
;; contents entry, `vellumory-goto-page-label' to a page by its label.
;; `vellumory-outline' toggles a foldable contents sidebar.  The sidebar shows
;; either the page previews or the contents; `s' or a click on its header
;; tabs switches between them.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'button)
(require 'imenu)
(require 'browse-url)

(defvar vellumory--file)
(defvar vellumory--hash)
(defvar vellumory--pages)
(declare-function vellumory--rpc "vellumory" (method params callback))
(declare-function vellumory--restore-position "vellumory" (position))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory-goto-page "vellumory" (page))
(declare-function vellumory--defer-gc "vellumory" ())
(declare-function vellumory--reset-thumbnails "vellumory-thumbnails" ())
(declare-function vellumory--show-thumbnails "vellumory-thumbnails" ())
(defvar vellumory--thumbnail-buffer)
(defvar vellumory--thumbnail-source)

(defvar-local vellumory--outline [])
(defvar-local vellumory--labels [])
(defvar-local vellumory--navigation-state 'loading)
(defvar-local vellumory--outline-buffer nil)
(defvar-local vellumory--outline-source nil)

(defun vellumory--page-description (page)
  "Describe zero-based PAGE using its PDF label and physical number."
  (let* ((number (number-to-string (1+ page)))
         (label (if (< page (length vellumory--labels)) (aref vellumory--labels page) number)))
    (if (or (equal label "") (equal label number)) number (format "%s (%s)" label number))))

(defun vellumory--load-navigation ()
  "Load navigation asynchronously for the current immutable document."
  (setq vellumory--navigation-state 'loading vellumory--outline [] vellumory--labels []
        imenu--index-alist nil)
  (vellumory--update-outline-buffer)
  (let ((hash vellumory--hash))
    (vellumory--rpc
     "navigation" nil
     (lambda (reply)
       (when (equal hash vellumory--hash)
         (let ((result (alist-get 'result reply)))
           (cond
            ((alist-get 'error reply)
             (setq vellumory--navigation-state (alist-get 'error reply)))
            ((equal hash (alist-get 'hash result))
             (setq vellumory--outline (alist-get 'outline result)
                   vellumory--labels (alist-get 'labels result)
                   vellumory--navigation-state 'ready)
             ;; Pages are images: only right-to-left page labels, shown in
             ;; the header and mode line, need bidirectional reordering.
             (setq-local bidi-display-reordering
                         (and (seq-some (lambda (label) (string-match-p "\\cR" label)) vellumory--labels) t))))
           (setq imenu--index-alist nil)
           (vellumory--update-outline-buffer)
           (vellumory--reset-thumbnails)
           (force-mode-line-update)))))))

(defun vellumory--navigation-ready ()
  "Signal a user error unless the contents and page labels are loaded."
  (unless (eq vellumory--navigation-state 'ready)
    (user-error "%s" (if (eq vellumory--navigation-state 'loading)
                         "Document contents are loading" vellumory--navigation-state))))

(defun vellumory--outline-title (entry)
  "Return the title of contents ENTRY, or \"(Untitled)\" when it is empty.
Control characters become spaces, so each heading occupies one line."
  (let ((title (replace-regexp-in-string "[[:cntrl:]]" " " (alist-get 'title entry))))
    (if (string-empty-p title) "(Untitled)" title)))

(defun vellumory--outline-description (entry index)
  "Return the completion candidate for contents ENTRY, the INDEXth entry.
It is the title indented by depth, the page when ENTRY has one, and
the one-based entry number."
  (let ((page (alist-get 'page entry)))
    (format "%s%s%s  [%d]"
            (make-string (* 2 (alist-get 'depth entry)) ?\s)
            (vellumory--outline-title entry)
            (if page (concat " · " (vellumory--page-description page)) "")
            (1+ index))))

(defun vellumory--follow-outline (source hash entry)
  "Follow ENTRY only if SOURCE still displays the snapshot identified by HASH."
  (unless (buffer-live-p source) (user-error "The PDF buffer has been closed"))
  (with-current-buffer source
    (unless (equal hash vellumory--hash) (user-error "The PDF changed; choose an updated contents entry"))
    (let ((page (alist-get 'page entry)) (uri (alist-get 'uri entry)))
      (cond
       ((and (natnump page) (< page (length vellumory--pages)))
        (pop-to-buffer source)
        (vellumory--restore-position `((page . ,page) (y . ,(max 0 (alist-get 'y entry)))))
        (vellumory--refresh t))
       ((and (stringp uri) (string-match-p "\\`\\(?:https?://\\|mailto:\\)" uri))
        (browse-url uri))
       (t (user-error "This entry has no supported destination"))))))

(defun vellumory-goto-heading ()
  "Jump to a PDF contents entry using completion."
  (interactive)
  (vellumory--navigation-ready)
  (let* ((source (current-buffer)) (hash vellumory--hash)
         (choices (cl-loop for entry across vellumory--outline for index from 0
                           collect (cons (vellumory--outline-description entry index) entry))))
    (unless choices (user-error "This PDF has no document outline"))
    (let ((choice (completing-read "Heading: " choices nil t)))
      (vellumory--follow-outline source hash (cdr (assoc choice choices))))))

(defun vellumory-goto-page-label ()
  "Jump by PDF page label, retaining physical numbers for duplicate labels."
  (interactive)
  (unless vellumory--pages (user-error "PDF is still opening"))
  (let* ((choices (cl-loop for page below (length vellumory--pages)
                           collect (cons (format "%s · page %d" (vellumory--page-description page) (1+ page)) page)))
         (choice (completing-read "Page label: " choices nil t)))
    (vellumory-goto-page (1+ (cdr (assoc choice choices))))))

(defun vellumory--imenu-jump (_name _position entry hash)
  "Follow contents ENTRY of the snapshot HASH; imenu calls it for an entry."
  (vellumory--follow-outline (current-buffer) hash entry))

(defun vellumory--imenu-index ()
  "Return an imenu index of the contents entries that go to a page."
  (vellumory--navigation-ready)
  (cl-loop for entry across vellumory--outline for index from 0
           when (natnump (alist-get 'page entry))
           collect (list (vellumory--outline-description entry index) index
                         #'vellumory--imenu-jump entry vellumory--hash)))

(defun vellumory--update-outline-buffer ()
  "Refresh the existing sidebar after document navigation data changes."
  (when (buffer-live-p vellumory--outline-buffer)
    (let ((source (current-buffer)) (hash vellumory--hash) (entries vellumory--outline)
          (state vellumory--navigation-state) (buffer vellumory--outline-buffer)
          (labels vellumory--labels))
      (with-current-buffer buffer
        (let ((inhibit-read-only t) (old-point (point)) folded)
          (erase-buffer)
          (setq vellumory--outline-source source)
          (cond
           ((eq state 'loading) (insert "Loading document contents…\n"))
           ((not (eq state 'ready)) (insert (format "Contents unavailable: %s\n" state)))
           ((zerop (length entries)) (insert "This PDF has no document outline.\n"))
           (t
            (cl-loop for entry across entries for index from 0 do
                     (let* ((entry entry) (start (point)) (page (alist-get 'page entry))
                            (has-children (and (< (1+ index) (length entries))
                                               (> (alist-get 'depth (aref entries (1+ index)))
                                                  (alist-get 'depth entry)))))
                       (insert (make-string (1+ (alist-get 'depth entry)) ?*) " ")
                       (insert-text-button
                        (vellumory--outline-title entry) 'follow-link t
                        'help-echo (if page (format "Go to page %d" (1+ page)) (alist-get 'uri entry))
                        'action (lambda (_button)
                                  (if (and has-children (null page) (equal (alist-get 'uri entry) ""))
                                      (outline-toggle-children)
                                    (vellumory--follow-outline source hash entry))))
                       (when page
                         (insert (propertize (format "  %s" (if (< page (length labels)) (aref labels page) (1+ page)))
                                             'face 'shadow)))
                       (insert "\n")
                       (when (and has-children (not (alist-get 'expanded entry))) (push start folded))))
            (dolist (position folded) (goto-char position) (outline-hide-subtree))))
          (goto-char (min old-point (point-max)))
          (set-buffer-modified-p nil))))))

(defun vellumory-outline-activate ()
  "Follow the contents entry on the current line."
  (interactive)
  (let ((button (or (button-at (point)) (next-button (line-beginning-position)))))
    (if (and button (< (button-start button) (line-end-position)))
        (button-activate button)
      (user-error "No contents entry on this line"))))

(defun vellumory-outline-return ()
  "Return to this sidebar's PDF buffer."
  (interactive)
  (unless (buffer-live-p vellumory--outline-source) (user-error "The PDF buffer has been closed"))
  (pop-to-buffer vellumory--outline-source))

(defvar vellumory-outline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'vellumory-outline-activate)
    (define-key map (kbd "TAB") #'outline-toggle-children)
    (define-key map (kbd "<backtab>") #'outline-show-all)
    (define-key map (kbd "n") #'outline-next-visible-heading)
    (define-key map (kbd "p") #'outline-previous-visible-heading)
    (define-key map (kbd "o") #'vellumory-outline-return)
    (define-key map (kbd "s") #'vellumory-sidebar-switch)
    map))

(define-derived-mode vellumory-outline-mode special-mode "PDF Contents"
  "Browse PDF contents.
RET follows, TAB folds, o returns to the PDF, s shows the page
previews in its place."
  (setq-local outline-regexp "\\*+ "
              outline-level (lambda () (save-excursion (skip-chars-forward "*") (- (point) (line-beginning-position))))
              truncate-lines t
              header-line-format '(:eval (vellumory--sidebar-tabs 'contents "RET visit · TAB fold")))
  (outline-minor-mode 1)
  (add-hook 'pre-command-hook #'vellumory--defer-gc nil t))

(defun vellumory--show-sidebar (buffer columns)
  "Display BUFFER in a left sidebar of COLUMNS and preserve its width."
  (let ((window (display-buffer-in-side-window
                 buffer `((side . left) (slot . 0) (window-width . ,columns)))))
    ;; A reused sidebar keeps its previous width unless explicitly resized.
    (window-preserve-size window t nil)
    (window-resize window (window-resizable window (- columns (window-total-width window)) t) t)
    (window-preserve-size window t t)
    window))

(defun vellumory--show-outline ()
  "Show the contents sidebar and return its window; the reader keeps focus."
  (unless (buffer-live-p vellumory--outline-buffer)
    (setq vellumory--outline-buffer (generate-new-buffer (format "*Contents: %s*" (buffer-name))))
    (with-current-buffer vellumory--outline-buffer (vellumory-outline-mode)))
  (vellumory--update-outline-buffer)
  (vellumory--show-sidebar vellumory--outline-buffer 34))

(defun vellumory-outline ()
  "Toggle a collapsible document contents sidebar."
  (interactive)
  (if-let* ((window (and (buffer-live-p vellumory--outline-buffer)
                         (get-buffer-window vellumory--outline-buffer))))
      (quit-window nil window)
    (select-window (vellumory--show-outline))))

;;;; One sidebar, two views: page previews or contents

(defun vellumory--sidebar-source ()
  "The reader buffer this buffer is, or is a sidebar of."
  (cond ((derived-mode-p 'vellumory-mode) (current-buffer))
        ((derived-mode-p 'vellumory-outline-mode) vellumory--outline-source)
        ((derived-mode-p 'vellumory-thumbnails-mode) vellumory--thumbnail-source)))

(defvar-local vellumory--sidebar-away nil
  "Sidebar view this document had when it last left its frame, or nil.
That view (`thumbnails' or `outline') is shown again when it returns.")

(defun vellumory--sidebar-follow (frame)
  "Default `window-buffer-change-functions' entry for FRAME.
A sidebar goes with its document: once no window of FRAME shows the
document (another file, Dired, a buffer switch), its sidebar closes, and
the document remembers it to show it again when it returns."
  (let ((windows (window-list frame 'never)))
    (dolist (window windows)
      (let ((sidebar (window-buffer window)))
        (when (and (window-live-p window) (window-parameter window 'window-side)
                   (memq (buffer-local-value 'major-mode sidebar)
                         '(vellumory-thumbnails-mode vellumory-outline-mode)))
          (let ((source (with-current-buffer sidebar (vellumory--sidebar-source))))
            (unless (seq-some (lambda (other) (and (window-live-p other) (eq (window-buffer other) source)
                                                   (not (window-parameter other 'window-side))))
                              windows)
              (when (buffer-live-p source)
                (with-current-buffer source
                  (setq vellumory--sidebar-away
                        (if (eq (buffer-local-value 'major-mode sidebar) 'vellumory-outline-mode) 'outline 'thumbnails))))
              (delete-window window))))))))

(defun vellumory--sidebar-return ()
  "Show the sidebar this document had when it left its frame, if any."
  (when-let* ((view vellumory--sidebar-away))
    (setq vellumory--sidebar-away nil)
    (unless (or (vellumory--shown vellumory--thumbnail-buffer) (vellumory--shown vellumory--outline-buffer))
      (if (eq view 'outline) (vellumory--show-outline) (vellumory--show-thumbnails)))))

(defun vellumory--has-contents-p ()
  "Whether this reader's document has an outline to show."
  (> (length vellumory--outline) 0))

(defun vellumory--shown (buffer)
  "Return a window showing BUFFER, or nil when it is dead or not shown."
  (and (buffer-live-p buffer) (get-buffer-window buffer)))

(defun vellumory-sidebar-switch (&optional event)
  "Switch the sidebar between the page previews and the contents.
Show the contents in place of the page previews, or the reverse; with
no sidebar, show the page previews.  A click on a sidebar tab (EVENT)
switches that sidebar.  The selected window stays selected."
  (interactive (list last-nonmenu-event))
  (when-let* (((mouse-event-p event)) (window (posn-window (event-start event))) ((windowp window)))
    (select-window window))
  (let ((source (or (vellumory--sidebar-source) (user-error "Not a document or its sidebar")))
        (from-sidebar (not (derived-mode-p 'vellumory-mode))))
    (unless (buffer-live-p source) (user-error "The PDF buffer has been closed"))
    (let ((window
           (with-current-buffer source
             (if (vellumory--shown vellumory--thumbnail-buffer)
                 (if (vellumory--has-contents-p) (vellumory--show-outline)
                   (user-error (if (eq vellumory--navigation-state 'loading) "The contents are still loading"
                                 "This document has no contents (outline); its pages are shown")))
               (vellumory--show-thumbnails)))))
      ;; Closing the sidebar later closes it, not shows the other view again.
      (set-window-prev-buffers window nil)
      (when from-sidebar (select-window window))
      window)))

(defvar vellumory--sidebar-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-1] #'ignore)
    (define-key map [header-line mouse-1] #'vellumory-sidebar-switch)
    (define-key map [header-line mouse-2] #'vellumory-sidebar-switch)
    map)
  "A click on the other tab switches the sidebar.")

(defun vellumory--sidebar-tabs (current hint)
  "Header line: the views Pages and Contents, CURRENT marked, then HINT."
  (let ((contents (when-let* ((source (vellumory--sidebar-source)) ((buffer-live-p source)))
                    (with-current-buffer source
                      (cond ((vellumory--has-contents-p) t)
                            ((eq vellumory--navigation-state 'loading) 'loading))))))
    (cl-flet ((tab (name view available)
                (cond ((eq view current) (propertize name 'face '(:inherit header-line :weight bold :underline t)))
                      ((eq available t)
                       (propertize name 'face '(:inherit (link header-line) :underline nil)
                                   'mouse-face 'header-line-highlight 'keymap vellumory--sidebar-tab-map
                                   'help-echo (format "Show the %s here (s)" (downcase name))))
                      (t (propertize name 'face '(:inherit (shadow header-line))
                                     'help-echo (if (eq available 'loading) "The contents are loading"
                                                  "This document has no contents (outline)"))))))
      (let ((tabs (concat " " (tab "Pages" 'pages t) "  " (tab "Contents" 'contents contents)))
            (hint (propertize (format "   %s · s switch" hint) 'face 'shadow))
            (window (get-buffer-window (current-buffer))))
        ;; A narrow sidebar shows the tabs whole rather than a hint cut short.
        (if (or (null window) (<= (+ (length tabs) (length hint)) (window-width window)))
            (concat tabs hint)
          tabs)))))

(provide 'vellumory-navigation)
;;; vellumory-navigation.el ends here
