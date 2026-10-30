;;; vellumory-noter.el --- org-noter sessions on Vellumory documents -*- lexical-binding: t; -*-

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
;; org-noter keeps notes in step with a document.  This backend lets a
;; session run on a Vellumory reader: notes follow the page in view, notes
;; open their place (page and height, from a selection or a click), the
;; selected text starts a note, and `org-noter-create-skeleton' turns the
;; document outline into headings.  Locations are org-noter's own form,
;; (PAGE . TOP) or (PAGE TOP . LEFT): one-based pages, fractions of the page.

;;; Code:
(require 'cl-lib)
;; Loaded after org-noter (see `vellumory-install'); compiles without it.
(require 'org-noter-core nil t)

(defvar org-noter--session)
(defvar org-noter-supported-modes)
(defvar org-noter-property-note-location)
(declare-function org-noter--valid-session "org-noter-core" (session))
(declare-function org-noter--get-location-page "org-noter-core" (location))
(declare-function org-noter--get-location-top "org-noter-core" (location))
(declare-function org-noter--doc-location-change-handler "org-noter-core" ())
(declare-function org-noter--parse-root "org-noter-core" (&optional info))
(declare-function org-noter--narrow-to-root "org-noter-core" (ast))
(declare-function org-noter--insert-heading "org-noter-core" (level title &optional newlines-number location))
(declare-function org-noter--pretty-print-location "org-noter-core" (location))
(declare-function org-noter--session-doc-mode "org-noter-core" (session))
(declare-function org-noter--session-doc-buffer "org-noter-core" (session))
(declare-function org-noter--session-notes-buffer "org-noter-core" (session))
(declare-function org-element-property "org-element" (property node &optional dflt force-undefer))
(declare-function org-entry-put "org" (epom property value))

(defmacro vellumory-noter--with-session (&rest body)
  "Run BODY with `session' bound to the valid org-noter session here.
As `org-noter--with-valid-session' does, BODY runs only when the session is
valid."
  (declare (indent 0) (debug t))
  `(let ((session org-noter--session))
     (when (org-noter--valid-session session) ,@body)))

(defvar vellumory--current-page)
(defvar vellumory--pages)
(defvar vellumory--selection)
(defvar vellumory--outline)
(defvar vellumory--navigation-state)
(defvar vellumory--process)
(defvar vellumory--source-file)
(defvar vellumory-page-change-functions)
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--reading-position "vellumory" (&optional window))
(declare-function vellumory--event-location "vellumory-select" (posn))
(declare-function vellumory--page-description "vellumory-navigation" (page))
(declare-function vellumory-selection-text "vellumory-select" ())

(defvar-local vellumory-noter--precise-page nil
  "Page (zero-based) of the latest precise location.
It may lie below the page at the top of the window.")

(defun vellumory-noter--size (page)
  "Return (WIDTH . HEIGHT) of the zero-based PAGE in points."
  (let ((size (aref vellumory--pages page))) (cons (alist-get 'width size) (alist-get 'height size))))

(defun vellumory-noter--fractions (page x y)
  "(TOP . LEFT) of point X, Y on PAGE as fractions of the page."
  (pcase-let ((`(,width . ,height) (vellumory-noter--size page)))
    (cons (max 0.0 (min 1.0 (/ y height))) (max 0.0 (min 1.0 (/ x width))))))

(defun vellumory-noter--approx-location (mode &optional precise-info _force-new-ref)
  "Return the reader's location as org-noter's (PAGE . PRECISE-INFO).
Only for MODE `vellumory-mode'.  PAGE is one-based: the page of the latest
precise location when PRECISE-INFO is given, else the current page.
PRECISE-INFO is kept when it is a TOP fraction or (TOP . LEFT), else 0."
  (when (eq mode 'vellumory-mode)
    (let ((page (or (and precise-info vellumory-noter--precise-page) vellumory--current-page 0)))
      (cons (1+ page) (if (or (numberp precise-info)
                              (and (consp precise-info) (numberp (car precise-info)) (numberp (cdr precise-info))))
                          precise-info
                        0)))))

(defun vellumory-noter--precise-info (mode window)
  "Return (TOP . LEFT) of the selection, or of a click on a page in WINDOW.
Only for MODE `vellumory-mode'.  Without a selection, wait for a click in
WINDOW.  Both are fractions of the page, whose zero-based number is kept
in `vellumory-noter--precise-page'."
  (when (eq mode 'vellumory-mode)
    (if-let* ((selection (car vellumory--selection))
              (quads (nth 2 selection))
              ((> (length quads) 0)))
        (let ((page (car selection)) (top most-positive-fixnum) (left most-positive-fixnum))
          (seq-doseq (quad quads)
            (setq top (min top (alist-get 'y0 quad) (alist-get 'y1 quad))
                  left (min left (alist-get 'x0 quad) (alist-get 'x3 quad))))
          (setq vellumory-noter--precise-page page)
          (vellumory-noter--fractions page left top))
      (let (location)
        (while (not location)
          (let ((event (read-event "Click where the note should begin")))
            (when (and (memq (car-safe event) '(mouse-1 down-mouse-1))
                       (eq window (posn-window (event-start event))))
              (setq location (vellumory--event-location (event-start event))))))
        (setq vellumory-noter--precise-page (car location))
        (vellumory-noter--fractions (car location) (nth 1 location) (nth 2 location))))))

(defun vellumory-noter--goto-location (mode location window)
  "Scroll WINDOW to org-noter's LOCATION and return t.
Only for MODE `vellumory-mode'.  The location's top, a fraction of its
page's height, is shown at the top of WINDOW."
  (when (eq mode 'vellumory-mode)
    (let* ((page (1- (org-noter--get-location-page location)))
           (top (or (org-noter--get-location-top location) 0))
           (height (cdr (vellumory-noter--size page))))
      (with-selected-window window
        (vellumory--restore-position `((page . ,page) (y . ,(* top height))))
        (vellumory--refresh t))
      t)))

(defun vellumory-noter--current-view (mode)
  "Return org-noter's view of the reader, [paged PAGE] with PAGE one-based.
Only for MODE `vellumory-mode'."
  (when (eq mode 'vellumory-mode)
    (vector 'paged (1+ (or vellumory--current-page 0)))))

(defun vellumory-noter--selected-text (mode)
  "Return the reader's selected text, for MODE `vellumory-mode' only."
  (when (eq mode 'vellumory-mode) (vellumory-selection-text)))

(defun vellumory-noter--buffer-file-name (&optional mode)
  "Return the reader's document file.
Only when MODE, or else the major mode, is `vellumory-mode'."
  (when (eq (or mode major-mode) 'vellumory-mode) vellumory--source-file))

(defun vellumory-noter--set-up (mode)
  "Make the notes follow page changes in the reader and return t.
Only for MODE `vellumory-mode'."
  (when (eq mode 'vellumory-mode)
    (add-hook 'vellumory-page-change-functions (lambda (_) (org-noter--doc-location-change-handler)) nil t)
    t))

(defun vellumory-noter--pretty-print (location)
  "Return LOCATION as text for the notes, in a session on a reader.
At the top of a page that is the page alone, else the whole location."
  (vellumory-noter--with-session
   (when (eq (org-noter--session-doc-mode session) 'vellumory-mode)
     (format "%s" (if (<= (or (org-noter--get-location-top location) 0) 0) (car location) location)))))

(defun vellumory-noter--pretty-print-for-title (location)
  "Return a title for LOCATION, in a session on a reader.
The page label, then the height on the page as a percentage when not at
the top."
  (vellumory-noter--with-session
   (when (eq (org-noter--session-doc-mode session) 'vellumory-mode)
     (let ((top (or (org-noter--get-location-top location) 0)))
       (with-current-buffer (org-noter--session-doc-buffer session)
         (concat (vellumory--page-description (1- (car location)))
                 (if (> top 0) (format " V: %d%%" (round (* 100 top))) "")))))))

(defun vellumory-noter--create-skeleton (mode)
  "Insert headings from the document outline, each at its page and height.
Only for MODE `vellumory-mode'; return t."
  (when (eq mode 'vellumory-mode)
    (vellumory-noter--with-session
     (let* ((ast (org-noter--parse-root))
            (top-level (or (org-element-property :level ast) 0))
            (entries (with-current-buffer (org-noter--session-doc-buffer session)
                       ;; The contents arrive after the document opens: wait
                       ;; for them a while rather than call them missing.
                       (let ((deadline (+ (float-time) 10)))
                         (while (and (eq vellumory--navigation-state 'loading) (< (float-time) deadline))
                           (accept-process-output vellumory--process 0.05)))
                       (when (eq vellumory--navigation-state 'loading)
                         (user-error "The contents are still loading; try again in a moment"))
                       (cl-loop for entry across vellumory--outline
                                when (alist-get 'page entry)
                                collect (let* ((page (alist-get 'page entry))
                                               (height (cdr (vellumory-noter--size page))))
                                          (list (alist-get 'title entry) (alist-get 'depth entry)
                                                (cons (1+ page) (/ (or (alist-get 'y entry) 0) height))))))))
       (unless entries (user-error "The document has no outline"))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))
           (pcase-dolist (`(,title ,depth ,location) entries)
             (org-noter--insert-heading (+ top-level 1 depth)
                                        (replace-regexp-in-string "[[:cntrl:]]" " " title))
             (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location))))
         (setq ast (org-noter--parse-root))
         (org-noter--narrow-to-root ast))
       t))))

(add-to-list 'org-noter-supported-modes 'vellumory-mode)
(add-hook 'org-noter-get-buffer-file-name-hook #'vellumory-noter--buffer-file-name)
(add-hook 'org-noter-set-up-document-hook #'vellumory-noter--set-up)
(add-hook 'org-noter--doc-approx-location-hook #'vellumory-noter--approx-location)
(add-hook 'org-noter--get-precise-info-hook #'vellumory-noter--precise-info)
(add-hook 'org-noter--doc-goto-location-hook #'vellumory-noter--goto-location)
(add-hook 'org-noter--get-current-view-hook #'vellumory-noter--current-view)
(add-hook 'org-noter-get-selected-text-hook #'vellumory-noter--selected-text)
(add-hook 'org-noter--pretty-print-location-hook #'vellumory-noter--pretty-print)
(add-hook 'org-noter--pretty-print-location-for-title-hook #'vellumory-noter--pretty-print-for-title)
(add-hook 'org-noter-create-skeleton-functions #'vellumory-noter--create-skeleton)

(provide 'vellumory-noter)
;;; vellumory-noter.el ends here
