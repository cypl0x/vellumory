;;; vellumory-sync.el --- SyncTeX between TeX sources and documents -*- lexical-binding: t; -*-

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
;; Forward search (`vellumory-sync-forward', AUCTeX's "Vellumory" viewer)
;; shows where the source line at point appears and briefly highlights it.
;; Backward search (C-mouse-1 in the reader) visits the source of a point on
;; a page.  Both read the document's .synctex.gz through TeX Live's
;; libsynctex, the parser Skim, Sumatra and Texworks use.
;;
;; With AUCTeX, `vellumory-sync-tex-viewer' makes Vellumory the PDF viewer
;; (C-c C-v, or SPC m v in Doom) once TeX mode starts; compile with
;; `TeX-source-correlate-mode' on.

;;; Code:
(require 'cl-lib)
(require 'seq)

(defvar vellumory--source-file)
(defvar vellumory--file)
(defvar vellumory--pages)
(defvar vellumory--pending-position)
(defvar TeX-view-program-list)
(defvar TeX-view-program-selection)
(defvar TeX-source-correlate-mode)
(defvar TeX-current-process-region-p)
(declare-function vellumory--cli "vellumory-edit" (args callback &optional buffer input))
(declare-function vellumory-open "vellumory" (file &optional no-select))
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--event-location "vellumory-select" (posn))
(declare-function vellumory--remember-position "vellumory-select" ())
(declare-function TeX-active-master "tex" (&optional extension nondirectory ignore))
(declare-function TeX-output-extension "tex" ())
(declare-function TeX-region-file "tex" (&optional extension nondirectory ignore))
(declare-function TeX-source-correlate-handle-TeX-region "tex" (file line col))

(defgroup vellumory-sync nil "SyncTeX." :group 'vellumory)

(defcustom vellumory-sync-flash-seconds 1.5
  "How long forward search highlights the found lines." :type 'number)

(defcustom vellumory-sync-context-points 72
  "Points of the page kept visible above a forward search result." :type 'number)

(defcustom vellumory-sync-tex-viewer t
  "Non-nil: AUCTeX views PDF output with Vellumory.
Applied when a TeX buffer starts, after every other viewer setup (Doom's
:lang latex included) has run." :type 'boolean)

(defvar vellumory-sync-backward-redirect-functions nil
  "Functions called with FILE LINE COLUMN after a backward search.
The first to return a list (FILE LINE COLUMN) replaces the location; AUCTeX's
`TeX-source-correlate-handle-TeX-region' maps region files back this way.")

(defvar-local vellumory--sync-flash nil "Page → list of quad vectors to flash.")
(defvar-local vellumory--sync-flash-timer nil)

(defun vellumory--sync-quads (page)
  "Forward search results on PAGE for the renderer."
  (alist-get page vellumory--sync-flash))

(defun vellumory--sync-boxes (boxes)
  "Page → quad vectors for the SyncTeX BOXES."
  (let (flash)
    (seq-doseq (box boxes)
      (let ((x0 (alist-get 'x box)) (y0 (alist-get 'y box))
            (x1 (+ (alist-get 'x box) (alist-get 'width box)))
            (y1 (+ (alist-get 'y box) (alist-get 'height box))))
        (push (vector x0 y0 x1 y0 x1 y1 x0 y1) (alist-get (1- (alist-get 'page box)) flash))))
    flash))

(defun vellumory--sync-show (pdf boxes)
  "Show the first of BOXES in PDF's reader and flash all of them."
  (let* ((buffer (vellumory-open pdf t))
         (box (aref boxes 0))
         (target `((page . ,(1- (alist-get 'page box)))
                   (y . ,(max 0 (- (alist-get 'y box) vellumory-sync-context-points))))))
    (display-buffer buffer)
    (with-current-buffer buffer
      (setq vellumory--sync-flash (vellumory--sync-boxes boxes))
      (if (not vellumory--pages)
          ;; Still opening: the first layout goes there.
          (setq vellumory--pending-position target)
        (vellumory--remember-position)
        (vellumory--restore-position target)
        (vellumory--refresh t))
      (when (timerp vellumory--sync-flash-timer) (cancel-timer vellumory--sync-flash-timer))
      (setq vellumory--sync-flash-timer
            (run-with-timer vellumory-sync-flash-seconds nil
                            (lambda ()
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (setq vellumory--sync-flash nil)
                                  (vellumory--refresh t)))))))
    buffer))

;;;###autoload
(defun vellumory-sync-forward (&optional pdf)
  "Show where the source line at point appears in PDF.
PDF defaults to the file's name with a .pdf extension."
  (interactive)
  (require 'vellumory)
  (let* ((source (or (buffer-file-name) (user-error "Forward search needs a TeX file")))
         (pdf (expand-file-name (or pdf (concat (file-name-sans-extension source) ".pdf"))))
         (line (save-restriction (widen) (line-number-at-pos nil t)))
         (column (current-column)))
    (unless (file-exists-p pdf) (user-error "No document %s" (abbreviate-file-name pdf)))
    (vellumory--cli (list "synctex" pdf source (number-to-string line) "--column" (number-to-string column))
                    (lambda (result)
                      (let ((boxes (alist-get 'boxes result)))
                        (if (zerop (length boxes))
                            (message "SyncTeX: %s:%d does not appear in %s" (file-name-nondirectory source)
                                     line (file-name-nondirectory pdf))
                          (vellumory--sync-show pdf boxes)))))))

;;;###autoload
(defun vellumory-sync-view ()
  "AUCTeX viewer: show the output, at point's position with source correlation.
Mirrors AUCTeX's PDF Tools viewer, region compilation included."
  (add-hook 'vellumory-sync-backward-redirect-functions #'TeX-source-correlate-handle-TeX-region)
  (let ((pdf (expand-file-name (TeX-active-master (TeX-output-extension)))))
    (if (bound-and-true-p TeX-source-correlate-mode)
        (with-current-buffer (or (and TeX-current-process-region-p (get-file-buffer (TeX-region-file t)))
                                 (current-buffer))
          (vellumory-sync-forward pdf))
      (require 'vellumory)
      (pop-to-buffer (vellumory-open pdf t)))))

;;;###autoload
(defun vellumory-sync-select-viewer ()
  "Make Vellumory AUCTeX's PDF viewer, per `vellumory-sync-tex-viewer'."
  (when vellumory-sync-tex-viewer
    (add-to-list 'TeX-view-program-list '("Vellumory" vellumory-sync-view))
    (setq TeX-view-program-selection
          (cons '(output-pdf "Vellumory")
                (cl-remove '(output-pdf "Vellumory") TeX-view-program-selection :test #'equal)))))

(defun vellumory--sync-resolve (file pdf)
  "FILE as TeX recorded it, or the same name next to PDF when it has moved."
  (let ((beside (expand-file-name (file-name-nondirectory file) (file-name-directory pdf))))
    (cond ((file-exists-p file) (expand-file-name file))
          ((file-exists-p beside) beside)
          (t file))))

(defun vellumory--sync-visit (file line column)
  "Show FILE, widened, at one-based LINE and zero-based COLUMN."
  (pop-to-buffer (find-file-noselect file))
  (widen)
  (goto-char (point-min))
  (forward-line (1- (max 1 line)))
  (when (> column 0) (move-to-column column)))

;;;###autoload
(defun vellumory-sync-backward (event)
  "Visit the TeX source that produced the point clicked in EVENT."
  (interactive "e")
  ;; A click into a non-selected window runs in the selected window's buffer.
  (let* ((posn (event-start event)) (window (posn-window posn)))
    (with-current-buffer (if (windowp window) (window-buffer window) (current-buffer))
      (let ((location (or (vellumory--event-location posn) (user-error "Not on a page")))
            (pdf (or vellumory--source-file vellumory--file)))
        (vellumory--cli (list "synctexback" pdf (number-to-string (1+ (car location)))
                              (number-to-string (round (nth 1 location)))
                              (number-to-string (round (nth 2 location))))
                        (lambda (result)
                          (if-let* ((source (seq-first (alist-get 'sources result))))
                              (let ((place (list (vellumory--sync-resolve (alist-get 'file source) pdf)
                                                 (alist-get 'line source) (alist-get 'column source))))
                                (apply #'vellumory--sync-visit
                                       (or (apply #'run-hook-with-args-until-success
                                                  'vellumory-sync-backward-redirect-functions place)
                                           place)))
                            (message "SyncTeX knows no source for this point"))))))))

(provide 'vellumory-sync)
;;; vellumory-sync.el ends here
