;;; vellumory-follow.el --- One document read across several windows -*- lexical-binding: t; -*-

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
;; `vellumory-follow-mode' is follow-mode for documents: the windows of this
;; frame showing the document read as one tall window, each starting where the
;; one before it ends, in follow-mode's order (top to bottom, then left to
;; right).  Scrolling any of them moves them all, and a screen is all of them.
;; Unlike follow-mode, which chains windows by buffer positions, it chains them
;; by pixels: a page may continue from the foot of one column to the head of
;; the next.  M-x follow-mode in a document turns it on, and
;; `vellumory-follow-split' (C-c . 1, as follow-mode's) shows the document in
;; side-by-side columns that follow each other.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar vellumory--rows)
(declare-function vellumory--absolute-position "vellumory" (&optional window))
(declare-function vellumory--seek "vellumory" (offset &optional window))
(declare-function vellumory--refresh "vellumory" (&optional force))

(defvar-local vellumory-follow--positions nil
  "Each follower's position, (WINDOW . PIXELS), as last aligned.")

(defun vellumory-follow--windows ()
  "The windows of this frame showing this document, in follow-mode's order."
  (let ((frame (window-frame (or (get-buffer-window (current-buffer)) (selected-window)))))
    (sort (get-buffer-window-list (current-buffer) 'no-minibuf frame)
          (lambda (a b)
            (let ((ea (window-pixel-edges a)) (eb (window-pixel-edges b)))
              (if (= (nth 1 ea) (nth 1 eb)) (< (car ea) (car eb)) (< (nth 1 ea) (nth 1 eb))))))))

(defun vellumory-follow--screen-height ()
  "Pixels all followers show together."
  (apply #'+ (mapcar (lambda (window) (window-body-height window t)) (vellumory-follow--windows))))

(defun vellumory-follow--align ()
  "Chain the followers from the one that moved.
That is the selected window if it moved, else any other that did, or
the first window when none did."
  (let ((windows (vellumory-follow--windows)))
    (if (not (cdr windows))
        (setq vellumory-follow--positions nil)
      (let* ((moved (lambda (window)
                      (not (eql (vellumory--absolute-position window)
                                (alist-get window vellumory-follow--positions)))))
             (anchor (or (and (memq (selected-window) windows) (funcall moved (selected-window))
                              (selected-window))
                         (seq-find moved windows)
                         (car windows)))
             (index (seq-position windows anchor))
             (offsets (make-vector (length windows) 0)))
        (aset offsets index (vellumory--absolute-position anchor))
        (cl-loop for i from (1+ index) below (length windows) do
                 (aset offsets i (+ (aref offsets (1- i)) (window-body-height (nth (1- i) windows) t))))
        (cl-loop for i downfrom (1- index) to 0 do
                 (aset offsets i (- (aref offsets (1+ i)) (window-body-height (nth i windows) t))))
        (cl-loop for window in windows for i from 0 do
                 (unless (or (eq window anchor)
                             (eql (vellumory--absolute-position window) (max 0 (aref offsets i))))
                   (vellumory--seek (max 0 (aref offsets i)) window)))
        (setq vellumory-follow--positions
              (mapcar (lambda (window) (cons window (vellumory--absolute-position window))) windows))))))

;;;###autoload
(define-minor-mode vellumory-follow-mode
  "Read this document across its windows in the frame as across one tall one.
Each window starts where the one before it ends (top to bottom, then left to
right, as in `follow-mode'); scrolling any of them moves all of them, and
paging moves by all of them together.  `vellumory-follow-split' shows the
document in such columns."
  :lighter " Follow"
  (setq vellumory-follow--positions nil)
  (when (and vellumory-follow-mode (bound-and-true-p vellumory--rows))
    (vellumory--refresh t)))

;;;###autoload
(defun vellumory-follow-split (&optional columns)
  "Show this document in COLUMNS side-by-side windows following each other.
COLUMNS is the prefix argument, two by default and at least two.  The
windows start at the position in view."
  (interactive "P")
  (let ((columns (max 2 (if columns (prefix-numeric-value columns) 2)))
        (position (vellumory--absolute-position)))
    (delete-other-windows)
    (dotimes (_ (1- columns)) (split-window-right))
    (balance-windows)
    (vellumory--seek position (selected-window))
    (vellumory-follow-mode 1)))

(defvar follow-mode)
(declare-function follow-mode "follow" (&optional arg))

(defun vellumory-follow--instead-of-follow-mode ()
  "Toggle `vellumory-follow-mode' instead of `follow-mode' in a document.
This applies to \\[follow-mode] and to its split command,
`follow-delete-other-windows-and-split': buffer positions cannot chain
pixel-scrolled pages."
  (when (and (bound-and-true-p follow-mode) (derived-mode-p 'vellumory-mode))
    (follow-mode -1)
    (vellumory-follow-mode (if vellumory-follow-mode -1 1))))

(add-hook 'follow-mode-hook #'vellumory-follow--instead-of-follow-mode)

(provide 'vellumory-follow)
;;; vellumory-follow.el ends here
