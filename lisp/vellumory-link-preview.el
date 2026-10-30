;;; vellumory-link-preview.el --- Where a link leads, on hover -*- lexical-binding: t; -*-

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
;; With the mouse resting on a link within the document (a reference, a
;; figure, a footnote, an equation), a small frame by the link shows where
;; it leads, as GNOME's Papers (and Evince before it) does: after
;; `vellumory-link-preview-delay', at the zoom and in the page colours you
;; read in, the target 30% down from its top.  Unlike there, the wheel
;; scrolls it, so more than its first lines can be read; a click goes there,
;; a jump \\[vellumory-history-back] returns from; and moving away, or any
;; key, closes it.  `vellumory-link-preview' turns it off.

;;; Code:
(require 'cl-lib)

(defvar vellumory--scale)
(defvar vellumory--width)
(defvar vellumory--pages)
(defvar vellumory--margin)
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--theme-params "vellumory" ())
(declare-function vellumory--points-per-pixel "vellumory" (page))
(declare-function vellumory--page-height "vellumory" (page width))
(declare-function vellumory--event-location "vellumory-select" (posn))
(declare-function vellumory--link-at "vellumory-select" (page x y))
(declare-function vellumory--visit-link "vellumory-select" (link))
(declare-function vellumory--page-description "vellumory-navigation" (page))

(defcustom vellumory-link-preview t
  "Show where a link within the document leads while the mouse rests on it."
  :type 'boolean :group 'vellumory)

(defcustom vellumory-link-preview-delay 0.3
  "Seconds the mouse rests on a link before its preview shows."
  :type 'number :group 'vellumory)

(defcustom vellumory-link-preview-size '(0.92 . 0.35)
  "Size of a link's preview as (WIDTH . HEIGHT), fractions of the window's.
It shows whole lines at the size you read at: the part of the page the
window shows, never more than the page."
  :type '(cons (float :tag "Width") (float :tag "Height")) :group 'vellumory)

(defvar vellumory--link-preview nil
  "The link previewed or waited on, as a plist: :link, :window, :timer, :watch.")
(defvar vellumory--link-preview-frame nil "The child frame showing previews.")

(defun vellumory--link-preview-buffer ()
  "The buffer previews are shown in."
  (or (get-buffer " *vellumory-link-preview*")
      (with-current-buffer (get-buffer-create " *vellumory-link-preview*")
        (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                    cursor-type nil cursor-in-non-selected-windows nil truncate-lines t
                    scroll-margin 0 make-cursor-line-fully-visible nil
                    left-margin-width 0 right-margin-width 0)
        (let ((map (make-sparse-keymap)))
          (keymap-set map "<wheel-down>" #'vellumory-link-preview-scroll-down)
          (keymap-set map "<wheel-up>" #'vellumory-link-preview-scroll-up)
          (keymap-set map "<mouse-5>" #'vellumory-link-preview-scroll-down)
          (keymap-set map "<mouse-4>" #'vellumory-link-preview-scroll-up)
          (keymap-set map "<down-mouse-1>" #'ignore)
          (keymap-set map "<mouse-1>" #'vellumory-link-preview-visit)
          (use-local-map map))
        (current-buffer))))

(defun vellumory--link-preview-hover (window link)
  "The mouse is on LINK in WINDOW: preview it once it rests there."
  (when (and vellumory-link-preview (window-live-p window) (display-graphic-p (window-frame window))
             (natnump (alist-get 'page link))
             (not (eq link (plist-get vellumory--link-preview :link))))
    (vellumory-link-preview-hide)
    (setq vellumory--link-preview
          (list :link link :window window
                :timer (run-with-timer vellumory-link-preview-delay nil #'vellumory--link-preview-show window link)
                :watch (run-with-timer 0.1 0.1 #'vellumory--link-preview-watch)))
    (add-hook 'pre-command-hook #'vellumory--link-preview-command)))

(defun vellumory--link-preview-command ()
  "Close the preview before any command but those in it."
  (unless (and (frame-live-p vellumory--link-preview-frame)
               (eq (window-frame (selected-window)) vellumory--link-preview-frame))
    (unless (memq this-command '(vellumory-link-preview-scroll-down vellumory-link-preview-scroll-up
                                 vellumory-link-preview-visit ignore handle-switch-frame))
      (vellumory-link-preview-hide))))

(defun vellumory--link-preview-under-mouse ()
  "The link under the mouse in a document, or nil."
  (pcase-let ((`(,frame ,x . ,y) (mouse-pixel-position)))
    (when (and (frame-live-p frame) x y)
      (let* ((posn (posn-at-x-y x y frame))
             (window (posn-window posn)))
        (when (and (windowp window) (eq window (plist-get vellumory--link-preview :window)))
          (with-current-buffer (window-buffer window)
            (pcase (vellumory--event-location posn)
              (`(,page ,px ,py) (vellumory--link-at page px py)))))))))

(defun vellumory--link-preview-watch ()
  "Close the preview once the mouse is neither on its link nor on it.
It must be away for two looks, a fifth of a second: on the way from the link
to the preview, the mouse crosses the page between them."
  (let ((frame (car (mouse-pixel-position))))
    (if (or (and (frame-live-p vellumory--link-preview-frame) (eq frame vellumory--link-preview-frame))
            (eq (vellumory--link-preview-under-mouse) (plist-get vellumory--link-preview :link)))
        (plist-put vellumory--link-preview :away 0)
      (if (>= (or (plist-get vellumory--link-preview :away) 0) 1)
          (vellumory-link-preview-hide)
        (setq vellumory--link-preview (plist-put vellumory--link-preview :away 1))))))

(defun vellumory-link-preview-hide ()
  "Close the link preview."
  (interactive)
  (when-let* ((timer (plist-get vellumory--link-preview :timer))) (cancel-timer timer))
  (when-let* ((timer (plist-get vellumory--link-preview :watch))) (cancel-timer timer))
  (setq vellumory--link-preview nil)
  (remove-hook 'pre-command-hook #'vellumory--link-preview-command)
  (when (and (frame-live-p vellumory--link-preview-frame) (frame-visible-p vellumory--link-preview-frame))
    (make-frame-invisible vellumory--link-preview-frame)))

(defun vellumory--link-preview-show (window link)
  "Ask for LINK's target, as WINDOW shows its page, to preview it."
  (when (and (window-live-p window) (eq link (plist-get vellumory--link-preview :link)))
    (with-current-buffer (window-buffer window)
      (let* ((page (alist-get 'page link))
             (scale vellumory--scale)
             (k (vellumory--points-per-pixel page))
             (page-width vellumory--width)
             (width (min page-width (max 240 (round (* (car vellumory-link-preview-size)
                                                        (window-body-width window t))))))
             (height (max 120 (round (* (cdr vellumory-link-preview-size) (window-body-height window t)))))
             (page-height (vellumory--page-height page page-width))
             ;; The target 30% down, as Papers places it.
             (top (max 0 (min (- page-height height)
                              (- (round (/ (max 0 (or (alist-get 'y link) 0)) k)) (round (* 0.3 height))))))
             (frame (window-frame window))
             ;; The page's left edge in the window, and the part the window shows.
             (origin (- (or vellumory--margin 0)
                        (* (window-hscroll window) (frame-char-width frame))))
             (inset (round (* 0.5 (- 1 (car vellumory-link-preview-size)) (window-body-width window t))))
             (left (max 0 (min (- page-width width) (- inset origin))))
             ;; Three heights more, for the wheel.
             (bottom (min page-height (+ top (* 4 height))))
             (theme (vellumory--theme-params))
             (description (vellumory--page-description page)))
        (vellumory--rpc
         "render" `((page . ,page) (width . ,(round (* scale page-width)))
                    (left . ,(floor (* scale left))) (right . ,(ceiling (* scale (+ left width))))
                    (top . ,(floor (* scale top))) (bottom . ,(ceiling (* scale bottom)))
                    (priority . 0)
                    ,@(and theme `((theme . ,theme))))
         (lambda (reply)
           (when (and (eq link (plist-get vellumory--link-preview :link)) (alist-get 'result reply))
             (vellumory--link-preview-display
              frame (create-image (alist-get 'path (alist-get 'result reply)) 'pbm nil
                                  :width width :height (- bottom top) :ascent 100)
              width height description
              ;; Over the page's own columns, the preview a window onto it.
              (+ (car (window-body-pixel-edges window)) origin left)))))))))

(defun vellumory--link-preview-frame (parent)
  "The preview frame, a child of PARENT."
  (unless (and (frame-live-p vellumory--link-preview-frame)
               (eq (frame-parent vellumory--link-preview-frame) parent))
    (when (frame-live-p vellumory--link-preview-frame) (delete-frame vellumory--link-preview-frame))
    (let ((buffer (vellumory--link-preview-buffer)))
      (setq vellumory--link-preview-frame
            (make-frame `((parent-frame . ,parent) (minibuffer . nil) (visibility . nil)
                          (no-accept-focus . t) (no-focus-on-map . t) (no-other-frame . t)
                          (undecorated . t) (unsplittable . t) (desktop-dont-save . t)
                          (internal-border-width . 0) (child-frame-border-width . 2)
                          (left-fringe . 0) (right-fringe . 0) (vertical-scroll-bars . nil)
                          (horizontal-scroll-bars . nil) (menu-bar-lines . 0) (tool-bar-lines . 0)
                          (tab-bar-lines . 0) (line-spacing . 0) (cursor-type . nil)
                          (background-color . ,(face-background 'default parent t)))))
      (let ((window (frame-root-window vellumory--link-preview-frame)))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t))))
  vellumory--link-preview-frame)

(defun vellumory--link-preview-display (parent image width height description left)
  "Show IMAGE, WIDTH by HEIGHT pixels, by the mouse on frame PARENT.
It is part of the page DESCRIPTION names; its left edge is at LEFT."
  (let* ((frame (vellumory--link-preview-frame parent))
         (window (frame-root-window frame))
         (y (or (cddr (mouse-pixel-position)) 0))
         (header (+ 6 (frame-char-height parent)))
         (total (+ height header)))
    (with-current-buffer (window-buffer window)
      (setq-local header-line-format
                  (list " " (propertize (concat "Page " description) 'face 'bold)
                        (propertize "   click to go there · the wheel scrolls" 'face 'shadow)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image image "preview")
        (goto-char (point-min))))
    (set-face-background 'child-frame-border (face-foreground 'shadow parent t) frame)
    (set-frame-size frame width total t)
    ;; Above the link when there is room, as Papers' popover; else below.
    (set-frame-position frame
                        (max 0 (min (- (frame-pixel-width parent) width 4) left))
                        (if (> (- y total 20) 0) (- y total 20) (+ y 20)))
    (set-window-vscroll window 0 t)
    (make-frame-visible frame)))

(defun vellumory-link-preview-scroll-down (&optional event)
  "Scroll the link preview down, by EVENT's amount or a few lines."
  (interactive "e")
  (ignore event)
  (when (frame-live-p vellumory--link-preview-frame)
    (let ((window (frame-root-window vellumory--link-preview-frame)))
      (set-window-vscroll window (+ (window-vscroll window t) (* 3 (default-line-height))) t))))

(defun vellumory-link-preview-scroll-up (&optional event)
  "Scroll the link preview up, by EVENT's amount or a few lines."
  (interactive "e")
  (ignore event)
  (when (frame-live-p vellumory--link-preview-frame)
    (let ((window (frame-root-window vellumory--link-preview-frame)))
      (set-window-vscroll window (max 0 (- (window-vscroll window t) (* 3 (default-line-height)))) t))))

(defun vellumory-link-preview-visit ()
  "Go where the previewed link leads; \\[vellumory-history-back] returns."
  (interactive)
  (let ((link (plist-get vellumory--link-preview :link))
        (window (plist-get vellumory--link-preview :window)))
    (vellumory-link-preview-hide)
    (when (and link (window-live-p window))
      (select-frame-set-input-focus (window-frame window))
      (with-selected-window window (vellumory--visit-link link)))))

(provide 'vellumory-link-preview)
;;; vellumory-link-preview.el ends here
