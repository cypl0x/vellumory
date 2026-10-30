;;; vellumory-present.el --- Present a document full screen -*- lexical-binding: t; -*-

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
;; `vellumory-present' (C-c C-p) shows the document one page at a time on
;; a full-screen frame of its own, as the presentation views of Evince and
;; Okular do: each page fitted to the screen and centred on black, the pages
;; either side rendered ahead so changing pages is immediate.
;;
;; Space, Right, Down, PgDn, n, j, l, a left click or the wheel go forward;
;; Backspace, S-SPC, Left, Up, PgUp, p, h, k, a right click or the wheel back;
;; Home and End go to either end; digits then RET to a page; b or . blank the
;; screen to black, w to white, and any key returns.  Links are followed by a
;; click.  A page's /Dur advances it on its own, as the document asks.  q or
;; Escape ends the presentation, leaving the reader at the page last shown.

;;; Code:
;; Primitives of Emacs built with image support (the only kind Vellumory runs in).
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function image-flush "image.c" (spec &optional frame))
(require 'cl-lib)
(require 'subr-x)
(require 'face-remap)

(defvar vellumory--pages)
(defvar vellumory--scale)
(defvar vellumory--current-page)
(defvar vellumory--hash)
;; Emacs 32 since 2026-08-16; used only when `image-type-available-p' says so.
(declare-function canvas-refresh "image.c" (image &optional reload-data))
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory-goto-page "vellumory" (page))
(declare-function vellumory--defer-gc "vellumory" ())
(defvar vellumory--animation-timers)
(declare-function vellumory--ensure-links "vellumory-select" (page))
(declare-function vellumory--link-at "vellumory-select" (page x y))
(declare-function vellumory--link-help "vellumory-select" (link))
(declare-function vellumory--visit-link "vellumory-select" (link))

(defgroup vellumory-present nil "Presenting documents full screen." :group 'vellumory)

(defcustom vellumory-present-follow-durations t
  "Advance on its own after a page's /Dur seconds, as the document asks."
  :type 'boolean)

(defcustom vellumory-present-background "black" "Colour around the pages." :type 'color)

(defcustom vellumory-present-monitor 'other
  "Where to present the document.
`other' prefers a monitor without the selected frame (a projector), `same'
the selected frame's monitor."
  :type '(choice (const :tag "Another monitor, if any" other) (const :tag "This monitor" same)))

(defvar-local vellumory-present--source nil "The reader buffer presented.")
(defvar-local vellumory-present--page 0 "Zero-based page shown.")
(defvar-local vellumory-present--state 'normal "`normal', `black', `white' or `end'.")
(defvar-local vellumory-present--digits "" "A page number being typed.")
(defvar-local vellumory-present--generation 0 "Bumped on every change; late renders are dropped.")
(defvar-local vellumory-present--timer nil "Advances the page after its /Dur.")
(defvar-local vellumory-present--info nil "Page → its page-info report (/Dur, /Trans), once known.")
(defvar-local vellumory-present--shown nil "(PAGE IMAGE W H) on screen now, or nil.")
(defvar-local vellumory-present--animation nil "Timer drawing a page transition.")
(defvar-local vellumory-present--last-wheel 0.0)
(defvar-local vellumory-present--background nil "Face-remap cookies for the background.")
(defvar-local vellumory-present--colour nil "The background colour painted.")

(defun vellumory-present--pages ()
  "Return the page sizes of the presented document, its `vellumory--pages'."
  (buffer-local-value 'vellumory--pages vellumory-present--source))

(defun vellumory-present--geometry (page width height)
  "PAGE fitted into WIDTH × HEIGHT pixels, centred: (W H LEFT TOP)."
  (let* ((size (aref (vellumory-present--pages) page))
         (pw (float (alist-get 'width size))) (ph (float (alist-get 'height size)))
         (scale (min (/ width pw) (/ height ph)))
         ;; Rounded, not floored: 1080 / 792 × 792 is 1079.999….
         (w (max 1 (min width (round (* pw scale))))) (h (max 1 (min height (round (* ph scale))))))
    (list w h (/ (- width w) 2) (/ (- height h) 2))))

(defun vellumory-present--window ()
  "Return a window showing this presentation, on any frame, or nil."
  (get-buffer-window (current-buffer) t))

(defun vellumory-present--fit (page)
  "PAGE's geometry in this buffer's window, or nil without one."
  (when-let* ((window (vellumory-present--window)))
    (vellumory-present--geometry page (window-body-width window t) (window-body-height window t))))

(defun vellumory-present--render (page width height priority callback)
  "Render PAGE to fill WIDTH × HEIGHT logical pixels, at PRIORITY.
CALLBACK gets the path of the raster, which has exactly the device pixels
it is drawn on: an image Emacs must rescale, even by one row, took 60 ms
instead of 6 to draw (pgtk, 1278 × 958)."
  (let* ((scale (buffer-local-value 'vellumory--scale vellumory-present--source))
         (device (max 1 (round (* scale width)))) (rows (max 1 (round (* scale height)))))
    (with-current-buffer vellumory-present--source
      (vellumory--rpc "render" `((page . ,page) (width . ,device) (top . 0) (bottom . ,rows)
                                 (priority . ,priority))
                      (lambda (reply) (funcall callback (alist-get 'path (alist-get 'result reply))))))))

(defun vellumory-present--link-map (page w)
  "Image-map areas for PAGE's links, drawn W pixels wide."
  (let ((k (/ w (float (alist-get 'width (aref (vellumory-present--pages) page))))))
    (mapcar (lambda (link)
              `((rect . ((,(round (* k (alist-get 'x0 link))) . ,(round (* k (alist-get 'y0 link))))
                         . (,(round (* k (alist-get 'x1 link))) . ,(round (* k (alist-get 'y1 link))))))
                vellumory-link (pointer hand help-echo ,(vellumory--link-help link))))
            (with-current-buffer vellumory-present--source (vellumory--ensure-links page)))))

(defun vellumory-present--spacer (width height)
  "Return a blank WIDTH × HEIGHT pixels, at least one pixel high."
  (propertize " " 'display `(space :width (,width) :height (,(max 1 height)) :ascent 100)))

(defun vellumory-present--image (page path w h)
  "PAGE's raster at PATH as an image drawn W × H, its links clickable."
  (create-image path 'pbm nil :scale 1.0 :width w :height h :ascent 100
                :transform-smoothing (/= (buffer-local-value 'vellumory--scale vellumory-present--source) 1.0)
                :map (vellumory-present--link-map page w)))

(defun vellumory-present--draw-rows (page rows images left top)
  "Draw ROWS of slices of IMAGES (`old' and `new' → (IMAGE W H)) at LEFT, TOP.
Slices of `new' show PAGE.  Each row is a list of (WHICH X Y WIDTH HEIGHT);
a row of one whole `new' page is drawn unsliced, so its links stay
clickable."
  (let ((inhibit-read-only t) (window (vellumory-present--window)))
    (erase-buffer)
    (when (> top 0) (insert (vellumory-present--spacer 1 top) (propertize "\n" 'line-height t)))
    (dolist (row rows)
      (insert (vellumory-present--spacer left 1))
      (dolist (segment row)
        (pcase-let ((`(,which ,x ,y ,width ,height) segment))
          (pcase-let ((`(,image ,w ,h) (alist-get which images)))
            (insert (propertize " " 'vellumory-present-page (and (eq which 'new) page)
                                'display (if (and (= x 0) (= y 0) (= width w) (= height h))
                                             image
                                           (list (list 'slice x y width height) image)))))))
      (insert (propertize "\n" 'line-height t)))
    (goto-char (point-min))
    (when window (set-window-start window (point-min) t))))

(defun vellumory-present--draw (page path)
  "Show PAGE's raster at PATH centred, or, without PATH, the blank or end screen."
  (when-let* ((window (vellumory-present--window)))
    (let ((inhibit-read-only t)
          (width (window-body-width window t)) (height (window-body-height window t)))
      (erase-buffer)
      (setq vellumory-present--shown nil)
      (cond
       (path
        (pcase-let* ((`(,w ,h ,left ,top) (vellumory-present--fit page))
                     (image (vellumory-present--image page path w h)))
          (vellumory-present--draw-rows page `(((new 0 0 ,w ,h))) `((new ,image ,w ,h)) left top)
          (setq vellumory-present--shown (list page image w h))))
       ((eq vellumory-present--state 'end)
        (let* ((text (propertize "End of presentation.  Click or press q to leave." 'face 'shadow))
               (line (default-line-height)))
          (insert (vellumory-present--spacer 1 (/ (- height line) 2)) (propertize "\n" 'line-height t)
                  (vellumory-present--spacer (max 0 (/ (- width (string-pixel-width text)) 2)) 1) text)))
       (t (insert (vellumory-present--spacer width height))))
      (goto-char (point-min))
      (set-window-start window (point-min) t))))

;;;; Page transitions (/Trans)

(defcustom vellumory-present-transitions t
  "Draw the page transitions (/Trans) the document asks for.
Wipe, push, cover, uncover, split, box and blinds move as the document
asks; dissolve, glitter, fade and fly are composed from both pages; other
styles cut."
  :type 'boolean)

(defconst vellumory-present--blinds 6 "Bands of a /Blinds transition.")

(defconst vellumory-present--sliced '("Wipe" "Push" "Cover" "Uncover" "Split" "Box" "Blinds")
  "Styles drawn by moving slices of the two pages.")

(defconst vellumory-present--blended '("Dissolve" "Glitter" "Fade" "Fly")
  "Styles drawn from frames the worker composes from the two pages.")

(defcustom vellumory-present-canvas t
  "Draw composed transitions on an Emacs canvas image when Emacs has them.
Each frame then replaces the pixels of one image in place: Emacs reads 4 bytes
a pixel and decodes nothing (2 ms a frame instead of 7 at 1278 × 958).
Canvas images are in Emacs 32 since 2026-08-16; without them, frames are
images of their own."
  :type 'boolean)

(defvar-local vellumory-present--frame nil "The composed transition frame on screen, if any.")
(defvar-local vellumory-present--frames 0 "Frames drawn by the latest composed transition.")
(defvar-local vellumory-present--slot nil "This presentation's canvas frame file in the worker.")
(defvar vellumory-present--slots 0 "Canvas frame files handed out so far (the worker keeps eight).")

(defun vellumory-present--drop-frame ()
  "Forget the transition frame on screen: a frame is shown once."
  (when vellumory-present--frame
    (image-flush vellumory-present--frame t)
    (setq vellumory-present--frame nil)))

(defun vellumory-present--direction (transition)
  "TRANSITION's /Di as 0, 90, 180 or 270 degrees (others as 0)."
  (let ((di (alist-get 'direction transition)))
    (if (numberp di) (let ((d (mod (round di) 360))) (if (memq d '(90 180 270)) d 0)) 0)))

(defun vellumory-present--compose (transition progress w h)
  "Rows of slices showing TRANSITION at PROGRESS (0 to 1) between pages W × H.
Each row is a list of (WHICH X Y WIDTH HEIGHT), WHICH `old' or `new'; nil when
the style is not drawn by slices (it cuts)."
  (let* ((p (min 1.0 (max 0.0 progress)))
         (x (round (* p w))) (y (round (* p h)))
         (direction (vellumory-present--direction transition))
         (vertical (equal (alist-get 'dimension transition) "V"))
         (outward (equal (alist-get 'motion transition) "O"))
         (rows
          (pcase (alist-get 'style transition)
            ("Wipe" (pcase direction
                      (0 `(((new 0 0 ,x ,h) (old ,x 0 ,(- w x) ,h))))
                      (180 `(((old 0 0 ,(- w x) ,h) (new ,(- w x) 0 ,x ,h))))
                      (270 `(((new 0 0 ,w ,y)) ((old 0 ,y ,w ,(- h y)))))
                      (_ `(((old 0 0 ,w ,(- h y))) ((new 0 ,(- h y) ,w ,y))))))
            ("Push" (pcase direction
                      (0 `(((new ,(- w x) 0 ,x ,h) (old 0 0 ,(- w x) ,h))))
                      (180 `(((old ,x 0 ,(- w x) ,h) (new 0 0 ,x ,h))))
                      (270 `(((new 0 ,(- h y) ,w ,y)) ((old 0 0 ,w ,(- h y)))))
                      (_ `(((old 0 ,y ,w ,(- h y))) ((new 0 0 ,w ,y))))))
            ("Cover" (pcase direction
                       (0 `(((new ,(- w x) 0 ,x ,h) (old ,x 0 ,(- w x) ,h))))
                       (180 `(((old 0 0 ,(- w x) ,h) (new 0 0 ,x ,h))))
                       (270 `(((new 0 ,(- h y) ,w ,y)) ((old 0 ,y ,w ,(- h y)))))
                       (_ `(((old 0 0 ,w ,(- h y))) ((new 0 0 ,w ,y))))))
            ("Uncover" (pcase direction
                         (0 `(((new 0 0 ,x ,h) (old 0 0 ,(- w x) ,h))))
                         (180 `(((old ,x 0 ,(- w x) ,h) (new ,(- w x) 0 ,x ,h))))
                         (270 `(((new 0 0 ,w ,y)) ((old 0 0 ,w ,(- h y)))))
                         (_ `(((old 0 ,y ,w ,(- h y))) ((new 0 ,(- h y) ,w ,y))))))
            ("Split"
             ;; Outward: the new page opens from the middle; inward: from both edges.
             (let* ((size (if vertical w h)) (open (if vertical x y))
                    (a (if outward (/ (- size open) 2) (/ open 2)))
                    (b (if outward open (- size (* 2 a))))
                    (bands (if outward `((old 0 ,a) (new ,a ,b) (old ,(+ a b) ,(- size a b)))
                             `((new 0 ,a) (old ,a ,b) (new ,(+ a b) ,(- size a b))))))
               (if vertical
                   (list (mapcar (pcase-lambda (`(,which ,start ,length)) (list which start 0 length h)) bands))
                 (mapcar (pcase-lambda (`(,which ,start ,length)) (list (list which 0 start w length))) bands))))
            ("Box"
             ;; Outward: a growing box of the new page; inward: the old page shrinks.
             (let* ((inner (if outward 'new 'old)) (outer (if outward 'old 'new))
                    (bw (if outward x (- w x))) (bh (if outward y (- h y)))
                    (ax (/ (- w bw) 2)) (ay (/ (- h bh) 2)))
               `(((,outer 0 0 ,w ,ay))
                 ((,outer 0 ,ay ,ax ,bh) (,inner ,ax ,ay ,bw ,bh) (,outer ,(+ ax bw) ,ay ,(- w ax bw) ,bh))
                 ((,outer 0 ,(+ ay bh) ,w ,(- h ay bh))))))
            ("Blinds"
             (let ((n vellumory-present--blinds) bands)
               (dotimes (i n)
                 (let* ((size (if vertical w h))
                        (start (/ (* i size) n)) (length (- (/ (* (1+ i) size) n) start))
                        (open (round (* p length))))
                   (push (list start open (- length open)) bands)))
               (setq bands (nreverse bands))
               (if vertical
                   (list (cl-mapcan (pcase-lambda (`(,start ,open ,rest))
                                      (list (list 'new start 0 open h) (list 'old (+ start open) 0 rest h)))
                                    bands))
                 (cl-mapcan (pcase-lambda (`(,start ,open ,rest))
                              (list (list (list 'new 0 start w open)) (list (list 'old 0 (+ start open) w rest))))
                            bands)))))))
    ;; Empty slices and rows draw nothing; leave them out.
    (delq nil (mapcar (lambda (row)
                        (let ((row (seq-filter (pcase-lambda (`(,_ ,_ ,_ ,width ,height)) (and (> width 0) (> height 0)))
                                               row)))
                          row))
                      rows))))

(defun vellumory-present--animate-blended (page transition new generation)
  "Draw TRANSITION from the page on screen to PAGE in composed frames.
NEW is PAGE's (IMAGE W H).  The worker composes the frames from both
rasters, then PAGE is drawn alone.  One frame is asked for at a time, for
the moment it is asked; each is drawn on arrival.  The animation stops
when the presentation's generation is no longer GENERATION."
  (pcase-let* ((`(,_ ,old-image ,_ ,_) vellumory-present--shown)
               (`(,image ,w ,h) new)
               (`(,_ ,_ ,left ,top) (vellumory-present--fit page))
               ;; /Di: degrees, or None (Fly: scaling only).
               (direction (let ((di (alist-get 'direction transition)))
                            (cond ((numberp di) (mod (round di) 360)) ((equal di "None") di) (t 0))))
               (request `((old . ,(image-property old-image :file)) (new . ,(image-property image :file))
                          (style . ,(alist-get 'style transition)) (direction . ,direction)
                          (motion . ,(or (alist-get 'motion transition) "I"))
                          (scale . ,(or (alist-get 'scale transition) 1.0))
                          (opaque . ,(if (eq (alist-get 'opaque transition) t) t :false))
                          (priority . 0)))
               (scale (buffer-local-value 'vellumory--scale vellumory-present--source))
               (canvas (and vellumory-present-canvas (image-type-available-p 'canvas)))
               (request (if canvas
                            `((format . "argb")
                              (slot . ,(or vellumory-present--slot
                                           (setq vellumory-present--slot (cl-incf vellumory-present--slots))))
                              ,@request)
                          request))
               (duration (max 0.05 (or (alist-get 'duration transition) 1.0)))
               (start (float-time)) (buffer (current-buffer)) (waiting nil))
    (when (timerp vellumory-present--animation) (cancel-timer vellumory-present--animation))
    (setq vellumory-present--frames 0)
    (cl-labels ((finish ()
                  (cancel-timer vellumory-present--animation)
                  (setq vellumory-present--animation nil)
                  (vellumory-present--draw-rows page `(((new 0 0 ,w ,h))) `((new ,image ,w ,h)) left top)
                  (vellumory-present--drop-frame)
                  (setq vellumory-present--shown (list page image w h))
                  (vellumory-present--arm-duration page generation)
                  (vellumory-present--refit-after-transition))
                (ask ()
                  ;; The frame for this moment, unless one is being made or it is over.
                  (let ((progress (/ (- (float-time) start) duration)))
                    (unless (or waiting (>= progress 1.0))
                      (setq waiting t)
                      (with-current-buffer vellumory-present--source
                        (vellumory--rpc "transition-frame" `((progress . ,progress) ,@request) #'arrived)))))
                (arrived (reply)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (setq waiting nil)
                      (when (and (= generation vellumory-present--generation) vellumory-present--animation)
                        (if-let* ((path (alist-get 'path (alist-get 'result reply))))
                            (progn
                              ;; The worker composes the next frame while this one is drawn.
                              (ask)
                              (cl-incf vellumory-present--frames)
                              (if (and canvas vellumory-present--frame)
                                  ;; The same file, rewritten: reread into the same pixels.
                                  (canvas-refresh vellumory-present--frame t)
                                (let ((frame (if canvas
                                                 (list 'image :type 'canvas :id (gensym "vellumory-frame-")
                                                       :data-width (round (* scale w))
                                                       :data-height (round (* scale h))
                                                       :file path :scale (/ 1.0 scale) :ascent 100
                                                       :transform-smoothing (/= scale 1.0))
                                               (create-image path 'pbm nil :scale 1.0 :width w :height h
                                                             :ascent 100 :transform-smoothing (/= scale 1.0)))))
                                  (vellumory-present--draw-rows page `(((frame 0 0 ,w ,h))) `((frame ,frame ,w ,h))
                                                                left top)
                                  (vellumory-present--drop-frame)
                                  (setq vellumory-present--frame frame))))
                          ;; No frame (a raster evicted meanwhile): the page cuts
                          ;; in, and why is said, unless the frame was cancelled.
                          (let ((error (alist-get 'error reply)))
                            (unless (or (null error) (equal error "cancelled"))
                              (message "Vellumory: transition frame: %s" error)))
                          (finish)))))))
      (setq vellumory-present--animation
            (run-at-time
             0 (/ 1.0 60)
             (lambda ()
               (if (not (buffer-live-p buffer)) (cancel-timer vellumory-present--animation)
                 (with-current-buffer buffer
                   (cond
                    ((/= generation vellumory-present--generation)
                     (cancel-timer vellumory-present--animation))
                    ((>= (/ (- (float-time) start) duration) 1.0) (finish))
                    (t (ask)))))))))))

(defun vellumory-present--animate (page transition new generation)
  "Draw TRANSITION from the page on screen to PAGE, then PAGE alone.
NEW is PAGE's (IMAGE W H).  Blended styles are drawn in frames the worker
composes, the others by moving slices of both pages.  The animation stops
when the presentation's generation is no longer GENERATION."
  (if (member (alist-get 'style transition) vellumory-present--blended)
      (vellumory-present--animate-blended page transition new generation)
    (vellumory-present--animate-sliced page transition new generation)))

(defun vellumory-present--animate-sliced (page transition new generation)
  "Draw TRANSITION from the page on screen to PAGE by moving slices of both."
  (pcase-let* ((`(,_ ,old-image ,_ ,_) vellumory-present--shown)
               (`(,image ,w ,h) new)
               (`(,_ ,_ ,left ,top) (vellumory-present--fit page))
               (images `((old ,old-image ,w ,h) (new ,image ,w ,h)))
               (duration (max 0.05 (or (alist-get 'duration transition) 1.0)))
               (start (float-time)) (buffer (current-buffer)))
    (when (timerp vellumory-present--animation) (cancel-timer vellumory-present--animation))
    (setq vellumory-present--animation
          (run-at-time
           0 (/ 1.0 60)
           (lambda ()
             (if (not (buffer-live-p buffer)) (cancel-timer vellumory-present--animation)
               (with-current-buffer buffer
                 (let ((progress (/ (- (float-time) start) duration)))
                   (cond
                    ((/= generation vellumory-present--generation)
                     (cancel-timer vellumory-present--animation))
                    ((>= progress 1.0)
                     (cancel-timer vellumory-present--animation)
                     (setq vellumory-present--animation nil)
                     (vellumory-present--draw-rows page `(((new 0 0 ,w ,h))) images left top)
                     (setq vellumory-present--shown (list page image w h))
                     (vellumory-present--arm-duration page generation)
                     (vellumory-present--refit-after-transition))
                    (t (vellumory-present--draw-rows
                        page (vellumory-present--compose transition progress w h) images left top)))))))))))

(defun vellumory-present--page-info (page &optional callback)
  "PAGE's page-info report, asking the worker the first time; then CALLBACK."
  (if-let* ((known (assq page vellumory-present--info)))
      (when callback (funcall callback (cdr known)))
    (let ((buffer (current-buffer)))
      (with-current-buffer vellumory-present--source
        (vellumory--rpc "page-info" `((page . ,page))
                        (lambda (reply)
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (let ((info (alist-get 'result reply)))
                                (push (cons page info) vellumory-present--info)
                                (when callback (funcall callback info)))))))))))

(defun vellumory-present--transition-to (page)
  "Return the /Trans to draw on the way to PAGE, or nil to cut.
A transition is drawn when `vellumory-present-transitions' is non-nil, its
style is one drawn here (by slices or in composed frames), PAGE's report
is known, its duration (if given) is positive, and another page of the
same size is on screen."
  (when-let* ((vellumory-present-transitions)
              (shown vellumory-present--shown)
              ((/= (car shown) page))
              (transition (alist-get 'transition (alist-get page vellumory-present--info)))
              ((consp transition))
              ((member (alist-get 'style transition)
                       (append vellumory-present--sliced vellumory-present--blended)))
              ((> (or (alist-get 'duration transition) 1) 0))
              (fit (vellumory-present--fit page))
              ((equal (list (nth 0 fit) (nth 1 fit)) (cddr shown))))
    transition))

(defun vellumory-present--paint ()
  "The background colour: black around pages, or white for a white screen."
  (let ((colour (if (eq vellumory-present--state 'white) "white" vellumory-present-background)))
    (unless (equal colour vellumory-present--colour)
      (mapc #'face-remap-remove-relative vellumory-present--background)
      (setq vellumory-present--background (list (face-remap-add-relative 'default :background colour)
                                                (face-remap-add-relative 'fringe :background colour))
            vellumory-present--colour colour)
      (when-let* ((window (vellumory-present--window)))
        (set-frame-parameter (window-frame window) 'background-color colour)))))

(defun vellumory-present--show ()
  "Draw the current page (or screen) and render its neighbours ahead."
  (dolist (timer (list vellumory-present--timer vellumory-present--animation))
    (when (timerp timer) (cancel-timer timer)))
  (setq vellumory-present--timer nil vellumory-present--animation nil)
  (vellumory-present--drop-frame)
  (unless (buffer-live-p vellumory-present--source)
    (kill-buffer (current-buffer))
    (user-error "The presented document has been closed"))
  (vellumory-present--paint)
  (let ((generation (cl-incf vellumory-present--generation))
        (buffer (current-buffer)) (page vellumory-present--page)
        (fit (vellumory-present--fit vellumory-present--page)))
    (if (or (not (eq vellumory-present--state 'normal)) (null fit))
        (vellumory-present--draw page nil)
      (vellumory-present--render
       page (car fit) (nth 1 fit) 0
       (lambda (path)
         (when (and path (buffer-live-p buffer))
           (with-current-buffer buffer
             (when (= generation vellumory-present--generation)
               (cond
                ;; Rendered for a window size that has changed meanwhile (a new
                ;; frame settling): drawn now it would be scaled, so render again.
                ((not (equal (vellumory-present--fit page) fit)) (vellumory-present--show))
                ((when-let* ((transition (vellumory-present--transition-to page)))
                   (pcase-let ((`(,w ,h ,_ ,_) fit))
                     (vellumory-present--animate page transition
                                                 (list (vellumory-present--image page path w h) w h) generation))
                   t))
                (t (vellumory-present--draw page path)
                   (vellumory-present--arm-duration page generation))))))))
      ;; Ahead: the next page first, then the previous one, with their reports.
      (vellumory-present--page-info page)
      (dolist (other (list (1+ page) (1- page)))
        (when (< -1 other (length (vellumory-present--pages)))
          (vellumory-present--page-info other)
          (with-current-buffer vellumory-present--source (vellumory--ensure-links other))
          (when-let* ((ahead (vellumory-present--fit other)))
            (vellumory-present--render other (car ahead) (nth 1 ahead) 900 #'ignore)))))))

(defun vellumory-present--arm-duration (page generation)
  "Advance after PAGE's /Dur, if it has one and nothing changed meanwhile.
Nothing changed while the presentation's generation is still GENERATION.
Only when `vellumory-present-follow-durations' is non-nil."
  (when vellumory-present-follow-durations
    (let ((buffer (current-buffer)))
      (cl-flet ((arm (seconds)
                  (when (and (numberp seconds) (>= seconds 0) (buffer-live-p buffer))
                    (with-current-buffer buffer
                      (when (= generation vellumory-present--generation)
                        (setq vellumory-present--timer
                              (run-at-time seconds nil
                                           (lambda ()
                                             (when (buffer-live-p buffer)
                                               (with-current-buffer buffer
                                                 (when (= generation vellumory-present--generation)
                                                   (vellumory-present-next))))))))))))
        (vellumory-present--page-info page (lambda (info) (arm (alist-get 'duration info))))))))

;;;; Commands

(defun vellumory-present--go (page)
  "Show zero-based PAGE, or the end screen past the last page."
  (let ((count (length (vellumory-present--pages))))
    (setq vellumory-present--digits "")
    (if (>= page count)
        (setq vellumory-present--state 'end vellumory-present--page (1- count))
      (setq vellumory-present--state 'normal vellumory-present--page (max 0 page))))
  (vellumory-present--show))

(defun vellumory-present-next ()
  "Next page; past the last, the end screen; from there, leave."
  (interactive)
  (pcase vellumory-present--state
    ('end (vellumory-present-quit))
    ((or 'black 'white) (vellumory-present--go vellumory-present--page))
    (_ (vellumory-present--go (1+ vellumory-present--page)))))

(defun vellumory-present-previous ()
  "Previous page."
  (interactive)
  (if (memq vellumory-present--state '(end black white))
      (vellumory-present--go vellumory-present--page)
    (vellumory-present--go (1- vellumory-present--page))))

(defun vellumory-present-first () "First page." (interactive) (vellumory-present--go 0))
(defun vellumory-present-last () "Last page." (interactive)
  (vellumory-present--go (1- (length (vellumory-present--pages)))))

(defun vellumory-present--blank (state)
  "Blank the screen to STATE, `black' or `white'; in STATE already, return."
  (setq vellumory-present--state (if (eq vellumory-present--state state) 'normal state))
  (vellumory-present--show))

(defun vellumory-present-black () "Blank the screen to black; again returns." (interactive)
  (vellumory-present--blank 'black))
(defun vellumory-present-white () "Blank the screen to white; again returns." (interactive)
  (vellumory-present--blank 'white))

(defun vellumory-present-digit ()
  "Type a page number; RET goes there."
  (interactive)
  (let ((digit (char-to-string (event-basic-type last-command-event))))
    (when (string-match-p "\\`[0-9]\\'" digit)
      (setq vellumory-present--digits (concat vellumory-present--digits digit))
      (message "Go to page %s (RET)" vellumory-present--digits))))

(defun vellumory-present-return ()
  "Go to the page number typed, or advance."
  (interactive)
  (if (string-empty-p vellumory-present--digits)
      (vellumory-present-next)
    (let ((page (string-to-number vellumory-present--digits)))
      (setq vellumory-present--digits "")
      (if (<= 1 page (length (vellumory-present--pages)))
          (progn (message nil) (vellumory-present--go (1- page)))
        (message "No page %d" page)))))

(defun vellumory-present-backspace ()
  "Delete a digit being typed, or go back a page."
  (interactive)
  (if (string-empty-p vellumory-present--digits)
      (vellumory-present-previous)
    (setq vellumory-present--digits (substring vellumory-present--digits 0 -1))
    (message "Go to page %s (RET)" vellumory-present--digits)))

(defun vellumory-present-click (event)
  "Follow the link under EVENT; elsewhere, advance."
  (interactive "e")
  (let* ((posn (event-start event))
         (page (and (consp (posn-object posn)) (eq (car (posn-object posn)) 'image)
                    (get-text-property (posn-point posn) 'vellumory-present-page)))
         (xy (posn-object-x-y posn))
         (link (and page xy
                    (let ((k (/ (float (alist-get 'width (aref (vellumory-present--pages) page)))
                                (car (image-size (posn-image posn) t)))))
                      (with-current-buffer vellumory-present--source
                        (vellumory--link-at page (* k (car xy)) (* k (cdr xy))))))))
    (cond ((and link (alist-get 'page link)) (vellumory-present--go (alist-get 'page link)))
          (link (with-current-buffer vellumory-present--source (vellumory--visit-link link)))
          (t (vellumory-present-next)))))

(defun vellumory-present-wheel (event)
  "Change the page by the wheel EVENT: down or right forward, else back.
A touchpad's stream of turns changes it once per 0.3 s at most."
  (interactive "e")
  (when (> (- (float-time) vellumory-present--last-wheel) 0.3)
    (setq vellumory-present--last-wheel (float-time))
    (if (memq (event-basic-type event) '(wheel-down wheel-right mouse-5))
        (vellumory-present-next)
      (vellumory-present-previous))))

(defun vellumory-present-any-key ()
  "On a blank or end screen any key returns to the page."
  (interactive)
  (if (memq vellumory-present--state '(black white end))
      (vellumory-present--go vellumory-present--page)
    (message "%s" (substitute-command-keys
                   "\\<vellumory-presentation-mode-map>\\[vellumory-present-next] next  \
\\[vellumory-present-previous] previous  \\[vellumory-present-black] black  \
\\[vellumory-present-white] white  \\[vellumory-present-quit] leave"))))

(defun vellumory-present-quit ()
  "Leave the presentation; the reader shows the page last presented."
  (interactive)
  (if (not (string-empty-p vellumory-present--digits))
      (progn (setq vellumory-present--digits "") (message nil))
    (let ((source vellumory-present--source) (page vellumory-present--page))
      (kill-buffer (current-buffer))
      (when (buffer-live-p source)
        (when-let* ((window (get-buffer-window source t)))
          (select-frame-set-input-focus (window-frame window))
          (select-window window)
          (with-current-buffer source (vellumory-goto-page (1+ page))))))))

(defvar vellumory-presentation-mode-map
  (let ((map (make-keymap)))
    (define-key map [remap self-insert-command] #'vellumory-present-any-key)
    (dolist (key '("SPC" "<right>" "<down>" "<next>" "n" "j" "l" "J" "L" "C-n" "C-v"))
      (keymap-set map key #'vellumory-present-next))
    (dolist (key '("S-SPC" "<left>" "<up>" "<prior>" "p" "h" "k" "H" "K" "C-p" "M-v"))
      (keymap-set map key #'vellumory-present-previous))
    (dolist (key '("<home>" "<" "M-<" "g")) (keymap-set map key #'vellumory-present-first))
    (dolist (key '("<end>" ">" "M->" "G")) (keymap-set map key #'vellumory-present-last))
    (dolist (key '("b" "B" "." "<kp-decimal>")) (keymap-set map key #'vellumory-present-black))
    (dolist (key '("w" "W" ",")) (keymap-set map key #'vellumory-present-white))
    (dotimes (digit 10)
      (keymap-set map (number-to-string digit) #'vellumory-present-digit)
      (keymap-set map (format "<kp-%d>" digit) #'vellumory-present-digit))
    (dolist (key '("RET" "<kp-enter>" "<return>")) (keymap-set map key #'vellumory-present-return))
    (dolist (key '("DEL" "<backspace>")) (keymap-set map key #'vellumory-present-backspace))
    (dolist (key '("q" "Q" "<escape>" "C-g")) (keymap-set map key #'vellumory-present-quit))
    (dolist (prefix '(nil vellumory-link))
      (cl-flet ((bind (event command) (define-key map (if prefix (vector prefix event) (vector event)) command)))
        (bind 'down-mouse-1 #'ignore) (bind 'mouse-1 #'vellumory-present-click)
        (bind 'down-mouse-3 #'ignore) (bind 'mouse-3 #'vellumory-present-previous)
        (dolist (event '(wheel-up wheel-down wheel-left wheel-right mouse-4 mouse-5))
          (bind event #'vellumory-present-wheel))))
    map))

(define-derived-mode vellumory-presentation-mode nil "Presentation"
  "A document shown one page at a time, full screen.
\\{vellumory-presentation-mode-map}"
  (setq-local mode-line-format nil header-line-format nil tab-line-format nil
              cursor-type nil cursor-in-non-selected-windows nil buffer-read-only t
              bidi-display-reordering nil
              truncate-lines t line-spacing nil show-trailing-whitespace nil
              indicate-empty-lines nil indicate-buffer-boundaries nil
              ;; Without fringes a line as wide as the window loses its last
              ;; column to the truncation glyph: keep 1 px ones, painted over.
              left-margin-width 0 right-margin-width 0 left-fringe-width 1 right-fringe-width 1
              overflow-newline-into-fringe t
              fringe-indicator-alist (mapcar (lambda (indicator) (list (car indicator))) fringe-indicator-alist)
              make-cursor-line-fully-visible nil scroll-margin 0 auto-hscroll-mode nil
              ;; A line highlight (Doom's global one, too) would tint the margins.
              hl-line-range-function (lambda () (cons (point-min) (point-min))))
  (add-hook 'window-size-change-functions #'vellumory-present--resized nil t)
  (add-hook 'kill-buffer-hook #'vellumory-present--cleanup nil t)
  ;; Collections wait for the end of a transition, as for a reading step.
  (add-hook 'pre-command-hook #'vellumory--defer-gc nil t))

(with-eval-after-load 'vellumory
  (add-to-list 'vellumory--animation-timers 'vellumory-present--animation))

(defvar-local vellumory-present--refit nil
  "Whether the window changed size during a transition, to fit it after.")

(defun vellumory-present--resized (window)
  "Show the presentation in WINDOW again, fitted to its new size.
During a transition, once it ends: showing the page anew stops it, and a
long message growing the echo area cut transitions short."
  (with-current-buffer (window-buffer window)
    (when (buffer-live-p vellumory-present--source)
      (if (memq vellumory-present--animation timer-list)
          (setq vellumory-present--refit t)
        (vellumory-present--show)))))

(defun vellumory-present--refit-after-transition ()
  "Fit the page to its window if it changed size during the transition."
  (when vellumory-present--refit
    (setq vellumory-present--refit nil)
    (vellumory-present--show)))

(defun vellumory-present--cleanup ()
  "Stop the presentation's timers and delete its full-screen frames.
A frame is kept when it is the only visible one."
  (dolist (timer (list vellumory-present--timer vellumory-present--animation))
    (when (timerp timer) (cancel-timer timer)))
  (vellumory-present--drop-frame)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (let ((frame (window-frame window)))
      (when (and (frame-live-p frame) (frame-parameter frame 'vellumory-presentation)
                 (cdr (visible-frame-list)))
        (delete-frame frame)))))

(defun vellumory-present--monitor ()
  "Geometry (X Y W H) of the monitor to present on."
  (let* ((here (frame-monitor-attributes))
         (other (and (eq vellumory-present-monitor 'other)
                     (seq-find (lambda (monitor) (not (memq (selected-frame) (alist-get 'frames monitor))))
                               (display-monitor-attributes-list)))))
    (alist-get 'geometry (or other here))))

;;;###autoload
(defun vellumory-present--covers-monitor-p (frame)
  "Whether FRAME is as wide as its monitor, as a full-screen frame is."
  (let ((edges (frame-edges frame 'native-edges))
        (monitor (nth 2 (alist-get 'geometry (frame-monitor-attributes frame)))))
    (and monitor (>= (- (nth 2 edges) (nth 0 edges)) monitor))))

(defun vellumory-present (&optional from-start)
  "Present this document full screen from the current page (FROM-START: the first).
\\<vellumory-presentation-mode-map>\\[vellumory-present-next] next, \
\\[vellumory-present-previous] previous, \\[vellumory-present-black] black screen, \
\\[vellumory-present-quit] leave."
  (interactive "P")
  (unless (and (derived-mode-p 'vellumory-mode) vellumory--pages) (user-error "No document to present"))
  (let* ((source (current-buffer))
         (page (if from-start 0 (or vellumory--current-page 0)))
         (name (buffer-name))
         (buffer (get-buffer-create (format "*Presentation: %s*" name)))
         (geometry (vellumory-present--monitor))
         frame)
    (with-current-buffer buffer
      (vellumory-presentation-mode)
      (setq vellumory-present--source source vellumory-present--page page)
      ;; A new frame shows the current buffer: never the reader, whose layout
      ;; follows the width of its windows.
      (setq frame (make-frame `((name . ,(format "%s — presentation" name))
                                (vellumory-presentation . t) (fullscreen . fullboth)
                                ,@(and geometry `((left . ,(nth 0 geometry)) (top . ,(nth 1 geometry))))
                                (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                                (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                                (left-fringe . 1) (right-fringe . 1) (internal-border-width . 0)
                                (background-color . ,vellumory-present-background)
                                (unsplittable . t)))))
    (select-frame-set-input-focus frame)
    ;; A window manager may drop a fullscreen request made before the window is
    ;; mapped (xfwm4 did, for pgtk on X): ask again once it shows.  macOS drops
    ;; one made while another window leaves full screen, keeping the parameter:
    ;; there, ask again for as long as the frame does not cover its monitor.
    (run-with-timer 0.2 nil (lambda ()
                              (when (and (frame-live-p frame)
                                         (not (eq (frame-parameter frame 'fullscreen) 'fullboth)))
                                (set-frame-parameter frame 'fullscreen 'fullboth))))
    (when (eq (window-system frame) 'ns)
      (dolist (delay '(1.5 3 5))
        (run-with-timer delay nil (lambda ()
                                    (when (and (frame-live-p frame) (not (vellumory-present--covers-monitor-p frame)))
                                      (set-frame-parameter frame 'fullscreen nil)
                                      (set-frame-parameter frame 'fullscreen 'fullboth))))))
    (let ((window (frame-root-window frame)))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      (set-window-fringes window 1 1)
      (set-window-scroll-bars window 0 nil 0 nil)
      (select-window window))
    (with-current-buffer buffer (vellumory-present--go page))
    buffer))

(provide 'vellumory-present)
;;; vellumory-present.el ends here
