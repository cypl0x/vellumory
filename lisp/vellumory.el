;;; vellumory.el --- Incremental native document reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Wolfhard Prell

;; Author: Wolfhard Prell <mail@wolfhard.net>
;; URL: https://github.com/cypl0x/vellumory
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0
;; Keywords: multimedia, files, tools

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
;; A continuous-scrolling PDF (and XPS/EPUB/CBZ/FB2) reader built on Emacs'
;; own display engine and an asynchronous Lean/MuPDF worker.
;;
;; Pages are laid out as lines of image tiles.  Only tiles intersecting a
;; window are rasterized at full device resolution; while the view moves
;; faster than the worker, whole-page previews are sliced into place and
;; "refocused" once motion stops.  Rendering is parallel, prioritized from
;; a reading focus line, cancellable, and never touches the source file.
;;
;; Visit a PDF after `vellumory-install', or use `vellumory-open'.  Press ?
;; for commands.

;;; Code:
;; Primitives of Emacs built with image support (the only kind Vellumory runs in).
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function clear-image-cache "image.c" (&optional filter animation-cache))
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'vellumory-install)
(require 'image)
(require 'color)
(require 'bookmark)
(require 'easymenu)
(require 'filenotify)
(require 'vellumory-navigation)
;; Bound to ? below; it must load when the generated autoloads are not.
(autoload 'vellumory-dispatch "vellumory-dispatch" nil t)
(autoload 'vellumory-reflow "vellumory-reflow" nil t)
(require 'vellumory-thumbnails)
(require 'vellumory-search)
(require 'vellumory-select)
(require 'vellumory-edit)
(require 'vellumory-details)
(require 'vellumory-validate)
(require 'vellumory-syntax)
(require 'vellumory-diff)
(require 'vellumory-sync)
(require 'vellumory-metadata)
(require 'vellumory-annotate)
(require 'vellumory-form)
(require 'vellumory-present)
(require 'vellumory-caret)
(require 'vellumory-ecosystem)
(require 'vellumory-follow)

(defun vellumory--bundled-program ()
  "The worker installed next to this library, if any.
Nix installs Lisp under share/emacs/site-lisp/vellumory and the worker in bin;
a source checkout builds it in .lake/build/bin."
  (when-let* ((library (or load-file-name buffer-file-name (locate-library "vellumory"))))
    (let ((directory (file-name-directory library)))
      (cl-find-if #'file-executable-p
                  (list (expand-file-name "../../../../bin/vellumory" directory)
                        (expand-file-name "../.lake/build/bin/vellumory" directory))))))

(defgroup vellumory-reader nil "Reading: rendering, scrolling, positions and appearance."
  :group 'vellumory)

(defcustom vellumory-program (or (vellumory--bundled-program) "vellumory")
  "Lean worker executable." :type 'file)
(defcustom vellumory-strip-height 768
  "Raster strip height in logical pixels.
Each strip is a line of images; redisplay costs about 1 ms per line in view,
so tall strips scroll cheaply, while idle prefetch already renders as much
around the view (measured on ISO 32000: 192 px, 2.9 ms mean scroll step and
21–27 ms worst; 768 px, 1.7 ms and under 5 ms, no more pixels rendered)."
  :type 'natnum)
(defcustom vellumory-tile-width 640
  "Tile width in logical pixels when pages are wider than the window." :type 'natnum)
(defcustom vellumory-page-gap 14 "Gap between pages in pixels." :type 'natnum)
(defcustom vellumory-spread nil
  "How pages are placed side by side.
nil shows single pages, `book' the first page alone, then 2–3, 4–5, as a
printed book opens, and `pairs' 1–2, 3–4.  \\[vellumory-cycle-spread] cycles
them."
  :type '(choice (const :tag "Single pages" nil) (const :tag "Book" book) (const :tag "Pairs" pairs)))
(defcustom vellumory-spread-gap 8 "Pixels between the pages of a spread." :type 'natnum)
(defcustom vellumory-follow-document-display t
  "Open documents as they ask to be shown.
That is two pages for a /PageLayout of two columns or pages (unless
`vellumory-spread' is set), the contents sidebar for /PageMode
/UseOutlines." :type 'boolean)
(defcustom vellumory-refine-delay 0.09
  "Seconds without motion before deferred tiles are rendered sharp." :type 'number)
(defcustom vellumory-immediate-budget 14
  "Render sharp tiles immediately, even while moving, below this many milliseconds.
The estimate comes from the worker's measured cost for this document."
  :type 'number)
(defcustom vellumory-preview-scale 0.25
  "Resolution of whole-page previews relative to sharp tiles." :type 'number)
(defcustom vellumory-prefetch-screens '(1.0 . 0.5)
  "Screens rendered ahead of and behind the direction of motion when idle."
  :type '(cons number number))
(defcustom vellumory-retain-screens 2.0
  "Decoded images within this many screens of a window are kept." :type 'number)
(defcustom vellumory-theme nil
  "Page colours: nil, the original; `tint', the theme's foreground and background;
`invert', inverted luminance (dark pages, hues kept); `auto', inverted while
the Emacs theme is dark and original while it is light.  \\[vellumory-night-mode]
inverts one document's pages."
  :type '(choice (const :tag "Original" nil) (const :tag "Theme colours" tint)
                 (const :tag "Inverted luminance" invert)
                 (const :tag "Inverted with a dark theme" auto)))
(defcustom vellumory-smooth-scroll t "Animate screen-wise scrolling." :type 'boolean)
(defcustom vellumory-smooth-scroll-duration 0.11 "Seconds per animated scroll." :type 'number)
(defcustom vellumory-header-line t "Show document status in the header line." :type 'boolean)
(defcustom vellumory-watch-file t "Reload changed documents after a brief quiet period." :type 'boolean)
(defcustom vellumory-wheel-step nil
  "Pixels a notch of the mouse wheel scrolls, or nil for GTK's distance.
That is the window's height to the power 2/3, about 96 pixels in a window
900 pixels high.  Fast notches scroll as far each; trackpads scroll exactly."
  :type '(choice (const :tag "As GTK" nil) (natnum :tag "Pixels")))
(defcustom vellumory-reflow-narrow-width nil
  "Show documents reflowed in windows narrower than this many columns.
nil never does; with a number, a window too narrow for the pages shows the
document as `vellumory-reflow' does, and the pages again once it is wider."
  :type '(choice (const :tag "Never" nil) (natnum :tag "Columns")))
(defcustom vellumory-state-directory
  (expand-file-name "vellumory" (or (getenv "XDG_STATE_HOME") "~/.local/state"))
  "Directory for exact reading positions." :type 'directory)
(defcustom vellumory-cache-directory
  (expand-file-name "vellumory" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Content-addressed cache for remote and in-memory documents." :type 'directory)

(defface vellumory-paper '((t :background "white")) "Unrendered page area.")
(defface vellumory-desk '((((background dark)) :background "#2b2b2b")
                          (t :background "#d9d9d9"))
  "Background between pages.")

(defcustom vellumory-gc-threshold (* 64 1024 1024)
  "`gc-cons-threshold' while laying out pages; garbage is collected when idle."
  :type 'natnum)

(defmacro vellumory--deferring-gc (&rest body)
  "Run BODY without collecting garbage in the middle of it.
Restoring the threshold re-applies what BODY allocated (alloc.c
`bump_consing_until_gc'), so this never reduces how often Emacs
collects; it merges several collections during one large burst, such as
laying out thousands of strips, into one afterwards."
  (declare (indent 0) (debug t))
  `(let ((gc-cons-threshold (max gc-cons-threshold vellumory-gc-threshold))) ,@body))

(defun vellumory--call-deferring-gc (fn)
  "Call FN as `vellumory--deferring-gc' would run its body."
  (vellumory--deferring-gc (funcall fn)))

(defcustom vellumory-gc-idle-delay 0.5
  "Seconds reading rests before the garbage it left is collected.
Moves of the view keep collection out of their redisplay until then, up to
`vellumory-gc-threshold'; keys tapped faster or held run free of it.  nil
lets Emacs collect as usual.  Left to `gcmh-mode' when that is on (Doom)."
  :type '(choice (const :tag "As usual" nil) number))

(defvar vellumory--gc-saved nil
  "`gc-cons-threshold' before reading deferred collection, until it rests.")

(defvar vellumory--gc-activity 0.0 "When reading last moved a view or ran a command.")

(defvar vellumory--animation-timers '(vellumory--animation)
  "Buffer-local variables holding a timer while frames are being drawn.")

(defun vellumory--defer-gc ()
  "Keep garbage collection out of this move of the view and its redisplay.
A collection takes about 15 ms in an Emacs reading one paper; between a
step and its redisplay it makes that frame late (the screen benchmark's
17-20 ms steps, each with a collection).  Restoring the threshold at once
would collect right there (alloc.c `bump_consing_until_gc'), so collect
once reading rests, as `gcmh-mode' does; it, or Emacs's incremental
collector, is left to it.  Resting is measured from the last move, not by
an idle timer: moves made while Emacs is idle (animation frames, reloads,
Lisp run by emacsclient) would have one fire within them."
  (setq vellumory--gc-activity (float-time))
  (unless (or (null vellumory-gc-idle-delay) vellumory--gc-saved
              (bound-and-true-p gcmh-mode) (fboundp 'igc-info)
              (>= gc-cons-threshold vellumory-gc-threshold))
    (setq vellumory--gc-saved gc-cons-threshold
          gc-cons-threshold vellumory-gc-threshold)
    (run-with-timer vellumory-gc-idle-delay nil #'vellumory--collect-deferred)))

(defun vellumory--animating-p ()
  "Whether some buffer is drawing a scroll or transition's frames."
  (seq-some (lambda (buffer)
              (seq-some (lambda (variable)
                          (and (boundp variable) (timerp (buffer-local-value variable buffer))))
                        vellumory--animation-timers))
            (buffer-list)))

(defun vellumory--collect-deferred ()
  "Collect the garbage reading deferred, once it rests and draws no frames."
  (let ((rest (- (float-time) vellumory--gc-activity)))
    (cond ((null vellumory--gc-saved))
          ((or (< rest vellumory-gc-idle-delay) (vellumory--animating-p) (input-pending-p))
           (run-with-timer (max 0.05 (- vellumory-gc-idle-delay rest)) nil
                           #'vellumory--collect-deferred))
          (t
           ;; Unless something else has set it since.
           (when (eql gc-cons-threshold vellumory-gc-threshold)
             (setq gc-cons-threshold vellumory--gc-saved))
           (setq vellumory--gc-saved nil)
           ;; Now, when half of the usual allotment is spent: the next step
           ;; then rarely brings Emacs to its own collection.
           (garbage-collect-maybe 2)))))

(cl-defstruct (vellumory--row (:constructor vellumory--row-create))
  page top bottom position offset gap tiles)
(cl-defstruct (vellumory--tile (:constructor vellumory--tile-create))
  row page left right position (quality 0) image signature job)

(defvar-local vellumory--file nil "Local file the worker reads.")
(defvar-local vellumory--process nil)
(defvar-local vellumory--threads 2)
(defvar-local vellumory--pages nil)
(defvar-local vellumory--hash nil)
(defvar-local vellumory--rows [])
(defvar-local vellumory--page-rows [] "Index of each page's first row.")
(defvar-local vellumory--spread nil "Spread layout here: nil, `book' or `pairs'.")
(defvar-local vellumory--spread-of [] "Each page's spread: the pages shown with it, in order.")
(defvar-local vellumory--visible nil "Visible tiles, highest priority first.")
(defvar-local vellumory--visible-and-near nil "Tiles allowed to hold images.")
(defvar-local vellumory--inflight nil "Request id → job plist.")
(defvar-local vellumory--generation 0)
(defvar-local vellumory--zoom 1.0)
(defvar-local vellumory--width 0 "Layout width in logical pixels.")
(defvar-local vellumory--scale 1.0 "Device pixels per logical pixel.")
(defvar-local vellumory--margin 0)
(defvar-local vellumory--previews nil "Page → plist (:path :width :images).")
(defvar-local vellumory--cost 4.0 "Estimated milliseconds per megapixel.")
(defvar-local vellumory--last-motion 0.0)
(defvar-local vellumory--direction 1)
(defvar-local vellumory--geometry nil)
(defvar-local vellumory--live-images 0)
(defvar-local vellumory--timer nil)
(defvar-local vellumory--refine-timer nil)
(defvar-local vellumory--state-timer nil)
(defvar-local vellumory--animation nil)
(defvar-local vellumory--watch nil)
(defvar-local vellumory--reload-timer nil)
(defvar-local vellumory--status "Opening…")
(defvar-local vellumory--pending-position nil)
(defvar-local vellumory--last-position nil)
(defvar-local vellumory--window-positions nil
  "Where each window showed the document at the last refresh.
Elements are (WINDOW POSITION . SIZE), SIZE being the window's body size
then.  An element is kept while its window shows something else, as
`window-prev-buffers' keeps a text buffer's start.")
(defvar-local vellumory--placed nil
  "Windows newly showing the document placed by `vellumory--window-shown'.
They are kept until a redisplay.")
(defvar-local vellumory--rendered-pixels 0)
(defvar-local vellumory--wheel-remainder 0.0)
(defvar-local vellumory--source-file nil "File the buffer visits, if any.")

(defvar vellumory-page-change-functions nil
  "Abnormal hook run with the new zero-based page when the viewport's page changes.")
(defvar-local vellumory--current-page nil)

;;;; Worker protocol

;; One worker serves every document of the session: one process, one render
;; pool and one raster budget, its threads shared by priority.  Requests name
;; their buffer's document; replies return to the buffer that asked.

(defvar vellumory--worker nil "The worker process documents share.")
(defvar vellumory--worker-callbacks (make-hash-table :test #'eql)
  "Request id → (BUFFER . CALLBACK).")
(defvar vellumory--worker-serial 0 "Last request id, unique across documents.")
(defvar vellumory--worker-documents 0 "Last document id handed out.")
(defvar vellumory--worker-threads 2 "Render threads the worker reported.")
(defvar-local vellumory--doc nil "This buffer's document in the worker.")

(defun vellumory--rpc (method params callback &optional id)
  "Send METHOD with PARAMS to the worker for this buffer's document.
CALLBACK, if non-nil, gets the whole reply in this buffer.  ID is the
request id, by default a new one.  The methods \"hello\", \"cancel\" and
\"rest\" are sent without the document.  Return the id."
  (unless (process-live-p vellumory--process) (user-error "Document worker is not running"))
  (let ((id (or id (cl-incf vellumory--worker-serial))))
    (when callback (puthash id (cons (current-buffer) callback) vellumory--worker-callbacks))
    (process-send-string
     vellumory--process
     (concat (json-serialize
              `((id . ,id) (method . ,method)
                (params . ,(if (member method '("hello" "cancel" "rest")) (or params #s(hash-table))
                             (cons (cons 'doc vellumory--doc) params)))))
             "\n"))
    id))

(defun vellumory--filter (process chunk)
  "Consume CHUNK of arbitrarily fragmented JSON lines from PROCESS."
  (vellumory--call-deferring-gc (lambda () (vellumory--filter-1 process chunk))))

(defun vellumory--filter-1 (process chunk)
  "Dispatch the complete JSON lines of CHUNK from PROCESS.
Each reply goes to the callback registered for its id, in the buffer that
asked; the incomplete rest is kept in PROCESS's `fragment' property."
  (let ((data (concat (or (process-get process 'fragment) "") chunk))
        (start 0))
    (while (string-match "\n" data start)
      (let ((line (substring data start (match-beginning 0))))
        (setq start (match-end 0))
        (when-let* ((reply (condition-case err
                               (json-parse-string line :object-type 'alist :array-type 'array
                                                  :null-object nil :false-object nil)
                             (error (message "Vellumory: %s" (error-message-string err)) nil)))
                    (id (alist-get 'id reply))
                    (entry (gethash id vellumory--worker-callbacks)))
          (remhash id vellumory--worker-callbacks)
          (when (buffer-live-p (car entry))
            (with-current-buffer (car entry)
              (condition-case err (funcall (cdr entry) reply)
                (error (setq vellumory--status (error-message-string err))
                       (message "Vellumory: %s" vellumory--status))))))))
    (process-put process 'fragment (substring data start))))

(defun vellumory--sentinel (process event)
  "Forget the requests of PROCESS, a worker, once EVENT ends it.
Callbacks of killed buffers are dropped too.  The documents it served
drop their jobs and show in their status that the worker stopped, with
EVENT."
  (unless (process-live-p process)
    (when (eq process vellumory--worker) (setq vellumory--worker nil))
    ;; Only what was asked of this worker: a newer one may already be serving.
    (maphash (lambda (id entry)
               (when (or (not (buffer-live-p (car entry)))
                         (eq (buffer-local-value 'vellumory--process (car entry)) process))
                 (remhash id vellumory--worker-callbacks)))
             vellumory--worker-callbacks)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq process vellumory--process)
          (when vellumory--inflight (clrhash vellumory--inflight))
          (setq vellumory--search-job nil
                vellumory--status (concat "Worker stopped: " (string-trim event)))
          (force-mode-line-update))))))

(defun vellumory--start ()
  "Join the shared worker, starting it if none runs, with a document id of our own."
  (setq vellumory--inflight (make-hash-table :test #'eql)
        vellumory--doc (cl-incf vellumory--worker-documents))
  (let ((new (not (process-live-p vellumory--worker))))
    (when new
      (setq vellumory--worker
            (let ((default-directory (vellumory--existing-directory)))
              (make-process :name "vellumory" :command (list vellumory-program "serve")
                            :connection-type 'pipe :coding 'utf-8-unix :noquery t
                            :stderr (get-buffer-create " *Vellumory worker diagnostics*")
                            :filter #'vellumory--filter :sentinel #'vellumory--sentinel))))
    (setq vellumory--process vellumory--worker vellumory--threads vellumory--worker-threads)
    (when new
      (vellumory--rpc "hello" nil
                      (lambda (reply)
                        (when-let* ((threads (alist-get 'threads (alist-get 'result reply))))
                          (setq vellumory--worker-threads threads)
                          (dolist (buffer (buffer-list))
                            (with-current-buffer buffer
                              (when (eq vellumory--process vellumory--worker)
                                (setq vellumory--threads threads))))))))))

(defun vellumory--leave-worker ()
  "Close this buffer's document; the last one out stops the worker."
  (when (process-live-p vellumory--process)
    (let ((process vellumory--process) (buffer (current-buffer)))
      (when vellumory--inflight (vellumory--cancel (hash-table-keys vellumory--inflight)))
      ;; Replies for this buffer have nowhere to go.
      (maphash (lambda (id entry) (when (eq (car entry) buffer) (remhash id vellumory--worker-callbacks)))
               vellumory--worker-callbacks)
      (if (seq-some (lambda (other) (and (not (eq other buffer))
                                         (eq (buffer-local-value 'vellumory--process other) process)))
                    (buffer-list))
          (vellumory--rpc "close" nil nil)
        (vellumory--stop-worker process)))
    (setq vellumory--process nil)))

(defun vellumory--stop-worker (&optional process)
  "End PROCESS, by default the shared worker, by closing its input.
The worker then removes its rasters itself; kill it only if it is still
running two seconds later."
  (when-let* ((process (or process vellumory--worker)) ((process-live-p process)))
    ;; A worker on its way out takes no new documents: the next one starts afresh.
    (when (eq process vellumory--worker) (setq vellumory--worker nil))
    (process-send-eof process)
    (run-at-time 2 nil (lambda () (when (process-live-p process) (delete-process process))))))

;; Quitting Emacs: the worker still gets to clean up (or the next one sweeps).
(add-hook 'kill-emacs-hook #'vellumory--stop-worker)

(defun vellumory--cancel (ids)
  "Ask the worker to cancel the requests IDS, if there are any."
  (when (and ids (process-live-p vellumory--process))
    (vellumory--rpc "cancel" `((ids . ,(vconcat ids))) nil 0)))

;;;; Geometry

(defun vellumory--window ()
  "Return a window showing this buffer on any frame, or nil."
  (get-buffer-window (current-buffer) t))

(defun vellumory--device-scale ()
  "Return device pixels per logical pixel for this buffer's windows.
That is the largest scale factor of their frames, at least 1.0."
  (let ((scale 1.0))
    (dolist (window (get-buffer-window-list (current-buffer) nil t) scale)
      (when (fboundp 'frame-scale-factor)
        (setq scale (max scale (frame-scale-factor (window-frame window))))))))

(defun vellumory--body-width ()
  "Return the body width in pixels of this buffer's window, or 800 without one."
  (if-let* ((w (vellumory--window))) (window-body-width w t) 800))

(defun vellumory--columns ()
  "Return the number of pages per row: 2 with spreads, otherwise 1."
  (if vellumory--spread 2 1))

(defun vellumory--fit-width ()
  "Width of one page: at zoom 1 a spread's pages fill the window together."
  (let ((columns (vellumory--columns)))
    (max 64 (min 16384 (floor (* vellumory--zoom (/ (- (vellumory--body-width) 16 (* (1- columns) vellumory-spread-gap))
                                                  (float columns))))))))

(defun vellumory--row-width ()
  "Width of a laid-out row: the pages of a spread and the gutters between them."
  (+ (* (vellumory--columns) vellumory--width) (* (1- (vellumory--columns)) vellumory-spread-gap)))

(defun vellumory--spreads ()
  "Pages laid out side by side, as lists of LEFT and RIGHT (nil: an empty side).
One page per list without spreads; `book' opens like a printed book, the
first page alone on the right; `pairs' shows 1–2, 3–4 and so on."
  (let ((count (length vellumory--pages)) spreads)
    (pcase vellumory--spread
      ('nil (dotimes (page count) (push (list page) spreads)))
      (kind (let ((page 0))
              (when (eq kind 'book) (push (list nil 0) spreads) (setq page 1))
              (while (< page count)
                (push (list page (and (< (1+ page) count) (1+ page))) spreads)
                (cl-incf page 2)))))
    (nreverse spreads)))

(defun vellumory--page-height (page &optional width)
  "Return the height in pixels of PAGE laid out WIDTH pixels wide.
WIDTH defaults to the layout width."
  (let ((size (aref vellumory--pages page)))
    (ceiling (* (or width vellumory--width)
                (/ (float (alist-get 'height size)) (alist-get 'width size))))))

(defun vellumory--points-per-pixel (page)
  "Return the PDF points of PAGE per pixel of the layout width."
  (/ (float (alist-get 'width (aref vellumory--pages page))) vellumory--width))

(defun vellumory--row-height (row)
  "Return the height in pixels of ROW; a gap row is `vellumory-page-gap' high."
  (if (vellumory--row-gap row) vellumory-page-gap
    (- (vellumory--row-bottom row) (vellumory--row-top row))))

(defun vellumory--placeholder (width height)
  "Return a display spec for blank space WIDTH by HEIGHT pixels."
  `(space :width (,width) :height (,height) :ascent 100))

(defun vellumory--tile-width (tile)
  "Return the width of TILE in layout pixels."
  (- (vellumory--tile-right tile) (vellumory--tile-left tile)))
(defun vellumory--tile-height (tile)
  "Return the height of TILE in layout pixels, that of its row."
  (let ((row (vellumory--tile-row tile))) (- (vellumory--row-bottom row) (vellumory--row-top row))))

(defun vellumory--row-index-at (position)
  "Binary search the row containing buffer POSITION."
  (let ((lo 0) (hi (length vellumory--rows)))
    (while (< (1+ lo) hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (<= (vellumory--row-position (aref vellumory--rows mid)) position)
            (setq lo mid) (setq hi mid))))
    lo))

(defun vellumory--row-at (position)
  "Return the row containing buffer POSITION, or nil when there are no rows."
  (when (> (length vellumory--rows) 0)
    (aref vellumory--rows (vellumory--row-index-at position))))

(defun vellumory--total-height ()
  "Return the height in pixels of the whole layout."
  (let ((n (length vellumory--rows)))
    (if (zerop n) 0
      (let ((last (aref vellumory--rows (1- n))))
        (+ (vellumory--row-offset last) (vellumory--row-height last))))))

;;;; Layout

(defvar-local vellumory--paper-remap nil "Face remapping that colours unrendered pages, or nil.")

(defun vellumory--invert-luminance (colour)
  "COLOUR with its luminance inverted and its hue kept.
That is what the worker does to the pages it inverts."
  (let* ((rgb (color-name-to-rgb colour))
         (shift (- 1 (* 2 (+ (* 0.299 (nth 0 rgb)) (* 0.587 (nth 1 rgb)) (* 0.114 (nth 2 rgb)))))))
    (apply #'color-rgb-to-hex
           (append (mapcar (lambda (c) (min 1.0 (max 0.0 (+ c shift)))) rgb) '(2)))))

(defun vellumory--paper-colour ()
  "The colour of paper once drawn in the page colours in effect, or nil if as is.
Unrendered pages take it, so nothing flashes white before a dark page arrives:
with `tint' paper is the theme's background, inverted it is black."
  (pcase (vellumory--theme)
    ('tint (face-background 'default nil t))
    ('invert (vellumory--invert-luminance (face-background 'vellumory-paper nil t)))))

(defun vellumory--update-paper ()
  "Colour unrendered pages as `vellumory--paper-colour' says."
  (when vellumory--paper-remap (face-remap-remove-relative vellumory--paper-remap))
  (setq vellumory--paper-remap
        (when-let* ((colour (vellumory--paper-colour)))
          (face-remap-add-relative 'vellumory-paper :background colour))))

(defun vellumory--layout ()
  "Lay out every page as placeholder tiles.  No raster work is requested."
  (vellumory--update-paper)
  (vellumory--deferring-gc (vellumory--layout-1)))

(defun vellumory--layout-1 ()
  "Do the work of `vellumory--layout'.
Abandon in-flight renders, then fill the erased buffer with rows of
placeholder tiles, at most `vellumory-strip-height' high, spread by spread,
with a gap row between spreads."
  ;; The layout never touches the file. After it changed on disk, modifying
  ;; the buffer would otherwise ask "changed on disk; really edit?".
  (when buffer-file-name (set-visited-file-modtime))
  (vellumory--abandon-jobs)
  (cl-incf vellumory--generation)
  ;; The old tiles vanish with the erased text; never touch their positions again.
  (setq vellumory--visible nil vellumory--visible-and-near nil vellumory--live-images 0
        vellumory--geometry nil
        vellumory--scale (vellumory--device-scale)
        vellumory--width (vellumory--fit-width))
  (let* ((body (vellumory--body-width))
         (tile-width (if (<= (vellumory--row-width) body) vellumory--width
                       (max 128 (min vellumory-tile-width vellumory--width))))
         (paper (list 'vellumory-paper))
         (gutter (propertize " " 'face 'vellumory-desk
                             'display `(space :width (,vellumory-spread-gap) :height (1) :ascent 100)))
         (inhibit-read-only t) (offset 0) (pages (length vellumory--pages))
         (spreads (vellumory--spreads))
         (page-rows (make-vector pages 0)) (spread-of (make-vector pages nil))
         rows)
    (setq vellumory--margin (max 0 (/ (- body (vellumory--row-width)) 2)))
    ;; A bare stretch inherits the default font's descent and would add it to
    ;; every strip.  One pixel of pure ascent never exceeds an image line.
    ;; The face is explicit: a prefix otherwise takes the face of the text it
    ;; precedes on the window's first line (paper white) but not on the others.
    (setq-local line-prefix
                (and (> vellumory--margin 0)
                     (propertize " " 'face 'vellumory-desk
                                 'display `(space :width (,vellumory--margin) :height (1) :ascent 100))))
    (erase-buffer)
    (dolist (spread spreads)
      (let* ((present (delq nil (copy-sequence spread)))
             (heights (mapcar (lambda (page) (and page (vellumory--page-height page))) spread))
             (height (apply #'max (delq nil (copy-sequence heights))))
             (top 0))
        (dolist (page present)
          (aset page-rows page (length rows))
          (aset spread-of page present))
        (while (< top height)
          ;; A row also ends where a page does: a shorter image would sit on
          ;; the baseline, at the bottom of its line.
          (let* ((bottom (apply #'min height (+ top vellumory-strip-height)
                                (seq-filter (lambda (h) (> h top)) (delq nil (copy-sequence heights)))))
                 (row (vellumory--row-create :page (car present) :top top :bottom bottom
                                             :position (point) :offset offset))
                 (column 0)
                 tiles)
            (cl-loop for page in spread
                     for page-height in heights
                     do (when (> column 0) (insert gutter))
                     (if (and page (< top page-height))
                         (let ((left 0))
                           (while (< left vellumory--width)
                             (let* ((right (min vellumory--width (+ left tile-width)))
                                    (tile (vellumory--tile-create :row row :page page :left left :right right
                                                                  :position (point))))
                               (insert (propertize " " 'display (vellumory--placeholder (- right left)
                                                                                        (- bottom top))
                                                   'face paper 'vellumory-tile tile))
                               (push tile tiles)
                               (setq left right))))
                       ;; The empty side of a spread, or below its shorter page.
                       (insert (propertize " " 'display (vellumory--placeholder vellumory--width (- bottom top))
                                           'face 'vellumory-desk)))
                     (cl-incf column))
            (setf (vellumory--row-tiles row) (vconcat (nreverse tiles)))
            (insert (propertize "\n" 'line-height t))
            (push row rows)
            (setq offset (+ offset (- bottom top)) top bottom)))
        (unless (eq spread (car (last spreads)))
          (push (vellumory--row-create :page (car (last present)) :top height :bottom height
                                       :position (point) :offset offset :gap t :tiles [])
                rows)
          (insert (propertize " " 'display (vellumory--placeholder (vellumory--row-width) vellumory-page-gap))
                  (propertize "\n" 'line-height t))
          (cl-incf offset vellumory-page-gap))))
    (setq vellumory--rows (vconcat (nreverse rows))
          vellumory--page-rows page-rows
          vellumory--spread-of spread-of)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun vellumory--abandon-jobs ()
  "Cancel every in-flight render; their replies will be ignored."
  (when vellumory--inflight
    (let (ids)
      (maphash (lambda (id _) (push id ids)) vellumory--inflight)
      (clrhash vellumory--inflight)
      (vellumory--cancel ids))))

;;;; Images

(defvar vellumory--released-files nil "Files of strips released since the last flush.")
(defvar vellumory--flush-timer nil)

(defun vellumory--flush-released ()
  "Free the decoded pixels of released strips once reading rests.
Emacs would keep them for `image-cache-eviction-delay' (300 seconds): the
screen benchmark saw Emacs grow by 45-78 MB in one run.  Freeing an image
redraws its frame, so this waits until the view has rested half a second."
  (if (or (< (- (float-time) vellumory--gc-activity) 0.5) (vellumory--animating-p) (input-pending-p))
      (setq vellumory--flush-timer (run-with-timer 0.5 nil #'vellumory--flush-released))
    (setq vellumory--flush-timer nil)
    (vellumory--free-released)))

(defun vellumory--free-released ()
  "Free the decoded pixels of the strips released since the last time.
The C library keeps freed memory for reuse instead of giving it back: after
40 pages of a 756-page document Emacs held 528 MB, 158 MB once trimmed.  So
Emacs and the worker give it back here, as reading rests."
  (dolist (file (delete-dups vellumory--released-files)) (clear-image-cache file))
  (setq vellumory--released-files nil)
  (when (fboundp 'malloc-trim) (malloc-trim))
  (when (process-live-p vellumory--process) (vellumory--rpc "rest" nil nil)))

(defun vellumory--release-tile (tile)
  "Drop the image of TILE and show its blank placeholder instead."
  (when-let* ((image (vellumory--tile-image tile)))
    (cl-decf vellumory--live-images)
    (when-let* ((file (plist-get (cdr image) :file)))
      (push file vellumory--released-files)
      (unless (timerp vellumory--flush-timer)
        (setq vellumory--flush-timer (run-with-timer 1 nil #'vellumory--flush-released)))))
  (setf (vellumory--tile-image tile) nil (vellumory--tile-quality tile) 0
        (vellumory--tile-signature tile) nil)
  (vellumory--put-display tile (vellumory--placeholder (vellumory--tile-width tile)
                                                      (vellumory--tile-height tile))))

(defun vellumory--put-display (tile display)
  "Set the `display' property of the character of TILE to DISPLAY."
  (with-silent-modifications
    (let ((pos (vellumory--tile-position tile)))
      (put-text-property pos (1+ pos) 'display display))))

(defun vellumory--preview-image (page)
  "A preview image of PAGE scaled to the layout width, or nil."
  (when-let* ((preview (and vellumory--previews (gethash page vellumory--previews))))
    (let* ((key (cons vellumory--width vellumory--generation))
           (images (plist-get preview :images)))
      (or (cdr (assoc key images))
          (let ((image (create-image (plist-get preview :path) 'pbm nil :scale 1.0
                                     :width vellumory--width :height (vellumory--page-height page)
                                     :ascent 100 :transform-smoothing t)))
            (plist-put preview :images (list (cons key image)))
            image)))))

(defun vellumory--trim-previews (tiles)
  "Drop decoded preview images of pages without any TILES nearby."
  (when vellumory--previews
    (let ((pages (make-hash-table :test #'eql)))
      (dolist (tile tiles) (puthash (vellumory--tile-page tile) t pages))
      (maphash (lambda (page preview)
                 (unless (gethash page pages) (plist-put preview :images nil)))
               vellumory--previews))))

(defun vellumory--show-preview (tile)
  "Show TILE as a slice of its page's preview while it shows a placeholder.
Do nothing until the preview has arrived."
  (when (< (vellumory--tile-quality tile) 1)
    (when-let* ((row (vellumory--tile-row tile))
                (image (vellumory--preview-image (vellumory--tile-page tile))))
      (vellumory--put-display
       tile (list (list 'slice (vellumory--tile-left tile) (vellumory--row-top row)
                        (vellumory--tile-width tile) (vellumory--tile-height tile))
                  image))
      (setf (vellumory--tile-quality tile) 1))))

(defun vellumory--show-sharp (tile path signature)
  "Show the sharp raster at PATH, rendered for SIGNATURE, in TILE."
  (let ((image (create-image path 'pbm nil :scale 1.0
                             :width (vellumory--tile-width tile) :height (vellumory--tile-height tile)
                             :ascent 100 :transform-smoothing (/= vellumory--scale 1.0))))
    (unless (vellumory--tile-image tile) (cl-incf vellumory--live-images))
    (setf (vellumory--tile-image tile) image (vellumory--tile-quality tile) 2
          (vellumory--tile-signature tile) signature)
    (let ((shown (vellumory--decorate-tile tile image)))
      (vellumory--put-display tile shown)
      ;; Emacs decodes an image when it is first drawn: about 5 ms for a
      ;; strip.  A tile rendered ahead is decoded now, between input events,
      ;; not in the scroll step that brings it into view.
      (unless (memq tile vellumory--visible)
        (when-let* ((window (vellumory--window)))
          (ignore-errors (image-size shown t (window-frame window))))))))

(defun vellumory--decorate-tile (tile image)
  "Attach clickable link areas, and annotations' hover text, to IMAGE for TILE.
Links come first: where both overlap, the link is what a click follows."
  (if-let* ((map (append (vellumory--link-map tile) (vellumory--form-areas tile) (vellumory--annotation-areas tile))))
      (append image (list :map map))
    image))

;;;; Visibility and scheduling

(defun vellumory--window-signature ()
  "Per window: rows shown, horizontal scroll and size.
Scrolling within the same rows changes nothing about raster work."
  (mapcar (lambda (w)
            (let* ((first (vellumory--row-index-at (window-start w)))
                   (index first)
                   (remaining (+ (window-body-height w t) (window-vscroll w t))))
              (while (and (> remaining 0) (< index (length vellumory--rows)))
                (cl-decf remaining (vellumory--row-height (aref vellumory--rows index)))
                (cl-incf index))
              (list w first index (window-hscroll w) (window-body-width w t) (window-body-height w t))))
          (get-buffer-window-list (current-buffer) nil t)))

(defun vellumory--scan (window from to fn)
  "Call FN with each tile of WINDOW's rows between pixel FROM and TO.
Coordinates are relative to the window top.  FN receives TILE and the row's
vertical centre distance from the reading focus."
  (let* ((index (vellumory--row-index-at (window-start window)))
         (y (- (window-vscroll window t)))
         (height (window-body-height window t))
         (focus (* height 0.3))
         (char (frame-char-width (window-frame window)))
         (x0 (- (* (window-hscroll window) char) vellumory--margin))
         (x1 (+ x0 (window-body-width window t))))
    ;; Walk upward first when FROM is above the window top.
    (let ((up index) (uy y))
      (while (and (> up 0) (> uy from))
        (cl-decf up)
        (cl-decf uy (vellumory--row-height (aref vellumory--rows up))))
      (setq index up y uy))
    (while (and (< y to) (< index (length vellumory--rows)))
      (let* ((row (aref vellumory--rows index)) (h (vellumory--row-height row)))
        (when (and (> (+ y h) from) (not (vellumory--row-gap row)))
          (let ((distance (abs (- (+ y (/ h 2.0)) focus))))
            (cl-loop for tile across (vellumory--row-tiles row)
                     when (and (> (vellumory--tile-right tile) x0) (< (vellumory--tile-left tile) x1))
                     do (funcall fn tile distance))))
        (cl-incf y h) (cl-incf index)))))

(defun vellumory--collect (from-screens to-screens)
  "Return the tiles from FROM-SCREENS above to TO-SCREENS below each window.
A screen is the window's body height.  Tiles of all windows are ordered by
their distance from a reading focus, nearest first."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (let ((height (window-body-height window t)))
        (vellumory--scan window (- (* from-screens height)) (* (1+ to-screens) height)
                         (lambda (tile distance)
                           (let ((old (gethash tile table)))
                             (when (or (null old) (< distance old)) (puthash tile distance table)))))))
    (let (pairs)
      (maphash (lambda (tile distance) (push (cons distance tile) pairs)) table)
      (mapcar #'cdr (sort pairs (lambda (a b) (< (car a) (car b))))))))

(defun vellumory--moving-p ()
  "Return non-nil if the view moved less than `vellumory-refine-delay' ago."
  (< (- (float-time) vellumory--last-motion) vellumory-refine-delay))

(defun vellumory--note-motion (pixels)
  "Record a move of the view by PIXELS: its time and, unless 0, its direction."
  (unless (zerop pixels) (setq vellumory--direction (if (> pixels 0) 1 -1)))
  (setq vellumory--last-motion (float-time)))

(defvar-local vellumory--list-cost 20.0 "Estimated milliseconds to build a page's display list.")
(defvar-local vellumory--listed-pages nil "Pages whose display lists the worker has built.")

(defun vellumory--learn-cost (page result)
  "Update cost estimates from a render RESULT for PAGE.
Raster time per megapixel and list-building time are learned separately:
the first excludes waiting for the document lock, the second is paid once."
  (let ((pixels (alist-get 'pixels result)) (raster (alist-get 'raster_ms result)))
    (cl-incf vellumory--rendered-pixels (or pixels 0))
    (unless vellumory--listed-pages (setq vellumory--listed-pages (make-hash-table :test #'eql)))
    (puthash page t vellumory--listed-pages)
    (when (and pixels (> pixels 4096) raster)
      (setq vellumory--cost (+ (* 0.7 vellumory--cost) (* 0.3 (/ raster (/ pixels 1e6))))))
    (when (alist-get 'built result)
      (setq vellumory--list-cost (+ (* 0.7 vellumory--list-cost) (* 0.3 (alist-get 'list_ms result)))))))

(defun vellumory--expected-raster-ms (tile)
  "Return the estimated milliseconds to rasterize TILE."
  (* vellumory--cost 1e-6 (vellumory--tile-width tile) (vellumory--tile-height tile)
     vellumory--scale vellumory--scale))

(defun vellumory--expected-ms (tile)
  "Return the estimated milliseconds to render TILE.
That includes building its page's display list unless the worker has it."
  (+ (* vellumory--cost 1e-6 (vellumory--tile-width tile) (vellumory--tile-height tile)
        vellumory--scale vellumory--scale)
     (if (and vellumory--listed-pages
              (gethash (vellumory--tile-page tile) vellumory--listed-pages))
         0 vellumory--list-cost)))

(defun vellumory--theme ()
  "The page colours in effect: `vellumory-theme', with `auto' resolved.
That is inverted while the frame's background is dark, else the original."
  (if (eq vellumory-theme 'auto)
      (and (eq (frame-parameter nil 'background-mode) 'dark) 'invert)
    vellumory-theme))

(defun vellumory--theme-changed (&rest _)
  "Draw again the documents whose colours follow the Emacs theme.
For `enable-theme-functions': `tint' takes the theme's colours, `auto'
inverts with a dark theme."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'vellumory-mode) (memq vellumory-theme '(tint auto)))
        (vellumory--recolour)))))

(define-minor-mode vellumory-night-mode
  "Show this document's pages with inverted luminance.
Pages are dark and text light, the hues kept, for reading at night or
beside a dark theme.  Turned off, the pages take `vellumory-theme' again."
  :lighter " Night"
  (if (not (derived-mode-p 'vellumory-mode))
      ;; From a sidebar, for the document it belongs to.
      (let ((document (vellumory--this-document)))
        (setq vellumory-night-mode nil)
        (with-current-buffer document (vellumory-night-mode arg)))
    (if vellumory-night-mode (setq-local vellumory-theme 'invert) (kill-local-variable 'vellumory-theme))
    (vellumory--recolour)))

(defun vellumory--theme-params ()
  "Return the worker's recolouring parameter for `vellumory-theme'.
That is \"invert\", for `tint' an alist of the default face's foreground and
background as packed RGB integers, or nil."
  (pcase (vellumory--theme)
    ('invert "invert")
    ('tint (let ((fg (color-name-to-rgb (face-foreground 'default nil t)))
                 (bg (color-name-to-rgb (face-background 'default nil t))))
             (cl-flet ((pack (rgb) (apply #'logior (cl-mapcar (lambda (c s) (ash (round (* 255 c)) s))
                                                             rgb '(16 8 0)))))
               `((fg . ,(pack fg)) (bg . ,(pack bg))))))))

(defun vellumory--signature (page)
  "Everything besides geometry that a sharp raster of PAGE depends on."
  (list (vellumory--theme-params) (vellumory--page-overlay page)))

(defun vellumory--needs-sharp (tile)
  "Return non-nil if TILE lacks a sharp raster for its page's signature."
  (or (< (vellumory--tile-quality tile) 2)
      (not (equal (vellumory--tile-signature tile)
                  (vellumory--signature (vellumory--tile-page tile))))))

(defun vellumory--request-sharp (tile rank)
  "Ask the worker for a sharp raster of TILE with priority RANK."
  (let* ((row (vellumory--tile-row tile))
         (page (vellumory--tile-page tile))
         (scale vellumory--scale)
         (signature (vellumory--signature page))
         (overlay (cadr signature))
         (theme (car signature))
         (generation vellumory--generation)
         (params `((page . ,page) (width . ,(round (* scale vellumory--width)))
                   (left . ,(floor (* scale (vellumory--tile-left tile))))
                   (right . ,(ceiling (* scale (vellumory--tile-right tile))))
                   (top . ,(floor (* scale (vellumory--row-top row))))
                   (bottom . ,(ceiling (* scale (vellumory--row-bottom row))))
                   (priority . ,rank)
                   ,@(and theme `((theme . ,theme)))
                   ,@(and overlay `((overlay . ,overlay)))))
         id)
    (setq id (vellumory--rpc
              "render" params
              (lambda (reply) (vellumory--sharp-reply tile generation signature reply id))))
    (setf (vellumory--tile-job tile) id)
    (puthash id (list :tile tile :kind 'sharp) vellumory--inflight)))

(defun vellumory--sharp-reply (tile generation signature reply id)
  "Handle REPLY to the sharp render request ID for TILE, then pump.
Show the raster, rendered for SIGNATURE, if the layout is still GENERATION
and TILE is visible or near; record the costs it reports."
  (remhash id vellumory--inflight)
  (when (eql (vellumory--tile-job tile) id) (setf (vellumory--tile-job tile) nil))
  (let ((result (alist-get 'result reply)))
    (cond
     ((alist-get 'cancelled reply))
     ((alist-get 'error reply) (setq vellumory--status (alist-get 'error reply)))
     ((/= generation vellumory--generation))
     (t
      (vellumory--learn-cost (vellumory--tile-page tile) result)
      (when (memq tile vellumory--visible-and-near)
        (vellumory--show-sharp tile (alist-get 'path result) signature)))))
  (vellumory--pump))

(defun vellumory--request-preview (page rank)
  "Ask the worker for a whole-page preview of PAGE with priority RANK.
It is rendered at `vellumory-preview-scale' and kept across sessions; when
it arrives, the visible and near tiles of PAGE show it."
  (let* ((width (max 48 (round (* vellumory--scale vellumory--width vellumory-preview-scale))))
         (generation vellumory--generation)
         (theme (vellumory--theme-params))
         id)
    (setq id (vellumory--rpc
              ;; Small and wanted on every visit: kept across sessions.
              "render" `((page . ,page) (width . ,width) (top . 0) (persist . t)
                         (bottom . ,(vellumory--page-height page width)) (priority . ,rank)
                         ,@(and theme `((theme . ,theme))))
              (lambda (reply)
                (remhash id vellumory--inflight)
                (when (and (= generation vellumory--generation) (alist-get 'result reply))
                  (puthash page (list :path (alist-get 'path (alist-get 'result reply)) :width width)
                           vellumory--previews)
                  (dolist (tile vellumory--visible-and-near)
                    (when (= page (vellumory--tile-page tile))
                      (vellumory--show-preview tile))))
                (vellumory--pump))))
    (puthash id (list :page page :kind 'preview) vellumory--inflight)))

(defun vellumory--desired ()
  "Ordered work: (KIND . OBJECT) for sharp tiles and page previews."
  (let* ((moving (vellumory--moving-p))
         (work nil) (previews nil))
    (dolist (tile vellumory--visible)
      (when (vellumory--needs-sharp tile)
        (let ((page (vellumory--tile-page tile)))
          ;; A preview needs the same display list, so it only helps when
          ;; rasterizing, not interpreting, is the expensive part.
          (if (or (not moving) (<= (vellumory--expected-raster-ms tile) vellumory-immediate-budget))
              (push (cons 'sharp tile) work)
            (unless (or (gethash page vellumory--previews) (memq page previews))
              (push page previews))))))
    (setq work (nreverse work))
    ;; Previews come first: they are cheap and cover whole pages at once.
    (setq work (append (mapcar (lambda (page) (cons 'preview page)) (nreverse previews)) work))
    (if moving work
      ;; Idle: prefetch one viewport ahead and half behind (PDF.js, Okular).
      (let* ((ahead (car vellumory-prefetch-screens)) (behind (cdr vellumory-prefetch-screens))
             (from (if (> vellumory--direction 0) behind ahead))
             (to (if (> vellumory--direction 0) ahead behind))
             prefetch)
        (dolist (tile (vellumory--collect from to))
          (when (and (not (memq tile vellumory--visible)) (vellumory--needs-sharp tile))
            (push (cons 'sharp tile) prefetch)))
        (append work (nreverse prefetch))))))

(defun vellumory--pump ()
  "Cancel unwanted renders and keep the worker's threads busy with the best work."
  (vellumory--pump-1))

(defun vellumory--pump-1 ()
  "Do the work of `vellumory--pump'.
Cancel jobs no longer wanted, request the best work while fewer than one
more than the worker's threads are in flight, update the status, then let
search and thumbnails request theirs."
  (unless vellumory--inflight (setq vellumory--inflight (make-hash-table :test #'eql)))
  (unless vellumory--previews (setq vellumory--previews (make-hash-table :test #'eql)))
  (when (and (process-live-p vellumory--process) vellumory--pages)
    (let* ((work (vellumory--desired))
           (wanted-tiles (make-hash-table :test #'eq))
           (wanted-pages (make-hash-table :test #'eql))
           cancel)
      (dolist (item work)
        (if (eq (car item) 'sharp) (puthash (cdr item) t wanted-tiles)
          (puthash (cdr item) t wanted-pages)))
      (maphash (lambda (id job)
                 (unless (if (eq (plist-get job :kind) 'sharp)
                             (gethash (plist-get job :tile) wanted-tiles)
                           (gethash (plist-get job :page) wanted-pages))
                   (push id cancel)))
               vellumory--inflight)
      (dolist (id cancel)
        (let ((job (gethash id vellumory--inflight)))
          (when-let* ((tile (plist-get job :tile))) (setf (vellumory--tile-job tile) nil)))
        (remhash id vellumory--inflight))
      (vellumory--cancel cancel)
      (let ((slots (- (1+ vellumory--threads) (hash-table-count vellumory--inflight)))
            (rank 0))
        (dolist (item work)
          (when (> slots 0)
            (pcase-exhaustive item
              (`(sharp . ,tile)
               (unless (vellumory--tile-job tile)
                 (vellumory--request-sharp tile rank) (cl-decf slots)))
              (`(preview . ,page)
               (unless (cl-loop for job being the hash-values of vellumory--inflight
                                thereis (eql (plist-get job :page) page))
                 (vellumory--request-preview page rank) (cl-decf slots))))
            (cl-incf rank))))
      (let ((status (if (zerop (hash-table-count vellumory--inflight)) "Ready" "Rendering…")))
        ;; Shown in the header and mode line, which Emacs keeps as they are
        ;; while no text changes: ask for them anew when the status does.
        (unless (equal status vellumory--status)
          (setq vellumory--status status)
          (force-mode-line-update)))))
  (vellumory--search-pump)
  (vellumory--thumbnail-pump))

(defun vellumory--refresh (&optional force)
  "Recompute visible tiles after any change of window geometry.
With FORCE, recompute them even if the windows show the same rows."
  (vellumory--refresh-1 force))

(defun vellumory--refresh-1 (&optional force)
  "Do the work of `vellumory--refresh', FORCE as there.
Lay out first when there are no rows or the page width, device scale or
margin changed, keeping every window's place."
  (when (and vellumory--pages (vellumory--window))
    (vellumory--note-windows)
    (unless (and (> (length vellumory--rows) 0)
                 (= (vellumory--fit-width) vellumory--width)
                 (= (vellumory--device-scale) vellumory--scale)
                 (= (max 0 (/ (- (vellumory--body-width) (vellumory--row-width)) 2)) vellumory--margin))
      ;; Every window on the document keeps its place, not only the first.
      (let ((positions (mapcar (lambda (window) (cons window (vellumory--reading-position window)))
                               (get-buffer-window-list (current-buffer) nil t))))
        (vellumory--layout)
        (pcase-dolist (`(,window . ,position) positions)
          (vellumory--restore-position position window))
        (vellumory--content-hscroll)
        (setq force t)))
    ;; Followers first: their rows are part of what is visible.
    (when vellumory-follow-mode (vellumory-follow--align))
    (let ((signature (vellumory--window-signature))
          (position (vellumory--reading-position)))
      (unless (equal position vellumory--last-position)
        (setq vellumory--last-position position)
        (vellumory--update-page)
        (vellumory--arm-refine))
      (when (or force (not (equal signature vellumory--geometry)))
        (setq vellumory--geometry signature)
        (let* ((visible (vellumory--collect 0 0))
               (near (vellumory--collect vellumory-retain-screens vellumory-retain-screens))
               (keep (make-hash-table :test #'eq)))
          (dolist (tile near) (puthash tile t keep))
          (dolist (tile vellumory--visible-and-near)
            (unless (gethash tile keep) (vellumory--release-tile tile)))
          (setq vellumory--visible visible vellumory--visible-and-near near)
          (vellumory--trim-previews near)
          (dolist (tile visible) (vellumory--show-preview tile)))
        (vellumory--update-page)
        (vellumory--arm-refine)
        (vellumory--pump))))
  (vellumory--update-thumbnails))

(defun vellumory--window-changed (window)
  "Buffer-local `window-size-change-functions' entry for WINDOW.
The refresh places a resized window again (`vellumory--note-windows')."
  (when (buffer-live-p (window-buffer window))
    (with-current-buffer (window-buffer window) (vellumory--refresh))))

(defun vellumory--window-size (window)
  "WINDOW's body size in pixels, (WIDTH . HEIGHT)."
  (cons (window-body-width window t) (window-body-height window t)))

(defun vellumory--newly-shown-p (window)
  "Return non-nil if WINDOW newly shows the document and is not yet placed.
Newly means since the last redisplay: `window-old-buffer' is what it showed
then, nil if created since.  A window restored from a window configuration,
whose old buffer is t, brings its place and does not count."
  (let ((old (window-old-buffer window)))
    (and (not (eq old (current-buffer))) (not (eq old t)) (not (memq window vellumory--placed)))))

(defun vellumory--window-shown (window)
  "Show the document in WINDOW, newly showing it, where it was.
That is where WINDOW last showed it, as `window-prev-buffers' restores a
text buffer's start; for a new window, where the latest window on it is, as
`image-mode' gives a new window the latest one's properties (DocView,
pdf-tools) and a split text window keeps its start; otherwise, if shown
before, where reading last was.  Emacs restores or copies the start, not
the vscroll: up to a strip off."
  (if-let* ((own (car (alist-get window vellumory--window-positions))))
      (vellumory--restore-position own window)
    (if-let* ((latest (car (sort (seq-remove #'vellumory--newly-shown-p
                                             (delq window (get-buffer-window-list (current-buffer) nil t)))
                                 (lambda (a b) (> (window-use-time a) (window-use-time b)))))))
        (vellumory--seek (vellumory--absolute-position latest) window)
      (when vellumory--window-positions
        (vellumory--restore-position vellumory--last-position window)))))

(defun vellumory--recenter-advice (fn &rest args)
  "Call FN, `recenter', with ARGS, except in a window reading a document.
There point is an anchor on a visible strip, not a place in the text, so
centring it only scrolls the page: up to half a window after each of Doom's
window splits, which recenter (`+evil-window-split-a'), and on evil's zz."
  (unless (and (eq (window-buffer) (current-buffer)) (derived-mode-p 'vellumory-mode))
    (apply fn args)))

(defun vellumory--window-buffer-changed (window)
  "Buffer-local `window-buffer-change-functions' entry for WINDOW."
  (when (buffer-live-p (window-buffer window))
    (with-current-buffer (window-buffer window) (vellumory--note-windows))))

(defun vellumory--narrow-window-reflow (window)
  "Show the document reflowed in WINDOW if narrow.
That is narrower than `vellumory-reflow-narrow-width' columns."
  (when (and vellumory-reflow-narrow-width (window-live-p window)
             (< (window-body-width window) vellumory-reflow-narrow-width))
    (let ((buffer (current-buffer)))
      (run-at-time 0 nil (lambda ()
                           (when (and (window-live-p window) (eq (window-buffer window) buffer))
                             (with-current-buffer buffer (vellumory-reflow window t))))))))

(defun vellumory--note-windows ()
  "Place windows newly showing the document, and remember where all show it.
Run by the refresh, before anything reads where windows are (a refresh after
`switch-to-buffer' comes before the redisplay), and buffer-locally by
`window-buffer-change-functions' (a redisplay without a refresh)."
  (when (and vellumory--pages (> (length vellumory--rows) 0))
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      ;; Placed once: later moves before the next redisplay (a keyboard
      ;; macro's) stand.
      (setq vellumory--placed (seq-filter (lambda (window) (and (window-live-p window)
                                                                (not (eq (window-old-buffer window) (current-buffer)))))
                                          vellumory--placed))
      (dolist (window windows)
        (let ((record (alist-get window vellumory--window-positions)))
          (cond ((vellumory--newly-shown-p window)
                 (vellumory--window-shown window)
                 (push window vellumory--placed)
                 ;; Back in a frame: with the sidebar it left with.
                 (with-selected-window window (vellumory--sidebar-return)))
                ;; Resized since: a resize never moves the place, but Emacs may
                ;; have (`delete-other-windows' recomputes the start and resets
                ;; the vscroll, window.c); placing it again also keeps point on
                ;; a row a shorter window shows, or redisplay would scroll to it.
                ((and (car record) (not (equal (cdr record) (vellumory--window-size window))))
                 (vellumory--restore-position (car record) window)))))
      (setq vellumory--window-positions
            (append (mapcar (lambda (window) (cons window (cons (vellumory--reading-position window)
                                                                (vellumory--window-size window))))
                            windows)
                    (seq-filter (lambda (entry) (and (window-live-p (car entry)) (not (memq (car entry) windows))))
                                vellumory--window-positions))))))

(defun vellumory--arm-refine ()
  "Pump this buffer's work again once `vellumory-refine-delay' has passed.
Arming it again restarts the wait."
  (when (timerp vellumory--refine-timer) (cancel-timer vellumory--refine-timer))
  (let ((buffer (current-buffer)))
    (setq vellumory--refine-timer
          (run-with-timer (+ vellumory-refine-delay 0.01) nil
                          (lambda () (when (buffer-live-p buffer)
                                       (with-current-buffer buffer (vellumory--pump))))))))

(defun vellumory--update-page ()
  "Run `vellumory-page-change-functions' if the page in view changed."
  (let ((page (alist-get 'page vellumory--last-position)))
    (unless (equal page vellumory--current-page)
      (setq vellumory--current-page page)
      (run-hook-with-args 'vellumory-page-change-functions page))))

;;;; Scrolling

(defun vellumory--absolute-position (&optional window)
  "Return how far WINDOW is scrolled, in pixels from the top of the layout.
WINDOW defaults to this buffer's window; without one, return 0."
  (let* ((window (or window (vellumory--window)))
         (row (and window (vellumory--row-at (window-start window)))))
    (if row (+ (vellumory--row-offset row) (window-vscroll window t)) 0)))

(defun vellumory--seek (offset &optional window)
  "Scroll WINDOW to OFFSET pixels from the top of the layout.
OFFSET is kept within the layout, point goes to a fully visible row, and
the new place is recorded for `vellumory--note-windows'.  WINDOW defaults to
this buffer's window."
  ;; Every move of the view, by a command or not (animation frames, follow
  ;; mode, Lisp): keep collection out of the redisplay that shows it.
  (vellumory--defer-gc)
  (let* ((window (or window (vellumory--window)))
         (count (length vellumory--rows)))
    (when (and window (> count 0))
      (let* ((end (vellumory--total-height))
             (offset (round (max 0 (min offset (max 0 (- end (window-body-height window t)))))))
             (lo 0) (hi count))
        (while (< (1+ lo) hi)
          (let ((mid (/ (+ lo hi) 2)))
            (if (<= (vellumory--row-offset (aref vellumory--rows mid)) offset)
                (setq lo mid) (setq hi mid))))
        (let* ((row (aref vellumory--rows lo))
               (vscroll (- offset (vellumory--row-offset row)))
               (point-row (if (and (> vscroll 0) (< (1+ lo) count)
                                   (< (- (vellumory--row-offset (aref vellumory--rows (1+ lo))) offset)
                                      (window-body-height window t)))
                              (aref vellumory--rows (1+ lo)) row)))
          ;; Keep point on a fully visible row. Otherwise redisplay scrolls the
          ;; partially clipped row back into view, losing the pixel position.
          (set-window-point window (vellumory--row-position point-row))
          (set-window-start window (vellumory--row-position row) t)
          (set-window-vscroll window vscroll t)
          ;; The window's place now, at its size now: a later resize alone
          ;; puts it back here (`vellumory--note-windows').
          (setf (alist-get window vellumory--window-positions)
                (cons (vellumory--reading-position window) (vellumory--window-size window))))))))

(defun vellumory-scroll (pixels)
  "Scroll continuously by PIXELS."
  (interactive "nPixels: ")
  (vellumory--stop-animation)
  (vellumory--note-motion pixels)
  (vellumory--seek (+ (vellumory--absolute-position) pixels))
  (vellumory--refresh))

(defun vellumory--stop-animation ()
  "Stop this buffer's scroll animation, if any."
  (when (timerp vellumory--animation) (cancel-timer vellumory--animation))
  (setq vellumory--animation nil))

(defun vellumory--animate (pixels)
  "Scroll by PIXELS, animated when `vellumory-smooth-scroll' is non-nil."
  (if (or (not vellumory-smooth-scroll) (not (display-graphic-p)) executing-kbd-macro noninteractive)
      (vellumory-scroll pixels)
    (vellumory--stop-animation)
    (let* ((buffer (current-buffer)) (window (selected-window))
           (origin (vellumory--absolute-position window))
           (target (+ origin pixels)) (start (float-time)))
      (vellumory--note-motion pixels)
      (setq vellumory--animation
            (run-with-timer
             0 (/ 1.0 120)
             (lambda ()
               (if (not (and (buffer-live-p buffer) (window-live-p window)
                             (eq (window-buffer window) buffer)))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer (vellumory--stop-animation)))
                 (with-current-buffer buffer
                   (let* ((u (min 1.0 (/ (- (float-time) start) vellumory-smooth-scroll-duration)))
                          (eased (- 1 (expt (- 1 u) 3))))
                     (setq vellumory--last-motion (float-time))
                     (vellumory--seek (+ origin (* eased (- target origin))) window)
                     (when (>= u 1.0) (vellumory--stop-animation))
                     (vellumory--refresh))))))))))

(defun vellumory-scroll-down (&optional count)
  "Scroll forward by COUNT small steps."
  (interactive "p") (vellumory-scroll (* 64 (or count 1))))
(defun vellumory-scroll-up (&optional count)
  "Scroll backward by COUNT small steps."
  (interactive "p") (vellumory-scroll (* -64 (or count 1))))
(defun vellumory--screen-height ()
  "Pixels of a screen: the window, or all of them in `vellumory-follow-mode'."
  (if vellumory-follow-mode (vellumory-follow--screen-height) (window-body-height nil t)))
(defun vellumory-next-screen ()
  "Scroll forward by most of a screen." (interactive)
  (vellumory--animate (round (* .88 (vellumory--screen-height)))))
(defun vellumory-previous-screen ()
  "Scroll backward by most of a screen." (interactive)
  (vellumory--animate (round (* -.88 (vellumory--screen-height)))))
(defun vellumory-half-screen-down ()
  "Scroll forward by half a screen." (interactive)
  (vellumory--animate (round (* .5 (vellumory--screen-height)))))
(defun vellumory-half-screen-up ()
  "Scroll backward by half a screen." (interactive)
  (vellumory--animate (round (* -.5 (vellumory--screen-height)))))

(defun vellumory-scroll-left (&optional count)
  "Scroll the page view left by COUNT steps." (interactive "p")
  (set-window-hscroll nil (max 0 (- (window-hscroll) (* 4 (or count 1))))) (vellumory--refresh))
(defun vellumory-scroll-right (&optional count)
  "Scroll the page view right by COUNT steps." (interactive "p")
  (set-window-hscroll nil (+ (window-hscroll) (* 4 (or count 1)))) (vellumory--refresh))

(defun vellumory-wheel (event)
  "Scroll by pixel deltas from EVENT, with a fallback for discrete wheels."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window)
      (with-selected-window window
        (if (memq (event-basic-type event) '(wheel-left wheel-right))
            (if (eq (event-basic-type event) 'wheel-left) (vellumory-scroll-left) (vellumory-scroll-right))
          (vellumory-scroll
           (if (nth 4 event)
               ;; Emacs precision events use negative deltas for moving down
               ;; the document. Retain fractional pixels from smooth trackpads.
               (let* ((delta (- vellumory--wheel-remainder (cdr (nth 4 event))))
                      (pixels (truncate delta)))
                 (setq vellumory--wheel-remainder (- delta pixels))
                 pixels)
             ;; One distance per notch, however fast they come: quick notches
             ;; arrive as double and triple events, and multiplying by their
             ;; count overshot fivefold, onto pages not rendered yet.
             (* (if (memq (event-basic-type event) '(wheel-up mouse-4)) -1 1)
                (or vellumory-wheel-step
                    (round (expt (window-body-height nil t) (/ 2.0 3))))))))))))

(defun vellumory-wheel-zoom (event)
  "Zoom with the wheel EVENT, keeping the document point under the pointer fixed."
  (interactive "e")
  (let* ((posn (event-start event)) (window (posn-window posn)))
    (when (windowp window)
      (with-selected-window window
        (let ((factor (if (memq (event-basic-type event) '(wheel-up mouse-4)) 1.1 (/ 1.1))))
          (vellumory--set-zoom (* vellumory--zoom factor) (cdr (posn-x-y posn))))))))

;;;; Positions

(defun vellumory--reading-position (&optional window)
  "Return the reading position of WINDOW, or nil without a window or rows.
That is an alist of the zero-based page of the row at the window start,
the y offset into it in PDF points, and the zoom.  WINDOW defaults to this
buffer's window."
  (when-let* ((window (or window (vellumory--window)))
              (row (vellumory--row-at (window-start window))))
    (let ((page (vellumory--row-page row)))
      `((page . ,page)
        (y . ,(* (vellumory--points-per-pixel page)
                 (+ (vellumory--row-top row) (window-vscroll window t))))
        (zoom . ,vellumory--zoom)))))

(defun vellumory--page-offset (page)
  "Return the layout offset in pixels of the first row of PAGE."
  (vellumory--row-offset (aref vellumory--rows (aref vellumory--page-rows page))))

(defun vellumory--offset-of (page y)
  "Absolute layout offset of point Y (PDF points) on PAGE."
  (+ (vellumory--page-offset page) (round (/ y (vellumory--points-per-pixel page)))))

(defun vellumory--restore-position (position &optional window)
  "Scroll WINDOW to POSITION, as `vellumory--reading-position' returns it.
A page outside the document is clamped to it.  WINDOW defaults to this
buffer's window."
  (when (and position vellumory--pages (> (length vellumory--page-rows) 0))
    (let ((page (max 0 (min (1- (length vellumory--pages)) (or (alist-get 'page position) 0)))))
      (vellumory--seek (vellumory--offset-of page (or (alist-get 'y position) 0)) window))))

(defun vellumory--state-file (&optional key)
  "Return the file in `vellumory-state-directory' holding a position for KEY.
It is named by the SHA-256 hash of KEY, by default the visited file or
else the file the worker reads."
  (expand-file-name (concat (secure-hash 'sha256 (or key vellumory--source-file vellumory--file)) ".json")
                    vellumory-state-directory))

(defun vellumory--save-position ()
  "Save the reading position for this file and for its content hash.
Each state file is written whole and renamed into place."
  (when (and vellumory--file vellumory--pages)
    (when-let* ((position (or (vellumory--reading-position) vellumory--last-position)))
      (make-directory vellumory-state-directory t)
      (set-file-modes vellumory-state-directory #o700)
      (let ((json (json-serialize (append position `((hash . ,(or vellumory--hash "")))))))
        (dolist (key (delq nil (list nil vellumory--hash)))
          (let ((temporary (make-temp-file (expand-file-name ".position-" vellumory-state-directory))))
            (unwind-protect
                (progn (with-temp-file temporary (insert json))
                       (rename-file temporary (vellumory--state-file key) t))
              (when (file-exists-p temporary) (delete-file temporary)))))))))

(defun vellumory--load-position ()
  "Position saved for this file, or for identical content saved elsewhere."
  (cl-loop for key in (delq nil (list nil vellumory--hash))
           thereis
           (condition-case nil
               (let* ((position (with-temp-buffer
                                  (insert-file-contents (vellumory--state-file key))
                                  (json-parse-buffer :object-type 'alist)))
                      (page (alist-get 'page position))
                      (y (alist-get 'y position))
                      (zoom (alist-get 'zoom position)))
                 (when (and (natnump page) (numberp y) (<= 0 y 1e9)
                            (numberp zoom) (<= .1 zoom 8.0))
                   (assq-delete-all 'hash position)))
             (error nil))))

;;;; Navigation commands

(defun vellumory-goto-page (page)
  "Jump to one-based PAGE."
  (interactive (list (if (and current-prefix-arg (not (consp current-prefix-arg)))
                         (prefix-numeric-value current-prefix-arg)
                       (read-number "Page: " (1+ (or vellumory--current-page 0))))))
  (unless (and vellumory--pages (<= 1 page (length vellumory--pages))) (user-error "Page outside document"))
  (vellumory--stop-animation)
  ;; A jump is not continuous motion: render the destination sharp at once.
  (setq vellumory--last-motion 0.0)
  (vellumory--restore-position `((page . ,(1- page)) (y . 0)))
  (vellumory--refresh t))
(defun vellumory-first-page () "Go to the first page." (interactive) (vellumory-goto-page 1))
(defun vellumory-last-page () "Go to the last page." (interactive)
  (vellumory-goto-page (length vellumory--pages)))
(defun vellumory--spread-pages (page)
  "The pages laid out beside PAGE, PAGE included."
  (or (and (< page (length vellumory--spread-of)) (aref vellumory--spread-of page)) (list page)))

(defun vellumory--step-page (page steps)
  "First page of the spread STEPS spreads after (or, negative, before) PAGE's."
  (let ((last (1- (length vellumory--pages))))
    (dotimes (_ (abs steps) page)
      (let ((spread (vellumory--spread-pages page)))
        (setq page (if (> steps 0) (min last (1+ (car (last spread))))
                     (car (vellumory--spread-pages (max 0 (1- (car spread)))))))))))

(defun vellumory-next-page (&optional count)
  "Go forward COUNT pages, or COUNT spreads when pages are side by side." (interactive "p")
  (vellumory-goto-page (1+ (vellumory--step-page (or vellumory--current-page 0) (or count 1)))))
(defun vellumory-previous-page (&optional count)
  "Go back COUNT pages (or spreads), or to the top of the current one.
The top, when the window is below it and COUNT is 1."
  (interactive "p")
  (let* ((position (vellumory--reading-position))
         (page (car (vellumory--spread-pages (alist-get 'page position)))))
    (vellumory-goto-page (1+ (if (and (= (or count 1) 1) (> (alist-get 'y position) 1)) page
                               (vellumory--step-page page (- (or count 1))))))))

(defun vellumory-cycle-spread ()
  "Lay out single pages, book spreads (first page alone) or pairs, in turn."
  (interactive)
  (setq vellumory--spread (pcase vellumory--spread ('nil 'book) ('book 'pairs) (_ nil)))
  (vellumory--layout-preserving)
  (message "%s" (pcase vellumory--spread ('nil "Single pages") ('book "Two pages, as a book opens")
                  (_ "Two pages, in pairs"))))

;;;; Zoom and theme

(defun vellumory-zoom-in () "Zoom in." (interactive) (vellumory--set-zoom (* vellumory--zoom 1.15)))
(defun vellumory-zoom-out () "Zoom out." (interactive) (vellumory--set-zoom (/ vellumory--zoom 1.15)))
(defun vellumory-fit-width () "Fit pages to the window width." (interactive) (vellumory--set-zoom 1.0))

(defvar-local vellumory--recenter-saved nil
  "(ZOOM CONTENT-FIT HSCROLL) that `vellumory-recenter' left, to return to.")

(defun vellumory-recenter ()
  "Alternate between the fitted, centred page and the zoom you were reading at.
From a zoomed or scrolled view, fit the pages to the window width and
remember the zoom and horizontal scroll; pressed again, return to them.
Either way the point at the middle of the window stays in place."
  (interactive)
  (let* ((window (selected-window))
         (middle (/ (window-body-height window t) 2)))
    (if (and (= vellumory--zoom 1.0) (not vellumory--content-fit) (= (window-hscroll window) 0))
        (if-let* ((saved vellumory--recenter-saved))
            (pcase-let ((`(,zoom ,content-fit ,hscroll) saved))
              (setq vellumory--recenter-saved nil)
              (if content-fit
                  (vellumory-fit-content-width)
                (vellumory--set-zoom zoom middle)
                (set-window-hscroll window hscroll)))
          (message "Already fitted to the window"))
      (setq vellumory--recenter-saved (list vellumory--zoom (and vellumory--content-fit t) (window-hscroll window)))
      (vellumory--set-zoom 1.0 middle)
      (set-window-hscroll window 0)
      (message "Fitted to the window; %s returns to %d%%"
               (substitute-command-keys "\\[vellumory-recenter]")
               (round (* 100 (car vellumory--recenter-saved)))))))
(defun vellumory-fit-page ()
  "Fit the current page entirely in the window." (interactive)
  (let* ((page (or vellumory--current-page 0))
         (size (aref vellumory--pages page))
         (ratio (/ (float (alist-get 'height size)) (alist-get 'width size)))
         (height (- (window-body-height nil t) 4))
         ;; One column's width, as `vellumory--fit-width' divides it.
         (width (/ (- (vellumory--body-width) 16 (* (1- (vellumory--columns)) vellumory-spread-gap))
                   (float (vellumory--columns)))))
    (vellumory--set-zoom (min 1.0 (/ (/ height ratio) width)))
    (vellumory-goto-page (1+ page))))

(defcustom vellumory-content-padding 12
  "Pixels of paper kept on each side when fitting the content to the window."
  :type 'natnum :group 'vellumory)

(defvar-local vellumory--content-fit nil
  "The content's horizontal extent (X0 . X1) in points, once fitted.
`vellumory-fit-content-width' sets it; the content is kept at the window's
left edge as the layout changes.")

(defun vellumory--content-hscroll ()
  "Scroll each window so the fitted content starts near its left edge.
It starts `vellumory-content-padding' pixels from the edge, to the nearest
column: horizontal scrolling is by columns."
  (when vellumory--content-fit
    (let* ((page (or vellumory--current-page 0))
           (pixels (- (/ (car vellumory--content-fit) (vellumory--points-per-pixel page))
                      vellumory-content-padding)))
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (set-window-hscroll window (max 0 (round pixels (frame-char-width (window-frame window)))))))))

(defun vellumory-fit-content-width ()
  "Zoom so the content of the page in view fills the window width.
The content is its ink, without the white margins around it, as pdf-tools'
slicing to the bounding box and Sioyek's smart width fit do.  It stays
fitted as the window changes size, until another zoom.  Single pages only:
a spread's two pages cannot both lose their margins to one horizontal
scroll."
  (interactive)
  (when vellumory--spread (user-error "Fitting to the content needs single pages (2 cycles the spread)"))
  (let ((page (or vellumory--current-page 0)) (buffer (current-buffer)))
    (vellumory--rpc
     "content-box" `((page . ,page))
     (lambda (reply)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((box (alist-get 'result reply)))
             (if (not (vectorp box)) (message "Page %d has no content to fit" (1+ page))
               (let* ((points (alist-get 'width (aref vellumory--pages page)))
                      (column (- (vellumory--body-width) 16))
                      ;; At zoom 1 the page is COLUMN pixels wide and its content the
                      ;; fraction (x1 − x0) / points of that; it should fill the window
                      ;; less the padding on both sides.
                      (zoom (/ (* (- (vellumory--body-width) (* 2 vellumory-content-padding)) points)
                               (* (- (aref box 2) (aref box 0)) column))))
                 (vellumory--set-zoom zoom)
                 (setq vellumory--content-fit (cons (aref box 0) (aref box 2)))
                 (vellumory--content-hscroll)
                 (vellumory--refresh t)
                 (message "Fitted to the content of page %d (%d%%)" (1+ page) (round (* 100 zoom))))))))))))

(defun vellumory--set-zoom (zoom &optional anchor-y)
  "Set ZOOM, keeping the document point at window pixel ANCHOR-Y fixed."
  (let* ((window (or (vellumory--window) (selected-window)))
         (anchor (or anchor-y 0))
         (target (vellumory--point-at-window-y window anchor)))
    ;; Leaving a fit to the content undoes its horizontal scroll too.
    (when vellumory--content-fit
      (dolist (w (get-buffer-window-list (current-buffer) nil t)) (set-window-hscroll w 0))
      (setq vellumory--content-fit nil))
    (setq vellumory--zoom (max .1 (min 8.0 zoom)))
    (vellumory--layout)
    (when target
      (vellumory--seek (- (vellumory--offset-of (car target) (cdr target)) anchor) window))
    (vellumory--refresh t)))

(defun vellumory--point-at-window-y (window y)
  "(PAGE . Y-POINTS) displayed at pixel Y of WINDOW."
  (when (> (length vellumory--rows) 0)
    (let* ((offset (+ (vellumory--absolute-position window) y))
           (lo 0) (hi (length vellumory--rows)))
      (while (< (1+ lo) hi)
        (let ((mid (/ (+ lo hi) 2)))
          (if (<= (vellumory--row-offset (aref vellumory--rows mid)) offset) (setq lo mid) (setq hi mid))))
      (let* ((row (aref vellumory--rows lo)) (page (vellumory--row-page row)))
        (cons page (* (vellumory--points-per-pixel page)
                      (min (vellumory--page-height page)
                           (+ (vellumory--row-top row) (- offset (vellumory--row-offset row))))))))))

(defconst vellumory--themes
  '((nil . "original") (tint . "theme colours") (invert . "inverted")
    (auto . "inverted with a dark theme"))
  "Page colours, as values of `vellumory-theme' and their names.")

(defun vellumory--this-document ()
  "The document this buffer shows, or that the sidebar it is belongs to."
  (cond ((derived-mode-p 'vellumory-mode) (current-buffer))
        ((buffer-live-p (bound-and-true-p vellumory--thumbnail-source)) vellumory--thumbnail-source)
        ((buffer-live-p (bound-and-true-p vellumory--outline-source)) vellumory--outline-source)
        (t (user-error "Not in a document or its sidebar"))))

(defun vellumory--recolour ()
  "Draw this document's pages and its previews again in the colours in effect."
  (when vellumory--pages
    (clrhash vellumory--previews)
    (vellumory--layout-preserving)
    (when (buffer-live-p vellumory--thumbnail-buffer) (vellumory--thumbnail-layout))))

(defun vellumory-set-theme (theme)
  "Show the pages and page previews of this document in THEME.
THEME is nil (the original colours), `tint' (the Emacs theme's foreground
and background), `invert' (inverted luminance) or `auto' (inverted while
the Emacs theme is dark).  Interactively, choose one."
  (interactive
   (list (car (rassoc (completing-read "Page colours: " (mapcar #'cdr vellumory--themes) nil t)
                      vellumory--themes))))
  (with-current-buffer (vellumory--this-document)
    (when vellumory-night-mode (setq vellumory-night-mode nil))
    (setq-local vellumory-theme theme)
    (vellumory--recolour)
    (message "Page colours: %s" (alist-get theme vellumory--themes))))

(defun vellumory-theme-original () "Show the pages in their own colours." (interactive) (vellumory-set-theme nil))
(defun vellumory-theme-tint () "Show the pages in the Emacs theme's colours." (interactive) (vellumory-set-theme 'tint))
(defun vellumory-theme-invert () "Show the pages inverted: dark, the hues kept." (interactive) (vellumory-set-theme 'invert))
(defun vellumory-theme-auto () "Show the pages inverted while the Emacs theme is dark." (interactive)
  (vellumory-set-theme 'auto))

(defun vellumory-cycle-theme (&optional choose)
  "Cycle the page colours: original, theme colours, inverted.
With \\[universal-argument] as CHOOSE, choose any of them by name,
`auto' too.  With a number N, take the Nth: 0 original, 1 theme colours,
2 inverted, 3 inverted with a dark theme.  The page previews change
with the pages."
  (interactive "P")
  (if (consp choose) (call-interactively #'vellumory-set-theme)
    (vellumory-set-theme
     (if (integerp choose) (car (nth (max 0 (min 3 choose)) vellumory--themes))
       (pcase (buffer-local-value 'vellumory-theme (vellumory--this-document))
         ('nil 'tint) ('tint 'invert) (_ nil))))))

(defun vellumory--layout-preserving ()
  "Lay out the pages again at the same reading position, and refresh."
  (let ((position (vellumory--reading-position)))
    (vellumory--layout)
    (vellumory--restore-position position)
    (vellumory--refresh t)))

;;;; Text

(defun vellumory-page-text ()
  "Show selectable, searchable text for the page at the viewport top."
  (interactive)
  (let* ((page (or vellumory--current-page 0))
         (name (format "*Text: %s · %d*" (buffer-name) (1+ page))))
    (vellumory--rpc "text" `((page . ,page))
                    (lambda (reply)
                      (if-let* ((error (alist-get 'error reply))) (message "%s" error)
                        (with-current-buffer (get-buffer-create name)
                          (let ((inhibit-read-only t))
                            (erase-buffer)
                            (insert (alist-get 'text (alist-get 'result reply)))
                            (goto-char (point-min)) (special-mode))
                          (pop-to-buffer (current-buffer))))))))

;;;; Opening, reloading and watching

(defun vellumory--no-line-highlight ()
  "An empty line-highlight range: readers show no point."
  (cons (point-min) (point-min)))

(defvar-local vellumory--signed 0 "Signed signature fields of the open document.")
(defvar-local vellumory--page-mode nil "The document's /PageMode, as it asks to be shown.")

(defvar-local vellumory--password nil
  "Password that opened this document, kept in memory only, for reloads.")

(defun vellumory--ask-password ()
  "Ask for the document's password and reopen it.
The prompt runs outside the worker's process filter."
  (let ((buffer (current-buffer)) (wrong vellumory--password))
    (setq vellumory--status "Password required" vellumory--password nil)
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((password (condition-case nil
                               (read-passwd (format "%sPassword for %s: " (if wrong "Wrong password.  " "")
                                                    (buffer-name)))
                             (quit nil))))
             (when (and password (not (string-empty-p password)))
               (setq vellumory--password password)
               (vellumory-reload)))))))))

(defun vellumory--follow-document-display (result)
  "Show the document as its catalog asks (RESULT of `open'), unless the user chose:
odd pages on the left of two columns are pairs, on the right a book's spreads."
  (when vellumory-follow-document-display
    (unless vellumory-spread
      (setq vellumory--spread (pcase (alist-get 'page_layout result)
                                ((or "TwoColumnLeft" "TwoPageLeft") 'pairs)
                                ((or "TwoColumnRight" "TwoPageRight") 'book))))
    (setq vellumory--page-mode (alist-get 'page_mode result))))

(defun vellumory--open-error (error)
  "ERROR from opening the document, in plain words when the cause is known."
  (cond ((and buffer-file-name (not (file-remote-p buffer-file-name)) (not (file-exists-p buffer-file-name)))
         (format "No such file: %s" (abbreviate-file-name buffer-file-name)))
        ((and vellumory--file (eql 0 (file-attribute-size (file-attributes vellumory--file))))
         "The document is empty")
        (t error)))

(defcustom vellumory-reflow-text-size 11
  "Points of text in reflowable documents (EPUB, FB2), as laid out on pages.
\\[text-scale-adjust] changes it for the document in view."
  :type 'number :group 'vellumory)

(defcustom vellumory-reflow-page-size '(450 . 600)
  "Width and height, in points, of the pages reflowable documents are laid out on."
  :type '(cons number number) :group 'vellumory)

(defcustom vellumory-reflow-stylesheet nil
  "A CSS file applied to reflowable documents (DocView's EPUB user stylesheet)."
  :type '(choice (const nil) file) :group 'vellumory)

(defvar-local vellumory--reflowable nil "The document's pages are a layout (EPUB, FB2).")
(defvar-local vellumory--reflow-size nil "This document's text size, once changed.")
(defvar-local vellumory--reflow-fraction nil
  "After a relayout, how far through the document to go (page numbers changed).")

(defun vellumory--reflow-settings ()
  "How the worker lays out a reflowable document."
  `((width . ,(float (car vellumory-reflow-page-size))) (height . ,(float (cdr vellumory-reflow-page-size)))
    (em . ,(float (or vellumory--reflow-size vellumory-reflow-text-size)))
    (css . ,(if (and vellumory-reflow-stylesheet (file-readable-p vellumory-reflow-stylesheet))
                (with-temp-buffer (insert-file-contents vellumory-reflow-stylesheet) (buffer-string))
              ""))))

(defun vellumory-text-scale-adjust (&optional increment)
  "Change a reflowable document's text size by INCREMENT points.
An INCREMENT of 0 returns to `vellumory-reflow-text-size'.  Other documents
zoom in, zoom out or fit the width instead.  This command remaps
`text-scale-adjust' (\\[text-scale-adjust] and the like); interactively,
the last key chooses INCREMENT: + or = is 1, - is -1, anything else 0."
  (interactive (list (pcase (event-basic-type last-command-event) ((or ?+ ?=) 1) (?- -1) (_ 0))))
  (if (not vellumory--reflowable)
      (pcase increment (1 (vellumory-zoom-in)) (-1 (vellumory-zoom-out)) (_ (vellumory-fit-width)))
    (let ((size (if (eq increment 0) vellumory-reflow-text-size
                  (max 6 (min 48 (+ (or vellumory--reflow-size vellumory-reflow-text-size) increment))))))
      (setq vellumory--reflow-fraction (/ (float (or vellumory--current-page 0)) (max 1 (length vellumory--pages)))
            vellumory--reflow-size size)
      (vellumory-reload)
      (message "Text %g points" size))))

(defun vellumory-reload ()
  "Reopen a fresh immutable snapshot while preserving the reading position."
  (interactive)
  (setq vellumory--pending-position (or (vellumory--reading-position) vellumory--pending-position))
  (unless (process-live-p vellumory--process) (vellumory--start))
  (setq vellumory--status "Opening…")
  (vellumory--rpc "open" `((path . ,vellumory--file) (password . ,(or vellumory--password ""))
                           (reflow . ,(vellumory--reflow-settings)))
                  (lambda (reply)
                    (if-let* ((error (alist-get 'error reply)))
                        (if (string-match-p "requires a valid password" error)
                            (vellumory--ask-password)
                          (setq vellumory--status (vellumory--open-error error))
                          (message "Vellumory: %s" vellumory--status))
                      (let ((result (alist-get 'result reply)) (first (null vellumory--hash)))
                        (vellumory-search-clear)
                        (when first (vellumory--follow-document-display result))
                        (setq vellumory--hash (alist-get 'hash result)
                              vellumory--signed (or (alist-get 'signed result) 0)
                              vellumory--pages (alist-get 'pages result)
                              vellumory--reflowable (eq (alist-get 'reflowable result) t)
                              vellumory--previews (make-hash-table :test #'eql)
                              vellumory--links (make-hash-table :test #'eql)
                              vellumory--listed-pages (make-hash-table :test #'eql))
                        (when (and first (null vellumory--pending-position))
                          (setq vellumory--pending-position (vellumory--load-position)))
                        ;; Laid out anew: the same share of the document, as page numbers changed.
                        (when vellumory--reflow-fraction
                          (setq vellumory--pending-position
                                `((page . ,(min (1- (length vellumory--pages))
                                                (floor (* vellumory--reflow-fraction (length vellumory--pages)))))
                                  (y . 0))
                                vellumory--reflow-fraction nil))
                        (setq vellumory--zoom (or (alist-get 'zoom vellumory--pending-position) vellumory--zoom)
                              vellumory--status "Rendering…"
                              vellumory--labels [] vellumory--rows [])
                        ;; Show the sidebar first so the page layout happens
                        ;; once, at its final width.
                        (vellumory--reset-thumbnails)
                        (when (zerop (length vellumory--rows)) (vellumory--layout))
                        (vellumory--restore-position vellumory--pending-position)
                        (when vellumory--source-file
                          (set-visited-file-modtime))
                        (vellumory--refresh t)
                        (vellumory--load-navigation))))))

(defun vellumory--watch-file ()
  "Reload the document when its file changes, if `vellumory-watch-file'.
A change reloads after 0.3 seconds without another one."
  ;; Watch the directory so an atomic rename does not detach the watch.
  (when (and vellumory-watch-file vellumory--source-file)
    (let ((buffer (current-buffer)) (file vellumory--source-file))
      (condition-case err
          (setq vellumory--watch
                (file-notify-add-watch
                 (file-name-directory file) '(change)
                 (lambda (event)
                   (when (and (buffer-live-p buffer)
                              (memq (cadr event) '(changed attribute-changed created renamed))
                              (member file (cddr event)))
                     (with-current-buffer buffer
                       (when (timerp vellumory--reload-timer) (cancel-timer vellumory--reload-timer))
                       (setq vellumory--reload-timer
                             (run-with-timer
                              .3 nil
                              (lambda ()
                                (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (when (file-readable-p file) (vellumory-reload))))))))))))
        (file-notify-error (message "Vellumory file watch: %s" (error-message-string err)))))))

(defun vellumory--bookmark-record ()
  "Return a bookmark record for the reading position in this document."
  `(,(format "%s, page %s" (buffer-name) (1+ (or vellumory--current-page 0)))
    (filename . ,(or vellumory--source-file vellumory--file)) (handler . vellumory-bookmark-jump)
    (position . ,(vellumory--reading-position))))

;;;###autoload
(defun vellumory-bookmark-jump (record)
  "Jump to the PDF position in bookmark RECORD."
  (let ((buffer (vellumory-open (bookmark-get-filename record) t)))
    (with-current-buffer buffer
      (setq vellumory--pending-position (alist-get 'position (bookmark-get-bookmark-record record)))
      (when vellumory--pages
        (vellumory--restore-position vellumory--pending-position) (vellumory--refresh t)))
    (bookmark-default-handler `("" (buffer . ,buffer)))))

(defun vellumory--cleanup ()
  "Save the reading position and release what this document holds.
That is its timers, file watch, sidebar buffers and place in the worker."
  (condition-case err (vellumory--save-position)
    (error (message "Vellumory position: %s" (error-message-string err))))
  (vellumory--stop-animation)
  (dolist (timer (list vellumory--timer vellumory--refine-timer vellumory--state-timer vellumory--reload-timer
                      vellumory--search-input-timer))
    (when (timerp timer) (cancel-timer timer)))
  (when vellumory--watch (file-notify-rm-watch vellumory--watch) (setq vellumory--watch nil))
  (when (buffer-live-p vellumory--outline-buffer) (kill-buffer vellumory--outline-buffer))
  (when (buffer-live-p vellumory--thumbnail-buffer) (kill-buffer vellumory--thumbnail-buffer))
  (vellumory--leave-worker))

(defvar-local vellumory--original-file nil
  "The file this buffer visits when the document read is a conversion of it.
Such are PostScript, DVI, DjVu and office documents: see
`vellumory-converted-mode'.")

(defun vellumory--restore-source ()
  "Leaving `vellumory-mode': put the document's bytes back into the buffer.
For a conversion, the original's."
  (vellumory--cleanup)
  (when-let* ((file (or vellumory--original-file vellumory--source-file))
              ((file-readable-p file)))
    (let ((inhibit-read-only t))
      (setq-local line-prefix nil)
      (insert-file-contents-literally file nil nil nil t)
      (set-buffer-modified-p nil))))

(defun vellumory--write-contents ()
  "The buffer shows a layout, not the document: never write it to the file."
  (set-buffer-modified-p nil)
  (message "Document unchanged; Vellumory never writes its page layout to %s"
           (file-name-nondirectory (or vellumory--source-file vellumory--file)))
  t)

(defvar vellumory--materialized nil
  "When bound, the document a buffer shows instead of its own bytes.
That is a virtual document's composition (see `vellumory-virtual-mode').")

(defun vellumory--materialize ()
  "Return a local file with this buffer's document bytes."
  (if (and buffer-file-name (not (file-remote-p buffer-file-name)) (file-readable-p buffer-file-name))
      (file-name-unquote (file-truename buffer-file-name))
    (let* ((hash (secure-hash 'sha256 (current-buffer)))
           (extension (or (and buffer-file-name (file-name-extension buffer-file-name)) "pdf"))
           (file (expand-file-name (format "sources/%s/%s.%s" (substring hash 0 2) hash extension)
                                   vellumory-cache-directory)))
      (unless (file-exists-p file)
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'no-conversion))
          (write-region nil nil file nil 'silent)))
      file)))

;;;; Mode

(defvar vellumory-edit-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding '(("r" . vellumory-rotate-pages) ("R" . vellumory-rotate-pages-counterclockwise)
                       ("d" . vellumory-delete-pages) ("m" . vellumory-move-page)
                       ("k" . vellumory-keep-pages) ("a" . vellumory-append-documents)
                       ("c" . vellumory-compress) ("L" . vellumory-linearize)
                       ("l" . vellumory-set-page-labels) ("A" . vellumory-attach-file)
                       ("D" . vellumory-remove-attachment) ("x" . vellumory-extract-pages)
                       ("s" . vellumory-split) ("t" . vellumory-export-text)
                       ("o" . vellumory-ocr) ("i" . vellumory-edit-metadata) ("w" . vellumory-stamp) ("f" . vellumory-fill-form)
                       ("E" . vellumory-encrypt) ("P" . vellumory-decrypt)
                       ("u" . vellumory-undo) ("U" . vellumory-redo) ("h" . vellumory-history)))
      (keymap-set map (car binding) (cdr binding)))
    map)
  "Reversible edits: every one is recorded and can be undone.")

(defconst vellumory--map-areas '(vellumory-link vellumory-annotation vellumory-form-field)
  "Ids of the areas in the image maps of page tiles.
Mouse events inside an area arrive prefixed with its id.")

(defvar vellumory-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "e" vellumory-edit-map)
    (keymap-set map "a" vellumory-annotate-map)
    (define-key map [remap text-scale-adjust] #'vellumory-text-scale-adjust)
    (define-key map [remap undo] #'vellumory-undo)
    (define-key map [remap undo-only] #'vellumory-undo)
    (define-key map [remap undo-redo] #'vellumory-redo)
    (dolist (binding '(("j" . vellumory-scroll-down) ("k" . vellumory-scroll-up)
                       ("C-n" . vellumory-scroll-down) ("C-p" . vellumory-scroll-up)
                       ("<down>" . vellumory-scroll-down) ("<up>" . vellumory-scroll-up)
                       ("<left>" . vellumory-scroll-left) ("<right>" . vellumory-scroll-right)
                       ("SPC" . vellumory-next-screen) ("DEL" . vellumory-previous-screen)
                       ("S-SPC" . vellumory-previous-screen)
                       ("C-v" . vellumory-next-screen) ("M-v" . vellumory-previous-screen)
                       ("<next>" . vellumory-next-screen) ("<prior>" . vellumory-previous-screen)
                       ("n" . vellumory-next-page) ("p" . vellumory-previous-page)
                       ("g" . vellumory-goto-page) ("M-g g" . vellumory-goto-page)
                       ("M-g l" . vellumory-goto-page-label)
                       ("<" . vellumory-first-page) (">" . vellumory-last-page)
                       ("M-<" . vellumory-first-page) ("M->" . vellumory-last-page)
                       ("<home>" . vellumory-first-page) ("<end>" . vellumory-last-page)
                       ("+" . vellumory-zoom-in) ("=" . vellumory-zoom-in) ("-" . vellumory-zoom-out)
                       ("0" . vellumory-recenter) ("w" . vellumory-fit-width) ("W" . vellumory-fit-width)
                       ("P" . vellumory-fit-page) ("H" . vellumory-fit-page) ("c" . vellumory-fit-content-width) ("2" . vellumory-cycle-spread)
                       ;; F5 to F9 are the user's (Key Binding Conventions).
                       ("C-c C-p" . vellumory-present)
                       ("v" . vellumory-visual) ("M-s l" . vellumory-consult-line)
                       ("t" . vellumory-page-text) ("r" . vellumory-reload) ("R" . vellumory-reload)
                       ("o" . vellumory-outline) ("i" . vellumory-goto-heading) ("L" . vellumory-goto-page-label)
                       ("T" . vellumory-thumbnails) ("s" . vellumory-sidebar-switch) ("C-c C-d" . vellumory-cycle-theme)
                       ("/" . vellumory-search) ("C-s" . vellumory-search) ("C-M-s" . vellumory-search-regexp)
                       ("M-n" . vellumory-search-next) ("M-p" . vellumory-search-previous)
                       ("M-s c" . vellumory-search-clear) ("M-s o" . vellumory-occur) ("M-w" . vellumory-copy-selection)
                       ("f" . vellumory-follow-link) ("I" . vellumory-details) ("V" . vellumory-validate) ("C-c C-c" . vellumory-syntax) ("d" . vellumory-diff)
                       ;; Every command in one menu; describe-mode stays on C-h m.
                       ("?" . vellumory-dispatch) ("C-c C-v" . vellumory-dispatch)
                       ("C-c C-r" . vellumory-reflow)
                       ;; Back and forth along jumps, as pdf-tools' B and N.
                       ("B" . vellumory-history-back) ("N" . vellumory-history-forward)
                       ("<remap> <write-file>" . vellumory-write-copy)))
      (keymap-set map (car binding) (cdr binding)))
    (dolist (event '(wheel-up wheel-down mouse-4 mouse-5 wheel-left wheel-right))
      (define-key map (vector event) #'vellumory-wheel))
    (dolist (event '(C-wheel-up C-wheel-down C-mouse-4 C-mouse-5))
      (define-key map (vector event) #'vellumory-wheel-zoom))
    (define-key map [down-mouse-1] #'vellumory-mouse-press)
    ;; SyncTeX backward search; the press would otherwise open the buffer menu.
    (define-key map [C-down-mouse-1] #'ignore)
    (define-key map [C-mouse-1] #'vellumory-sync-backward)
    (define-key map [mouse-1] #'ignore)
    (define-key map [double-mouse-1] #'vellumory-mouse-select-word)
    (define-key map [triple-mouse-1] #'vellumory-mouse-select-line)
    ;; Inside an image-map area (a link, an annotation, a form field) a mouse
    ;; event arrives prefixed with the area's id; without these it is undefined.
    (dolist (area vellumory--map-areas)
      (dolist (binding `((down-mouse-1 . vellumory-mouse-press) (mouse-1 . ignore) (drag-mouse-1 . ignore)
                         (double-mouse-1 . vellumory-mouse-select-word)
                         (triple-mouse-1 . vellumory-mouse-select-line)
                         (C-down-mouse-1 . ignore) (C-mouse-1 . vellumory-sync-backward)
                         ,@(mapcar (lambda (e) (cons e 'vellumory-wheel))
                                   '(wheel-up wheel-down mouse-4 mouse-5 wheel-left wheel-right))
                         ,@(mapcar (lambda (e) (cons e 'vellumory-wheel-zoom))
                                   '(C-wheel-up C-wheel-down C-mouse-4 C-mouse-5))))
        (define-key map (vector area (car binding)) (cdr binding))))
    ;; Searching a buffer's lines (consult, Doom's SPC s s) searches the document's.
    (define-key map [remap consult-line] #'vellumory-consult-line)
    (define-key map [remap +default/search-buffer] #'vellumory-consult-line)
    (define-key map [remap keyboard-quit] #'vellumory-search-quit)
    map))

(easy-menu-define vellumory-menu vellumory-mode-map "Document reading commands."
  '("Document"
    ["Go to page…" vellumory-goto-page] ["Go to page label…" vellumory-goto-page-label]
    ["Go to heading…" vellumory-goto-heading] ["Next page" vellumory-next-page]
    ["Previous page" vellumory-previous-page] "--"
    ["Fit width" vellumory-fit-width] ["Fit page" vellumory-fit-page] ["Fit content width (no margins)" vellumory-fit-content-width] ["Two pages (single, book, pairs)" vellumory-cycle-spread]
    ["Present full screen" vellumory-present]
    ["Read across windows (follow)" vellumory-follow-mode :style toggle :selected vellumory-follow-mode]
    ["Columns that follow each other" vellumory-follow-split]
    ["Zoom in" vellumory-zoom-in] ["Zoom out" vellumory-zoom-out]
    ["Cycle page colours" vellumory-cycle-theme] "--"
    ["Page previews" vellumory-thumbnails] ["Contents sidebar" vellumory-outline]
    ["Switch sidebar: previews or contents" vellumory-sidebar-switch] "--"
    ["Find text…" vellumory-search] ["Next match" vellumory-search-next]
    ["Previous match" vellumory-search-previous] ["Clear search" vellumory-search-clear]
    ["Copy selection" vellumory-copy-selection] ["Follow link…" vellumory-follow-link]
    ["Page text" vellumory-page-text] ["Document info…" vellumory-details] ["Validate (PDF/A, PDF/UA)…" vellumory-validate] ["PDF syntax (QDF)" vellumory-syntax] ["Compare with earlier version…" vellumory-diff] "--"
    ("Annotate"
     ["Highlight selection" vellumory-highlight] ["Underline selection" vellumory-underline]
     ["Strike out selection" vellumory-strike-out] ["Squiggly underline" vellumory-squiggly]
     ["Add note…" vellumory-add-note] ["List annotations" vellumory-annotations])
    ("Edit (reversible)"
     ["Edit metadata (title, author, language…)" vellumory-edit-metadata]
     ["Fill in form…" vellumory-fill-form]
     ["Stamp or watermark pages…" vellumory-stamp] ["Encrypt…" vellumory-encrypt] ["Remove encryption…" vellumory-decrypt]
     ["Rotate pages clockwise" vellumory-rotate-pages]
     ["Rotate counterclockwise" vellumory-rotate-pages-counterclockwise]
     ["Delete pages…" vellumory-delete-pages] ["Move page…" vellumory-move-page]
     ["Keep or reorder pages…" vellumory-keep-pages] ["Append documents…" vellumory-append-documents]
     ["Compress losslessly" vellumory-compress] ["Linearize" vellumory-linearize]
     ["Set page labels…" vellumory-set-page-labels] ["Attach file…" vellumory-attach-file]
     ["Remove attachment…" vellumory-remove-attachment] ["Recognize text (OCR)…" vellumory-ocr] "--"
     ["Undo" vellumory-undo] ["Redo" vellumory-redo] ["History tree" vellumory-history])
    ("Export"
     ["Extract pages…" vellumory-extract-pages] ["Split…" vellumory-split]
     ["Export text…" vellumory-export-text])
    ["Reload" vellumory-reload] ["Set bookmark…" bookmark-set]
    ["Reading mode (reflowed text)" vellumory-reflow]
    ["All commands…" vellumory-dispatch] ["Manual" vellumory-info]
    ["Check the setup (doctor)" vellumory-doctor] ["Help" describe-mode]))

(defun vellumory--page-string ()
  "Return the page or spread in view and the page count, such as \"3/10\".
Pages are named by `vellumory--page-description'; in a header or mode line,
the one in view in the window being drawn."
  (if vellumory--pages
      ;; In a header or mode line the selected window is the one drawn: each
      ;; window of the document (columns that follow each other) names its page.
      (let ((page (or (and (eq (window-buffer (selected-window)) (current-buffer))
                           (alist-get 'page (vellumory--reading-position (selected-window))))
                      vellumory--current-page 0)))
        (format "%s/%d" (mapconcat #'vellumory--page-description (vellumory--spread-pages page) "–")
                (length vellumory--pages)))
    "…"))

(defun vellumory--header-line ()
  "Return the header line: page, zoom, status, search status and caret."
  (replace-regexp-in-string
   "%" "%%"
   (format " Page %s   %d%%   %s%s%s"
           (vellumory--page-string) (round (* 100 vellumory--zoom)) vellumory--status
           (if (string-empty-p vellumory--search-status) "" (concat " | " vellumory--search-status))
           (vellumory-caret--indicator))
   t t))

(defvar save-place-mode)

(defun vellumory--forget-save-place ()
  "Keep documents out of saveplace's list.
Vellumory restores their exact position itself, and their point means
nothing.  `save-place-find-file-hook'
turns saving back on for files it knows; this runs after it, so a document's
old entry is dropped when it is next closed."
  (when (derived-mode-p 'vellumory-mode) (setq save-place-mode nil)))

(add-hook 'save-place-after-find-file-hook #'vellumory--forget-save-place)

;;;###autoload
(define-derived-mode vellumory-mode special-mode "Vellumory"
  "Read documents with continuous scrolling and incremental raster refinement.
\\{vellumory-mode-map}"
  ;; Batch sessions may drive the mode headlessly for tests and automation.
  (unless (or noninteractive (and (display-graphic-p) (image-type-available-p 'pbm)))
    (user-error "Vellumory needs a graphical Emacs with image support"))
  ;; Said plainly, rather than as Emacs's "Searching for program" error.
  (unless (or (and (file-name-absolute-p vellumory-program) (file-executable-p vellumory-program))
              (executable-find vellumory-program))
    (user-error "Vellumory's worker program `%s' was not found: M-x vellumory-doctor says how to get it, or set `vellumory-program'"
                vellumory-program))
  (let ((file (or vellumory--materialized (vellumory--materialize))))
    (setq vellumory--source-file (or vellumory--materialized
                                     (and buffer-file-name (not (file-remote-p buffer-file-name))
                                          (file-name-unquote (file-truename buffer-file-name))))
          vellumory--file file))
  (let ((inhibit-read-only t)) (erase-buffer))
  (set-buffer-multibyte t)
  ;; The buffer holds only images and spaces; reordering them is most of a
  ;; scroll step's redisplay (measured 1.27 ms with it, 0.52 ms without).
  ;; Right-to-left page labels turn it back on (`vellumory--load-navigation').
  (setq-local bidi-display-reordering nil)
  (setq buffer-undo-list t)
  ;; The reading position is kept exactly by Vellumory; see
  ;; `vellumory--forget-save-place'.
  (setq-local save-place-mode nil)
  ;; Scrolled sideways (zoomed, or fitted to the content) every line would show
  ;; a truncation arrow in the fringe: a page has no lines to continue.
  (setq-local fringe-indicator-alist (cons '(truncation nil nil) fringe-indicator-alist))
  (setq-local truncate-lines t cursor-type nil line-spacing 0 cursor-in-non-selected-windows nil
              auto-window-vscroll nil scroll-margin 0 scroll-conservatively 101
              ;; Point is not shown: horizontal scrolling is the reader's (zoom, fit to
              ;; the content), never redisplay's keeping point in view (with Doom's
              ;; hscroll-step it scrolled a fitted page 68 columns instead of 15).
              auto-hscroll-mode nil
              ;; Positions are exact pixels: in a window shorter than two strips no
              ;; row is wholly visible, and redisplay must not scroll to make one so.
              make-cursor-line-fully-visible nil
              ;; Point is never shown; a line highlight (Doom enables one in every
              ;; special-mode buffer) would only tint the margins of one strip.
              ;; An empty range: the window-sticky global mode (Doom's) does not
              ;; accept nil, which `hl-line-move' would.
              hl-line-range-function #'vellumory--no-line-highlight
              vellumory--spread vellumory-spread
              show-trailing-whitespace nil display-line-numbers nil
              buffer-auto-save-file-name nil backup-inhibited t
              revert-buffer-function (lambda (&rest _) (vellumory-reload))
              bookmark-make-record-function #'vellumory--bookmark-record
              imenu-create-index-function #'vellumory--imenu-index imenu-auto-rescan t
              mode-line-position '(:eval (concat " " (vellumory--page-string))))
  (face-remap-add-relative 'default 'vellumory-desk)
  (when vellumory-header-line
    (setq-local header-line-format '(:eval (vellumory--header-line))))
  (add-hook 'write-contents-functions #'vellumory--write-contents nil t)
  (add-hook 'kill-buffer-hook #'vellumory--cleanup nil t)
  (add-hook 'change-major-mode-hook #'vellumory--restore-source nil t)
  (add-hook 'post-command-hook #'vellumory--refresh nil t)
  (add-hook 'pre-command-hook #'vellumory--defer-gc nil t)
  (add-hook 'window-size-change-functions #'vellumory--window-changed nil t)
  (add-hook 'window-buffer-change-functions #'vellumory--window-buffer-changed nil t)
  (add-hook 'window-buffer-change-functions #'vellumory--narrow-window-reflow nil t)
  (add-hook 'window-size-change-functions #'vellumory--narrow-window-reflow nil t)
  (advice-add 'recenter :around #'vellumory--recenter-advice)
  ;; Frame-wide: a sidebar closes once its document leaves the frame.
  (add-hook 'window-buffer-change-functions #'vellumory--sidebar-follow)
  ;; Pages coloured after the theme follow it when it changes.
  (add-hook 'enable-theme-functions #'vellumory--theme-changed)
  (set-buffer-modified-p nil)
  (vellumory--start)
  (vellumory-reload)
  (vellumory--watch-file)
  (let ((buffer (current-buffer)))
    (setq vellumory--timer
          (run-with-timer .2 .2 (lambda () (when (buffer-live-p buffer)
                                             (with-current-buffer buffer (vellumory--refresh))))))
    (setq vellumory--state-timer
          (run-with-idle-timer 2 t (lambda () (when (buffer-live-p buffer)
                                               (with-current-buffer buffer (vellumory--save-position))))))))

;;;###autoload
(defun vellumory-open (file &optional no-select)
  "Visit FILE in `vellumory-mode'.  Return the buffer; select it unless NO-SELECT."
  (interactive "fDocument: ")
  (unless (file-exists-p file) (user-error "No such file: %s" file))
  ;; Select this mode directly: other document modes would first convert or
  ;; hash the whole file (DocView computes an MD5 of every byte).
  (let* ((this (concat "\\`" (regexp-opt (delete-dups (list (expand-file-name file) (file-truename file))))
                       "\\'"))
         (buffer (let ((auto-mode-alist (cons (cons this 'vellumory-mode) auto-mode-alist))
                       ;; Only this file: a catch-all handler would also see
                       ;; `load' of the libraries the mode needs.
                       (file-name-handler-alist (cons (cons this #'vellumory--file-handler)
                                                      file-name-handler-alist))
                       (magic-mode-alist nil) (large-file-warning-threshold nil))
                   (find-file-noselect file nil nil))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vellumory-mode) (vellumory-mode)))
    (unless no-select (pop-to-buffer-same-window buffer))
    buffer))

;; Right-click menus in the reader, its sidebars and its views.
(require 'vellumory-link-preview)
(require 'vellumory-context)
(vellumory-context-install)

(provide 'vellumory)
;;; vellumory.el ends here
