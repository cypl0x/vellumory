;;; gui-tests.el --- Real redisplay tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'vellumory)

(defun vellumory-gui--kill (buffer)
  "Kill BUFFER, first showing another buffer in each window showing it.
A window left with nothing to show is deleted by `kill-buffer', and with it
its frame when it was the only window there and another frame exists
\(`replace-buffer-in-windows'): under Doom, whose buffer skipping finds
nothing to show more readily, the review frame itself."
  (when (buffer-live-p buffer)
    (let ((other (get-buffer "reader.pdf")))
      (dolist (window (get-buffer-window-list buffer nil t))
        (unless (window-parameter window 'window-side)
          (set-window-dedicated-p window nil)
          (set-window-buffer window (if (and other (not (eq other buffer))) other
                                      (get-scratch-buffer-create))))))
    (kill-buffer buffer)))

(defun vellumory-gui--settle (&optional seconds)
  "Pump redisplay and worker output until no render or search work remains."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (setq vellumory--last-motion 0.0)
    (vellumory--refresh t)
    (while (and (< (float-time) deadline)
                (or vellumory--search-job vellumory--search-busy
                    (> (hash-table-count vellumory--inflight) 0)
                    (and vellumory--thumbnail-inflight
                         (> (hash-table-count vellumory--thumbnail-inflight) 0))
                    vellumory--thumbnail-queue
                    (cl-some #'vellumory--needs-sharp vellumory--visible)))
      (accept-process-output vellumory--process .03)
      (redisplay t)
      (vellumory--refresh)
      (vellumory--update-thumbnails)))
  (should (zerop (hash-table-count vellumory--inflight)))
  (should-not vellumory--search-job))

(defun vellumory-gui--ppm-pixel (file x y)
  "RGB of pixel X,Y of binary PPM FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (dotimes (_ 3) (forward-line 1))
    (let* ((width (progn (goto-char (point-min)) (forward-line 1)
                         (string-to-number (buffer-substring (point) (line-end-position)))))
           (start (progn (goto-char (point-min)) (forward-line 3) (point)))
           (i (+ start (* 3 (+ x (* y width))))))
      (list (char-after i) (char-after (1+ i)) (char-after (+ 2 i))))))

(ert-deftest vellumory-gui-sharp-visible-tiles-and-bounded-images ()
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-fit-width)
    (vellumory-first-page)
    (vellumory-gui--settle)
    (should vellumory--visible)
    (dolist (tile vellumory--visible)
      (should (= (vellumory--tile-quality tile) 2))
      (let ((size (image-size (vellumory--tile-image tile) t)))
        (should (= (car size) (vellumory--tile-width tile)))
        (should (= (cdr size) (vellumory--tile-height tile)))))
    ;; Fit width at this window size uses one tile per row.
    (should (cl-every (lambda (tile) (= (vellumory--tile-width tile) vellumory--width)) vellumory--visible))
    ;; Images are only held near the viewport.
    (let ((holders 0))
      (cl-loop for row across vellumory--rows do
               (cl-loop for tile across (vellumory--row-tiles row)
                        when (vellumory--tile-image tile) do (cl-incf holders)
                        and do (should (memq tile vellumory--visible-and-near))))
      (should (> holders 0))
      (should (<= holders (length vellumory--visible-and-near))))
    ;; Requests are sized in device pixels.
    (should (= vellumory--scale (vellumory--device-scale)))))

(ert-deftest vellumory-gui-worker-highlights-and-cached-clear ()
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-first-page)
    (vellumory-gui--settle)
    (let ((plain (mapcar (lambda (tile) (plist-get (cdr (vellumory--tile-image tile)) :file))
                         vellumory--visible)))
      (vellumory--search-begin "text" nil '((page . 0) (y . 0)))
      (vellumory-gui--settle)
      (should (equal vellumory--search-current '(0 . 0)))
      (should (= (length (alist-get 'hits (gethash 0 vellumory--search-cache))) 17))
      (let ((hit (aref (alist-get 'hits (gethash 0 vellumory--search-cache)) 0)))
        (should (< (abs (- (max 0 (- (vellumory--hit-top hit) 36))
                           (alist-get 'y (vellumory--reading-position))))
                   1)))
      ;; Highlighted tiles carry an overlay in their raster signature.
      (should (cl-some (lambda (tile) (cadr (vellumory--tile-signature tile))) vellumory--visible))
      (let ((position (vellumory--reading-position)))
        (vellumory-zoom-in)
        (vellumory-gui--settle)
        (should (< (abs (- (alist-get 'y position) (alist-get 'y (vellumory--reading-position)))) 1))
        (vellumory-fit-width)
        (vellumory--restore-position position)
        (vellumory-gui--settle))
      ;; Clearing returns to the cached plain rasters: no new pixels.
      (let ((pixels vellumory--rendered-pixels))
        (vellumory-search-clear)
        (vellumory-gui--settle)
        (should (= pixels vellumory--rendered-pixels))
        (should-not (cl-some (lambda (tile) (cadr (vellumory--tile-signature tile))) vellumory--visible))
        (vellumory-first-page)
        (vellumory-gui--settle)
        (should (equal plain (mapcar (lambda (tile) (plist-get (cdr (vellumory--tile-image tile)) :file))
                                     vellumory--visible)))))))

(ert-deftest vellumory-gui-search-keyboard-and-new-origin ()
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-search-clear)
    (vellumory-first-page)
    (execute-kbd-macro (kbd "/ t e x t RET"))
    (vellumory-gui--settle)
    (should (equal vellumory--search-current '(0 . 0)))
    (should (zerop (minibuffer-depth)))
    ;; Starting the same query again searches from the new reading position.
    (vellumory-goto-page 2)
    (execute-kbd-macro (kbd "/ t e x t RET"))
    (vellumory-gui--settle)
    (should (equal vellumory--search-current '(1 . 0)))
    (execute-kbd-macro (kbd "M-p"))
    (vellumory-gui--settle)
    (should (equal vellumory--search-current '(0 . 16)))
    (execute-kbd-macro (kbd "M-s c"))
    (should (string-empty-p vellumory--search-query))
    (vellumory-first-page)))

(ert-deftest vellumory-gui-zoom-tiles-only-visible-columns ()
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-first-page)
    (vellumory--set-zoom 3.0)
    (vellumory-gui--settle)
    (should (> vellumory--width (window-body-width nil t)))
    (let ((row (vellumory--tile-row (car vellumory--visible))))
      (should (> (length (vellumory--row-tiles row)) 1)))
    ;; Columns right of the window stay unrendered.
    (should (cl-every (lambda (tile) (< (vellumory--tile-left tile) (window-body-width nil t)))
                      vellumory--visible))
    (should (cl-some (lambda (row)
                       (cl-some (lambda (tile) (and (null (vellumory--tile-image tile))
                                                    (> (vellumory--tile-left tile) (window-body-width nil t))))
                                (vellumory--row-tiles row)))
                     (mapcar #'vellumory--tile-row vellumory--visible)))
    (vellumory-scroll-right 40)
    (vellumory-gui--settle)
    (should (cl-some (lambda (tile) (> (vellumory--tile-left tile) 0)) vellumory--visible))
    (should (cl-every (lambda (tile) (= (vellumory--tile-quality tile) 2)) vellumory--visible))
    (set-window-hscroll nil 0)
    (vellumory-fit-width)
    (vellumory-gui--settle)))

(ert-deftest vellumory-gui-exact-scrolling-and-position ()
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-fit-width)
    (should (= (vellumory--page-height 0) (ceiling (* vellumory--width (/ 792.0 612)))))
    ;; Offsets clamp to the last full screen, whatever the window size.
    (let ((limit (- (vellumory--total-height) (window-body-height nil t))))
      (dolist (offset (list 0 123 321 677 1221 100 1 192 193 (+ limit 500)))
        (vellumory--seek offset)
        (vellumory--refresh t)
        (redisplay t)
        (should (= (vellumory--absolute-position) (min offset limit)))))
    (vellumory--seek 321)
    (redisplay t)
    (let ((position (vellumory--reading-position)))
      (vellumory--save-position)
      (should (equal position (vellumory--load-position)))
      (vellumory--seek 0)
      (vellumory--restore-position (vellumory--load-position))
      (redisplay t)
      (should (= (vellumory--absolute-position) 321))
      (vellumory-zoom-in)
      (redisplay t)
      (should (< (abs (- (alist-get 'y position)
                         (alist-get 'y (vellumory--reading-position)))) 1.0)))
    (vellumory-fit-width)
    (vellumory-first-page)))

(ert-deftest vellumory-gui-links-history-and-selection ()
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-first-page)
    (vellumory-gui--settle)
    (let ((deadline (+ (float-time) 5)))
      (while (and (not (vectorp (gethash 0 vellumory--links))) (< (float-time) deadline))
        (accept-process-output vellumory--process .05)))
    (let ((internal (cl-find-if (lambda (l) (alist-get 'page l)) (gethash 0 vellumory--links))))
      (should internal)
      (vellumory-gui--settle)
      ;; Sharp tiles over links carry a hand-pointer image map.
      (should (cl-some (lambda (tile) (plist-get (cdr (get-text-property (vellumory--tile-position tile) 'display))
                                                 :map))
                       vellumory--visible))
      (vellumory--visit-link internal)
      (redisplay t)
      (should (= (alist-get 'page (vellumory--reading-position)) 2))
      (vellumory-history-back)
      (redisplay t)
      (should (= (alist-get 'page (vellumory--reading-position)) 0)))
    ;; Select the heading line on page 1 and copy it.
    (let ((done nil))
      (vellumory--select-range '(0 42 40) '(0 300 60) 'chars
                               (lambda (s) (vellumory--set-selection s) (setq done t)))
      (let ((deadline (+ (float-time) 5)))
        (while (and (not done) (< (float-time) deadline)) (accept-process-output vellumory--process .05))))
    (should (string-match-p "Vellumory" (vellumory-selection-text)))
    (vellumory-gui--settle)
    (should (cl-some (lambda (tile) (cadr (vellumory--tile-signature tile))) vellumory--visible))
    (vellumory-copy-selection)
    (should (string-match-p "Vellumory" (current-kill 0)))
    (vellumory-clear-selection)
    (vellumory-gui--settle)))

(ert-deftest vellumory-gui-theme-recolours-rasters ()
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-first-page)
    (vellumory--seek 400)
    (vellumory-gui--settle)
    ;; Near a tile's bottom-left corner is page margin in every strip (the
    ;; header band only spans the top of the first).
    (cl-flet ((margin (tile) (list 5 (- (round (* vellumory--scale (vellumory--tile-height tile))) 5))))
      (let ((tile (car vellumory--visible)))
        (should (equal (apply #'vellumory-gui--ppm-pixel (plist-get (cdr (vellumory--tile-image tile)) :file)
                              (margin tile))
                       '(255 255 255))))
      (let ((vellumory-theme 'invert))
        (vellumory--layout-preserving)
        (vellumory-gui--settle)
        (let* ((tile (car vellumory--visible))
               (rgb (apply #'vellumory-gui--ppm-pixel (plist-get (cdr (vellumory--tile-image tile)) :file)
                           (margin tile))))
          (should (cl-every (lambda (c) (< c 16)) rgb)))))
    (vellumory--layout-preserving)
    (vellumory-gui--settle)
    (vellumory-first-page)))

(ert-deftest vellumory-gui-thumbnails-render-visit-and-release ()
  (skip-unless (display-graphic-p))
  (let ((source (get-buffer "reader.pdf")))
    (with-selected-window (get-buffer-window source t)
      (unless (and (buffer-live-p vellumory--thumbnail-buffer)
                   (get-buffer-window vellumory--thumbnail-buffer))
        (vellumory-thumbnails))
      (vellumory-first-page)
      (redisplay t)
      (vellumory-gui--settle)
      (should (= (length vellumory--thumbnails) 3))
      (should (eq (vellumory--thumbnail-state (aref vellumory--thumbnails 0)) 'ready))
      (dolist (item vellumory--thumbnail-visible)
        (let ((size (image-size (vellumory--thumbnail-image item) t)))
          (should (= (car size) (vellumory--thumbnail-width item)))
          (should (= (cdr size) (vellumory--thumbnail-height item)))))
      (let ((sidebar vellumory--thumbnail-buffer))
        (with-selected-window (get-buffer-window sidebar)
          (goto-char (point-min))
          (vellumory-thumbnail-next)
          (vellumory-thumbnail-visit)))
      (redisplay t)
      (should (= (alist-get 'page (vellumory--reading-position)) 1))
      (should (< (alist-get 'y (vellumory--reading-position)) 1))
      (vellumory-thumbnails)
      (redisplay t)
      (vellumory--refresh t)
      (should-not vellumory--thumbnail-visible)
      (should-not vellumory--thumbnail-queue)
      (should (cl-every (lambda (item) (null (vellumory--thumbnail-image item))) vellumory--thumbnails))
      ;; Closing the sidebar changes reader width but keeps its PDF position.
      (should (= (alist-get 'page (vellumory--reading-position)) 1))
      (should (< (alist-get 'y (vellumory--reading-position)) 1))
      (vellumory-first-page))))

(ert-deftest vellumory-gui-many-page-preview-window ()
  (skip-unless (display-graphic-p))
  (let ((file (expand-file-name "target/review/long-reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
        (vellumory-show-thumbnails t) buffer)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (let ((deadline (+ (float-time) 10)))
              (while (and (< (float-time) deadline) (not (eq vellumory--navigation-state 'ready)))
                (accept-process-output vellumory--process .05)))
            (should (= (length vellumory--pages) 80))
            (should (get-buffer-window vellumory--thumbnail-buffer))
            ;; Re-showing the sidebar after reading elsewhere follows the reader.
            (vellumory-thumbnails)
            (vellumory-goto-page 70)
            (vellumory-thumbnails)
            (redisplay t)
            (vellumory-gui--settle)
            (should (memq (aref vellumory--thumbnails 69) vellumory--thumbnail-visible))
            (vellumory-goto-page 40)
            (redisplay t)
            (vellumory-gui--settle)
            (should (< (length vellumory--thumbnail-visible) 15))
            (should (memq (aref vellumory--thumbnails 39) vellumory--thumbnail-visible))
            (should (eq (vellumory--thumbnail-state (aref vellumory--thumbnails 39)) 'ready))
            (should-not (vellumory--thumbnail-image (aref vellumory--thumbnails 0)))
            (should (= (cl-count-if #'vellumory--thumbnail-image vellumory--thumbnails)
                       (length vellumory--thumbnail-visible)))
            ;; Reader tiles exist only around page 40.
            (should (cl-every (lambda (tile) (<= 37 (vellumory--row-page (vellumory--tile-row tile)) 42))
                              vellumory--visible-and-near))
            (let ((sidebar vellumory--thumbnail-buffer)
                  (item (aref vellumory--thumbnails 59)))
              (with-selected-window (get-buffer-window sidebar)
                (goto-char (vellumory--thumbnail-position item))
                (set-window-start nil (1- (point)) t)
                (redisplay t)
                (with-current-buffer buffer
                  (vellumory-gui--settle)
                  ;; Browsing previews alone must not move the reader.
                  (should (= (alist-get 'page (vellumory--reading-position)) 39))
                  (should-not (vellumory--thumbnail-image (aref vellumory--thumbnails 39))))
                (vellumory-thumbnail-visit)))
            (redisplay t)
            (with-current-buffer buffer
              (should (= (alist-get 'page (vellumory--reading-position)) 59))))
        (when (buffer-live-p buffer) (vellumory-gui--kill buffer))))))

(ert-deftest vellumory-gui-outline-navigation-and-resize ()
  (skip-unless (display-graphic-p))
  (let ((source (get-buffer "reader.pdf")))
    (with-selected-window (get-buffer-window source t)
      (let ((deadline (+ (float-time) 5)))
        (while (and (eq vellumory--navigation-state 'loading) (< (float-time) deadline))
          (accept-process-output vellumory--process .05)))
      (should (eq vellumory--navigation-state 'ready))
      (should (equal vellumory--labels ["i" "Ch-1" "Ch-2"]))
      (vellumory--seek 321)
      (redisplay t)
      (let ((before (alist-get 'y (vellumory--reading-position))))
        (vellumory-outline)
        (with-current-buffer source
          (vellumory--refresh t)
          (redisplay t)
          (should (< (abs (- before (alist-get 'y (vellumory--reading-position)))) 1))))
      (let ((contents (buffer-local-value 'vellumory--outline-buffer source)))
        (with-selected-window (get-buffer-window contents)
          (goto-char (point-min))
          (vellumory-outline-activate))
        (with-selected-window (get-buffer-window source)
          (redisplay t)
          (should (= (alist-get 'page (vellumory--reading-position)) 0))
          (should (< (abs (- (alist-get 'y (vellumory--reading-position)) 192)) 1)))
        (with-selected-window (get-buffer-window contents)
          (goto-char (point-min))
          (outline-next-visible-heading 1)
          (vellumory-outline-activate))
        (with-selected-window (get-buffer-window source)
          (redisplay t)
          (should (= (alist-get 'page (vellumory--reading-position)) 1))
          (should (< (alist-get 'y (vellumory--reading-position)) 1))
          (vellumory-outline)
          (vellumory--refresh t)
          (redisplay t)
          (should (= (alist-get 'page (vellumory--reading-position)) 1))
          (vellumory-first-page))))))

(ert-deftest vellumory-gui-displayed-geometry-matches-layout ()
  "Redisplay must place every row exactly where the layout model says."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    ;; Short strips: several whole rows in view to compare with the model.
    (let ((vellumory-strip-height 192))
      (dolist (zoom '(1.0 0.6 1.7))
        (vellumory--set-zoom zoom)
        (dolist (offset (list 0 (- (vellumory--page-offset 1) 300)))
          (vellumory--seek offset)
          (redisplay t)
          (let* ((window (selected-window))
                 (start (vellumory--row-index-at (window-start window)))
                 (checked 0))
            ;; `window-line-height' reads the display matrix: each screen line
            ;; must have its model height and follow the previous one exactly.
            (cl-loop for n from 1
                     for line = (window-line-height n window)
                     for row-index = (+ start n)
                     ;; Past the last displayed line Emacs repeats that line's entry.
                     while (and line (= (nth 1 line) n) (< row-index (length vellumory--rows))
                                (zerop (nth 3 line))
                                (<= (+ (nth 2 line) (car line))
                                    (+ (window-header-line-height window) (window-body-height window t))))
                     do (let ((above (window-line-height (1- n) window)))
                          (should (= (car line) (vellumory--row-height (aref vellumory--rows row-index))))
                          ;; Line 0 may be clipped by vscroll; chain from line 1 on.
                          (when (> n 1) (should (= (nth 2 line) (+ (nth 2 above) (car above)))))
                          (cl-incf checked)))
            ;; Small frames (Doom's) may show only two full lines at high zoom.
            (should (>= checked 1))))))
    (vellumory-fit-width)
    (vellumory-first-page)))

(defun vellumory-gui--can-capture-p ()
  "Whether the selected frame's pixels can be read: exported by Emacs where
it draws with Cairo (X, pgtk), or captured from the screen on macOS."
  (and (display-graphic-p) (executable-find "magick")
       (or (fboundp 'x-export-frames) (executable-find "screencapture"))))

(defun vellumory-gui--near (a b)
  "Whether colours A and B, lists of RGB, differ by at most 2 per channel.
macOS colour-manages a screen capture: pure green comes back (1 255 0)."
  (cl-every (lambda (x y) (<= (abs (- x y)) 2)) a b))

(defun vellumory-gui--capture-frame (png)
  "Write what redisplay painted in the selected frame to PNG.
Return how many of its pixels make one frame pixel (2 on a Retina display)."
  (if (fboundp 'x-export-frames)
      (let ((coding-system-for-write 'no-conversion))
        (write-region (x-export-frames nil 'png) nil png nil 'silent)
        1)
    ;; macOS: its Cocoa build exports no frames; capture the frame's area.
    ;; Emacs keeps the position it last set (`f->top_pos', nsfns.m), which
    ;; goes stale when macOS moves the window itself: setting it again makes
    ;; both agree.
    (let ((position (frame-position))) (set-frame-position nil (car position) (cdr position)))
    (redisplay t)
    (sit-for 0.2)
    (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges nil 'native-edges)))
      ;; Where the capture lands is the screen's say, not Emacs's: log both.
      (message "capture: native %S outer %S geometry %S" (frame-edges nil 'native-edges)
               (frame-edges nil 'outer-edges) (frame-geometry))
      (should (zerop (call-process "screencapture" nil nil nil "-x"
                                   (format "-R%d,%d,%d,%d" left top (- right left) (- bottom top))
                                   png)))
      (with-temp-buffer
        (should (zerop (call-process "magick" nil t nil "identify" "-format" "%w" png)))
        (/ (float (string-to-number (buffer-string))) (- right left))))))

(defmacro vellumory-gui--with-frame-pixels (pixel &rest body)
  "Run BODY with PIXEL bound to a function of X Y giving the RGB that
redisplay painted there, from one capture of the selected frame."
  (declare (indent 1))
  `(let ((png (make-temp-file "vellumory-frame-" nil ".png"))
         (ppm (make-temp-file "vellumory-frame-" nil ".ppm")))
     (unwind-protect
         (let ((scale (vellumory-gui--capture-frame png)))
           (should (zerop (call-process "magick" nil nil nil png ppm)))
           (let ((,pixel (lambda (x y) (vellumory-gui--ppm-pixel ppm (round (* scale x)) (round (* scale y))))))
             ,@body))
       (delete-file png) (delete-file ppm))))

(ert-deftest vellumory-gui-margins-are-desk-on-every-line ()
  "The centring margins are desk-coloured on every visible strip: on the
window's first line, and on point's line under a line highlight."
  (skip-unless (vellumory-gui--can-capture-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (unwind-protect
        (progn
          ;; As in Doom, which enables line highlighting in every special-mode buffer.
          (require 'hl-line)
          (hl-line-mode 1)
          (vellumory--set-zoom 0.6)
          (vellumory-first-page)
          ;; Start the window inside a row, as after any scroll.
          (vellumory--seek 300)
          (vellumory-gui--settle)
          ;; What the command loop runs after a scroll command.
          (hl-line-highlight)
          (redisplay t)
          (should (> vellumory--margin 8))
          (let* ((desk (mapcar (lambda (c) (/ c 257)) (color-values (face-background 'vellumory-desk nil t))))
                 (edges (window-body-pixel-edges))
                 (top (vellumory--absolute-position))
                 (bottom (+ top (window-body-height nil t)))
                 (first (vellumory--row-index-at (window-start)))
                 ;; Visible page strips (gaps are desk-coloured throughout).
                 (rows (cl-loop for i from first below (length vellumory--rows)
                                for row = (aref vellumory--rows i)
                                while (< (vellumory--row-offset row) bottom)
                                unless (vellumory--row-gap row) collect row)))
            (should (> (vellumory--row-offset (car rows)) (- top (vellumory--row-height (car rows)))))
            (should (< (vellumory--row-offset (car rows)) top))   ; the first strip is clipped
            (should (>= (length rows) 2))
            (vellumory-gui--with-frame-pixels pixel
              (dolist (row rows)
                (let* ((visible-top (max top (vellumory--row-offset row)))
                       (visible-bottom (min bottom (+ (vellumory--row-offset row) (vellumory--row-height row))))
                       (y (+ (nth 1 edges) (- (/ (+ visible-top visible-bottom) 2) top))))
                  (should (equal (list (vellumory--row-offset row) (funcall pixel (+ (nth 0 edges) (/ vellumory--margin 2)) y))
                                 (list (vellumory--row-offset row) desk)))
                  (should (equal (list (vellumory--row-offset row) (funcall pixel (- (nth 2 edges) (/ vellumory--margin 2)) y))
                                 (list (vellumory--row-offset row) desk)))
                  ;; The page itself starts right after the margin: paper or
                  ;; ink, not desk (page 3 begins with a navy band, in view
                  ;; in a window as tall as Doom's at 736 by 648 pixels).
                  (should-not (equal (list (vellumory--row-offset row)
                                           (funcall pixel (+ (nth 0 edges) vellumory--margin 4) y))
                                     (list (vellumory--row-offset row) desk))))))))
      (hl-line-mode -1)
      (vellumory-fit-width)
      (vellumory-first-page)
      (vellumory-gui--settle))))

(ert-deftest vellumory-gui-new-windows-show-the-same-place ()
  "A window split off a reader, below or beside, and a window showing it again
show its place, as image-mode's new windows take the latest one's properties.
Emacs copies a split window's start, not its vscroll."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (vellumory-fit-width)
    (vellumory-first-page)
    ;; Inside a strip: the part a split does not copy.
    (vellumory--seek (+ (vellumory--row-offset (aref vellumory--rows 2)) 37))
    (vellumory-gui--settle)
    (should (> (window-vscroll nil t) 0))
    ;; Doom's splits recenter the window afterwards, as evil's zz does.
    (let ((place (vellumory--absolute-position (selected-window))))
      (recenter)
      (vellumory-gui--settle)
      (redisplay t)
      (should (= (vellumory--absolute-position (selected-window)) place)))
    ;; Below: the same width and layout, so the same pixel.
    (let ((place (vellumory--absolute-position (selected-window)))
          (other (split-window-below)))
      (unwind-protect
          (progn (vellumory-gui--settle)
                 (redisplay t)
                 (should (= (vellumory--absolute-position other) place))
                 (should (= (vellumory--absolute-position (selected-window)) place)))
        (delete-window other)))
    (vellumory-gui--settle)
    ;; Beside: both halves are laid out anew, each at the same point of the page.
    (let* ((before (vellumory--reading-position)) (other (split-window-right)))
      (unwind-protect
          (progn (vellumory-gui--settle)
                 (redisplay t)
                 (dolist (window (list (selected-window) other))
                   (let ((position (vellumory--reading-position window))
                         ;; A window scrolls no further than to show the
                         ;; document's end; in a narrow frame the pages are
                         ;; short enough that the place lies beyond.
                         (limit (max 0 (- (vellumory--total-height) (window-body-height window t)))))
                     (if (> (vellumory--offset-of (alist-get 'page before) (alist-get 'y before)) limit)
                         (should (= (vellumory--absolute-position window) limit))
                       (should (equal (alist-get 'page position) (alist-get 'page before)))
                       ;; To a pixel of the narrower layout.
                       (should (< (abs (- (alist-get 'y position) (alist-get 'y before)))
                                  (* 1.01 (vellumory--points-per-pixel (alist-get 'page before)))))))))
        (delete-window other)))
    (vellumory-gui--settle)
    ;; Closing every other window: Emacs recomputes the remaining window's start
    ;; and resets its vscroll (delete-other-windows-internal).
    (let ((before (vellumory--reading-position)))
      (split-window-below)
      (split-window-right)
      (vellumory-gui--settle)
      (delete-other-windows)
      (vellumory-gui--settle)
      (redisplay t)
      (let ((position (vellumory--reading-position)))
        (should (equal (alist-get 'page position) (alist-get 'page before)))
        (should (< (abs (- (alist-get 'y position) (alist-get 'y before)))
                   (* 1.01 (vellumory--points-per-pixel (alist-get 'page before)))))))
    (vellumory-gui--settle)
    ;; Shown again after another buffer.
    (let ((place (vellumory--absolute-position (selected-window))) (reader (current-buffer))
          (other (get-buffer-create " *vellumory-gui-other*")))
      (unwind-protect
          (progn (switch-to-buffer other t t)
                 (redisplay t)
                 (switch-to-buffer reader t t)
                 (vellumory-gui--settle)
                 (redisplay t)
                 (should (= (vellumory--absolute-position (selected-window)) place)))
        (switch-to-buffer reader t t)
        (kill-buffer other)))
    (vellumory-first-page)
    (vellumory-gui--settle)))

(ert-deftest vellumory-gui-seek-holds-when-no-row-is-fully-visible ()
  "A window shorter than two strips, scrolled into a strip, shows only
clipped rows; redisplay must keep the exact pixel position anyway."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (let ((vellumory-strip-height 192)
          ;; The mode and header lines' height is the font's: a body of 286
          ;; pixels on every system, not a window of 330.
          (chrome (- (window-pixel-height) (window-body-height nil t))))
      (vellumory-fit-width)
      (let ((other (split-window nil (+ 286 chrome) 'below t)))
        (unwind-protect
            (let* ((height (window-body-height nil t))
                   (row (aref vellumory--rows 1))
                   (next (aref vellumory--rows 2))
                   (offset (+ (vellumory--row-offset row) 40)))
              (should (< height (+ (vellumory--row-height row) (vellumory--row-height next))))
              ;; Neither row is wholly inside [offset, offset + height).
              (should (> (+ (vellumory--row-offset next) (vellumory--row-height next)) (+ offset height)))
              (vellumory--seek offset)
              (vellumory-gui--settle)
              (redisplay t)
              (should (= (vellumory--absolute-position) offset))
              (redisplay t)
              (should (= (vellumory--absolute-position) offset)))
          (delete-window other))))
    ;; Back to the default strips.
    (vellumory--layout-preserving)
    (vellumory-first-page)
    (vellumory-gui--settle)))

(ert-deftest vellumory-gui-two-page-spread ()
  "Book spreads: pages 2 and 3 side by side, sharp, with a desk gutter between."
  (skip-unless (vellumory-gui--can-capture-p))
  (let ((file (expand-file-name "target/review/long-reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
        (vellumory-spread 'book) buffer)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (delete-other-windows)
            (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
            (vellumory-goto-page 2)
            (vellumory-gui--settle)
            (redisplay t)
            (should (equal (vellumory--spread-pages vellumory--current-page) '(1 2)))
            (should (string-match-p "–" (vellumory--page-string)))
            (should (<= (vellumory--row-width) (window-body-width nil t)))
            (let ((row (vellumory--row-at (window-start))))
              (should (equal (mapcar #'vellumory--tile-page (vellumory--row-tiles row)) '(1 2)))
              (should (cl-every (lambda (tile) (= (vellumory--tile-quality tile) 2)) (vellumory--row-tiles row)))
              (let* ((desk (mapcar (lambda (c) (/ c 257)) (color-values (face-background 'vellumory-desk nil t))))
                     (edges (window-body-pixel-edges))
                     (y (+ (nth 1 edges) (/ (vellumory--row-height row) 2)))
                     (left (+ (nth 0 edges) vellumory--margin)))
                (vellumory-gui--with-frame-pixels pixel
                  (should (equal (funcall pixel (+ left 10) y) '(255 255 255)))
                  (should (equal (funcall pixel (+ left vellumory--width (/ vellumory-spread-gap 2)) y) desk))
                  (should (equal (funcall pixel (+ left vellumory--width vellumory-spread-gap 10) y)
                                 '(255 255 255))))))
            ;; Next goes to the next spread; cycling returns to single pages.
            (vellumory-next-page)
            (should (= vellumory--current-page 3))
            (vellumory-cycle-spread)
            (should (eq vellumory--spread 'pairs))
            (vellumory-cycle-spread)
            (should-not vellumory--spread)
            (should (= (vellumory--columns) 1)))
        (when (buffer-live-p buffer) (vellumory-gui--kill buffer))))))

(defun vellumory-test-gui--wait (predicate &optional seconds)
  "Pump worker output until PREDICATE holds or SECONDS pass."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(ert-deftest vellumory-gui-opens-as-the-document-asks ()
  "/PageLayout /TwoColumnRight opens book spreads, /PageMode /UseOutlines the
contents sidebar in place of the previews; the reader keeps focus."
  (let ((file (expand-file-name "target/review/book-reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
        (vellumory-show-thumbnails t) (vellumory-spread nil) buffer)
    (save-window-excursion
      (unwind-protect
          (progn
            (delete-other-windows)
            (setq buffer (vellumory-open file))
            (should (vellumory-test-gui--wait (lambda () (buffer-local-value 'vellumory--pages buffer))))
            (vellumory-gui--settle)
            (with-current-buffer buffer
              (should (eq vellumory--spread 'book))
              (should (equal (vellumory--spread-pages 1) '(1 2)))
              (should (buffer-live-p vellumory--outline-buffer))
              (should (get-buffer-window vellumory--outline-buffer))
              (should-not (and (buffer-live-p vellumory--thumbnail-buffer)
                               (get-buffer-window vellumory--thumbnail-buffer)))
              (should (eq (window-buffer (selected-window)) buffer))
              (should (string-match-p "Reading" (with-current-buffer vellumory--outline-buffer (buffer-string))))))
        (when (buffer-live-p buffer)
          (let ((outline (buffer-local-value 'vellumory--outline-buffer buffer)))
            (vellumory-gui--kill buffer)
            (when (buffer-live-p outline) (kill-buffer outline))))))))

(ert-deftest vellumory-gui-presentation-full-screen ()
  "Presenting: a frame of its own, each page fitted, centred on black and sharp
at device resolution; a click on a link follows it, b blanks the screen, and
leaving returns to the reader at the page last shown."
  (skip-unless (vellumory-gui--can-capture-p))
  (let* ((file (expand-file-name "target/review/reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
        (reader-frame (selected-frame)) (existing (find-buffer-visiting file)) buffer presentation)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (delete-other-windows)
            (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
            (vellumory-first-page)
            (setq presentation (vellumory-present))
            (cl-flet ((shown (page)
                        (vellumory-test-gui--wait
                         (lambda () (with-current-buffer presentation
                                      (redisplay t)
                                      (text-property-any (point-min) (point-max) 'vellumory-present-page page))))))
              (should (shown 0))
              (let* ((window (get-buffer-window presentation t))
                     (frame (window-frame window)))
                (should-not (eq frame reader-frame))
                (should (frame-parameter frame 'vellumory-presentation))
                (should (eq (selected-frame) frame))
                (should-not (window-parameter window 'mode-line-format))
                (with-current-buffer presentation
                  (pcase-let* ((`(,w ,h ,left ,top) (vellumory-present--fit 0))
                               (edges (window-body-pixel-edges window))
                               (x0 (+ (nth 0 edges) left)) (y0 (+ (nth 1 edges) top))
                               (image (get-text-property
                                       (text-property-any (point-min) (point-max) 'vellumory-present-page 0)
                                       'display)))
                    (should (equal (image-size image t) (cons w h)))
                    ;; Sharp: the raster has one pixel per device pixel.
                    (should (= (car (image-size (create-image (plist-get (cdr image) :file) 'pbm) t))
                               (round (* (buffer-local-value 'vellumory--scale buffer) w))))
                    (should (or (> left 0) (> top 0)))
                    (vellumory-gui--with-frame-pixels pixel
                      (should (equal (funcall pixel (+ x0 6) (+ y0 h -6)) '(255 255 255)))
                      (when (> left 1) (should (equal (funcall pixel (+ (nth 0 edges) (/ left 2)) (+ y0 (/ h 2)))
                                                      '(0 0 0))))
                      (when (> top 1) (should (equal (funcall pixel (+ x0 (/ w 2)) (+ (nth 1 edges) (/ top 2)))
                                                     '(0 0 0)))))
                    ;; The first page's link (points 40–300 × 32–77) goes to page 3.
                    (let* ((k (/ w 612.0))
                           (posn (posn-at-x-y (+ left (round (* k 170))) (+ top (round (* k 55))) window)))
                      (vellumory-present-click (list 'mouse-1 posn)))
                    (should (shown 2))
                    (vellumory-present-previous)
                    (should (shown 1))
                    ;; A page as wide as the window keeps its last column (no
                    ;; truncation glyph): the landscape page's navy band reaches the edge.
                    (pcase-let ((`(,w1 ,h1 ,l1 ,t1) (vellumory-present--fit 1)))
                      (vellumory-gui--with-frame-pixels pixel
                        (should-not (equal (funcall pixel (+ (nth 0 edges) l1 w1 -2) (+ (nth 1 edges) t1 (/ h1 2)))
                                           '(0 0 0)))))
                    (vellumory-present-black)
                    (redisplay t)
                    (should-not (text-property-any (point-min) (point-max) 'vellumory-present-page 1))
                    (vellumory-gui--with-frame-pixels pixel
                      (should (equal (funcall pixel (+ x0 6) (+ y0 h -6)) '(0 0 0))))
                    (vellumory-present-black)
                    (should (shown 1))
                    (vellumory-present-quit)))
                (should-not (frame-live-p frame))
                (should-not (buffer-live-p presentation))
                (with-current-buffer buffer (should (= vellumory--current-page 1))))))
        (when (buffer-live-p presentation) (kill-buffer presentation))
        (when (and (buffer-live-p buffer) (not (eq buffer existing))) (vellumory-gui--kill buffer))
        (when (frame-live-p reader-frame) (select-frame reader-frame))))))

(ert-deftest vellumory-gui-sidebar-switches-previews-and-contents ()
  "s, or a click on the other tab, shows the contents in place of the previews
and back; the selected window stays selected; closing closes; a document
without contents keeps its previews and says why."
  (let* ((file (expand-file-name "target/review/reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (plain (expand-file-name "target/review/long-reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (existing (find-buffer-visiting file)) buffer other)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (delete-other-windows)
            (should (vellumory-test-gui--wait (lambda () (and vellumory--pages (vellumory--has-contents-p)))))
            (let ((reader (selected-window)))
              (vellumory--show-thumbnails)
              (vellumory-sidebar-switch)
              (should (eq (selected-window) reader))
              (should (vellumory--shown vellumory--outline-buffer))
              (should-not (vellumory--shown vellumory--thumbnail-buffer))
              (let ((sidebar (vellumory--shown vellumory--outline-buffer)))
                (should (eq (window-parameter sidebar 'window-side) 'left))
                (should (string-match-p "Pages.*Contents"
                                        (with-selected-window sidebar (format-mode-line header-line-format))))
                ;; A click in the sidebar switches it and selects it.
                (vellumory-sidebar-switch (list 'mouse-1 (posn-at-point (window-start sidebar) sidebar)))
                (should (eq (window-buffer (selected-window)) (buffer-local-value 'vellumory--thumbnail-buffer buffer)))
                (should (eq (selected-window) sidebar)))
              (select-window reader)
              (should-not (vellumory--shown vellumory--outline-buffer))
              ;; Closing the sidebar closes it rather than showing the other view.
              (vellumory-thumbnails)
              (should-not (vellumory--shown vellumory--thumbnail-buffer))
              (should-not (vellumory--shown vellumory--outline-buffer))
              (should-not (window-parameter (selected-window) 'window-side)))
            (setq other (vellumory-open plain))
            (delete-other-windows)
            (should (vellumory-test-gui--wait
                     (lambda () (and vellumory--pages (not (eq vellumory--navigation-state 'loading))))))
            (should-not (vellumory--has-contents-p))
            (vellumory--show-thumbnails)
            (should-error (vellumory-sidebar-switch) :type 'user-error)
            (should (vellumory--shown vellumory--thumbnail-buffer)))
        (dolist (b (list buffer other))
          (when (and (buffer-live-p b) (not (eq b existing)))
            (let ((sidebars (with-current-buffer b (list vellumory--thumbnail-buffer vellumory--outline-buffer))))
              (vellumory-gui--kill b)
              (dolist (s sidebars) (when (buffer-live-p s) (kill-buffer s))))))
        (when (buffer-live-p existing)
          (with-current-buffer existing
            (dolist (s (list vellumory--thumbnail-buffer vellumory--outline-buffer))
              (when-let* ((w (vellumory--shown s))) (delete-window w)))))))))

(ert-deftest vellumory-gui-presentation-draws-page-transitions ()
  "A page's /Trans Wipe draws the new page sweeping in from the left over the
old one for its /D seconds, then the page alone."
  (skip-unless (vellumory-gui--can-capture-p))
  (let* ((file (expand-file-name "target/review/slides.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (reader-frame (selected-frame)) buffer presentation)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
            (vellumory-first-page)
            (setq presentation (vellumory-present))
            (with-current-buffer presentation
              (cl-flet ((shown (page) (vellumory-test-gui--wait
                                       (lambda () (redisplay t) (eql (car vellumory-present--shown) page)))))
                (should (shown 0))
                ;; The report of the next page, with its /Trans, is fetched ahead.
                (should (vellumory-test-gui--wait (lambda () (assq 1 vellumory-present--info))))
                (let ((started (float-time)) finished)
                  (vellumory-present-next)
                  (should (vellumory-test-gui--wait (lambda () (timerp vellumory-present--animation))))
                  ;; Mid-way: blue (new) on the left, red (old) on the right.
                  (while (< (- (float-time) started) 0.3) (accept-process-output nil 0.01) (redisplay t))
                  (pcase-let* ((`(,w ,h ,left ,top) (vellumory-present--fit 1))
                               (edges (window-body-pixel-edges (get-buffer-window presentation t)))
                               (y (+ (nth 1 edges) top (/ h 2))))
                    (redisplay t)
                    (vellumory-gui--with-frame-pixels pixel
                      (should (equal (funcall pixel (+ (nth 0 edges) left (/ w 20)) y) '(0 0 255)))
                      (should (equal (funcall pixel (+ (nth 0 edges) left (- w (/ w 20))) y) '(255 0 0))))
                    (should (shown 1))
                    (setq finished (float-time))
                    (should (>= (- finished started) 0.5))
                    (should-not vellumory-present--animation)
                    ;; Then the page alone, whole and clickable.
                    (let ((image (get-text-property
                                  (text-property-any (point-min) (point-max) 'vellumory-present-page 1) 'display)))
                      (should (eq (car image) 'image)))
                    (vellumory-gui--with-frame-pixels pixel
                      (should (equal (funcall pixel (+ (nth 0 edges) left (- w (/ w 20))) y) '(0 0 255))))))
                ;; Going back: page 1 has no /Trans, so it cuts.
                (vellumory-present-previous)
                (should (shown 0))
                (should-not vellumory-present--animation)
                (vellumory-present-quit))))
        (when (buffer-live-p presentation) (kill-buffer presentation))
        (when (buffer-live-p buffer) (vellumory-gui--kill buffer))
        (when (frame-live-p reader-frame) (select-frame reader-frame))))))

(ert-deftest vellumory-gui-presentation-blends-pages ()
  "Fade, Dissolve, Glitter and Fly draw frames the worker composes from both
pages: mid-way a fade shows a mix of the colours, a dissolve some pixels of each
page and nothing else, a glitter or a fly from the left the new page on the left; each frame
is dropped from the image cache once replaced, and the page ends alone."
  (skip-unless (vellumory-gui--can-capture-p))
  (let* ((file (expand-file-name "target/review/slides.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (reader-frame (selected-frame)) buffer presentation)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
            (vellumory-goto-page 2)
            (setq presentation (vellumory-present))
            (with-current-buffer presentation
              (cl-labels ((shown (page) (vellumory-test-gui--wait
                                         (lambda () (redisplay t) (eql (car vellumory-present--shown) page))))
                          (midway (page check)
                            ;; Ahead: the next page's report, with its /Trans.
                            (should (vellumory-test-gui--wait (lambda () (assq page vellumory-present--info))))
                            (let ((started (float-time)))
                              (vellumory-present-next)
                              (should (vellumory-test-gui--wait (lambda () vellumory-present--frame)))
                              ;; One canvas refreshed in place where Emacs has them.
                              (should (eq (image-property vellumory-present--frame :type)
                                          (if (image-type-available-p 'canvas) 'canvas 'pbm)))
                              (while (< (- (float-time) started) 0.4) (accept-process-output nil 0.01) (redisplay t))
                              (pcase-let* ((`(,w ,h ,left ,top) (vellumory-present--fit page))
                                           (edges (window-body-pixel-edges (get-buffer-window presentation t))))
                                (redisplay t)
                                (vellumory-gui--with-frame-pixels pixel
                                  (funcall check (lambda (fx fy)
                                                   (funcall pixel (+ (nth 0 edges) left (round (* fx (1- w))))
                                                            (+ (nth 1 edges) top (round (* fy (1- h)))))))))
                              (should (shown page))
                              (should (>= (- (float-time) started) 0.75))
                              (should-not vellumory-present--animation)
                              (should-not vellumory-present--frame)
                              (message "%s: %d frames in %.2f s (%s)"
                                       (alist-get 'style (alist-get 'transition (alist-get page vellumory-present--info)))
                                       vellumory-present--frames (- (float-time) started)
                                       (if (image-type-available-p 'canvas) "canvas" "images"))
                              ;; At least Okular's 20 frames a second.
                              (should (>= vellumory-present--frames 14)))))
                (should (shown 1))
                ;; Blue to green: a mix of both, neither pure.
                (midway 2 (lambda (at)
                            (pcase-let ((`(,r ,g ,b) (funcall at 0.5 0.5)))
                              (should (= r 0))
                              (should (and (< 25 g 230) (< 25 b 230) (< (abs (- (+ g b) 255)) 4))))))
                ;; Green to red: pixels of each page, none of anything else.
                (midway 3 (lambda (at)
                            (let ((colours nil))
                              (dotimes (i 20)
                                (dotimes (j 15)
                                  (push (funcall at (/ (+ i 0.5) 20) (/ (+ j 0.5) 15)) colours)))
                              (let ((red (cl-count '(255 0 0) colours :test #'vellumory-gui--near))
                                    (green (cl-count '(0 255 0) colours :test #'vellumory-gui--near)))
                                (unless (= (+ red green) (length colours))
                                  (message "blend colours seen: %S" (seq-take (seq-uniq colours) 12)))
                                (should (= (+ red green) (length colours)))
                                (should (and (> red 30) (> green 30)))))))
                ;; Red to yellow from the left: yellow on the left, red on the right.
                (midway 4 (lambda (at)
                            (should (equal (funcall at 0.03 0.5) '(255 255 0)))
                            (should (equal (funcall at 0.97 0.5) '(255 0 0)))))
                ;; Yellow to white flying in from the left, opaque: white on the left only.
                (midway 5 (lambda (at)
                            (should (equal (funcall at 0.03 0.5) '(255 255 255)))
                            (should (equal (funcall at 0.97 0.5) '(255 255 0)))))
                (vellumory-present-quit))))
        (when (buffer-live-p presentation) (kill-buffer presentation))
        (when (buffer-live-p buffer) (vellumory-gui--kill buffer))
        (when (frame-live-p reader-frame) (select-frame reader-frame))))))

(ert-deftest vellumory-gui-keyboard-caret-and-selection ()
  "The caret mode puts a caret into the text in view; it is drawn on the page, moves by
lines and words through MuPDF's characters, and v $ y copies a whole line."
  (skip-unless (vellumory-gui--can-capture-p))
  (let* ((file (expand-file-name "target/review/reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (existing (find-buffer-visiting file)) buffer)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (delete-other-windows)
            (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
            (vellumory-first-page)
            (vellumory-gui--settle)
            (vellumory-caret-mode 1)
            (should (equal vellumory-caret--pos '(0 0 0)))
            (should (equal (mapconcat (lambda (c) (aref c 0)) (vellumory-caret--chars vellumory-caret--pos) "")
                           "Vellumory / 1"))
            ;; Down to "Line 1: …", on white: the caret bar is drawn there.
            (vellumory-caret-next-line)
            (vellumory-caret-next-line)
            (should (string-prefix-p "Line 1" (mapconcat (lambda (c) (aref c 0))
                                                         (vellumory-caret--chars vellumory-caret--pos) "")))
            (vellumory-gui--settle)
            (redisplay t)
            (let* ((pos vellumory-caret--pos) (box (alist-get 'box (vellumory-caret--line pos)))
                   (k (vellumory--points-per-pixel 0)) (edges (window-body-pixel-edges))
                   (x (+ (nth 0 edges) vellumory--margin (round (/ (vellumory-caret--x pos) k))))
                   (y (+ (nth 1 edges) (- (vellumory--offset-of 0 (/ (+ (aref box 1) (aref box 3)) 2))
                                          (vellumory--absolute-position)))))
              (vellumory-gui--with-frame-pixels pixel
                ;; The caret has the cursor's colour (black here, the theme's in Doom):
                ;; it stands out from the paper, whatever the colour.
                (unless (< (min (apply #'+ (funcall pixel (1- x) y)) (apply #'+ (funcall pixel x y))) 600)
                  ;; Where the dark caret is instead, if anywhere near.
                  (message "caret expected at %d,%d; darkest within 40 px: %S" x y
                           (car (sort (cl-loop for dx from -40 to 40 nconc
                                               (cl-loop for dy from -40 to 40
                                                        collect (list (apply #'+ (funcall pixel (+ x dx) (+ y dy))) dx dy)))
                                      (lambda (a b) (< (car a) (car b)))))))
                (should (< (min (apply #'+ (funcall pixel (1- x) y)) (apply #'+ (funcall pixel x y))) 600))
                (should (equal (funcall pixel (- x 6) y) '(255 255 255)))))
            ;; Words: "Line 1: text" — w stops at 1, at the colon, at text.
            (vellumory-caret-forward-word)
            (should (equal (vellumory-caret--char vellumory-caret--pos) "1"))
            (vellumory-caret-forward-word)
            (should (equal (vellumory-caret--char vellumory-caret--pos) ":"))
            (vellumory-caret-forward-word)
            (should (equal (vellumory-caret--char vellumory-caret--pos) "t"))
            ;; v from the line start to its end, then y.
            (vellumory-caret-line-start)
            (vellumory-caret-toggle-selection)
            (vellumory-caret-line-end)
            (vellumory-caret-copy)
            (should (equal (car kill-ring) "Line 1: text and shapes cross raster strip boundaries."))
            (should-not vellumory-caret-mode)
            (should-not vellumory--selection))
        (when (and (buffer-live-p buffer) (not (eq buffer existing))) (vellumory-gui--kill buffer))
        (when (buffer-live-p buffer) (with-current-buffer buffer (when vellumory-caret-mode (vellumory-caret-mode -1))))))))

(ert-deftest vellumory-gui-follow-columns ()
  "`vellumory-follow-split' shows the document in two columns that continue
each other: the second starts where the first ends, paging moves both by both
heights, scrolling either re-chains the other, each header names its own page,
and M-x follow-mode turns it off."
  (skip-unless (display-graphic-p))
  (let* ((file (expand-file-name "target/review/long-reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (existing (find-buffer-visiting file)) buffer)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (delete-other-windows)
            (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
            (vellumory-goto-page 2)
            (vellumory-follow-split)
            (redisplay t)
            (cl-flet ((chained ()
                        (pcase-let ((`(,a ,b) (vellumory-follow--windows)))
                          (= (vellumory--absolute-position b)
                             (+ (vellumory--absolute-position a) (window-body-height a t)))))
                      ;; What the header line or doom-modeline shows for each window.
                      (header-page (window)
                        (with-selected-window window
                          (let ((text (vellumory--page-string)))
                            (and (string-match "\\`\\([0-9]+\\)/" text)
                                 (string-to-number (match-string 1 text)))))))
              (should vellumory-follow-mode)
              (should (= (length (vellumory-follow--windows)) 2))
              (should (chained))
              (let ((before (vellumory--absolute-position (car (vellumory-follow--windows))))
                    (vellumory-smooth-scroll nil))
                (vellumory-next-screen)
                (should (chained))
                (should (= (- (vellumory--absolute-position (car (vellumory-follow--windows))) before)
                           (round (* .88 (vellumory-follow--screen-height))))))
              (with-selected-window (cadr (vellumory-follow--windows)) (vellumory-scroll 100))
              (should (chained))
              (redisplay t)
              (dolist (window (vellumory-follow--windows))
                (should (= (header-page window)
                           (1+ (alist-get 'page (vellumory--reading-position window))))))
              (should (/= (header-page (car (vellumory-follow--windows)))
                          (header-page (cadr (vellumory-follow--windows)))))
              (call-interactively #'follow-mode)
              (should-not vellumory-follow-mode)
              (should-not (bound-and-true-p follow-mode))))
        (when (and (buffer-live-p buffer) (not existing)) (vellumory-gui--kill buffer))))))

(ert-deftest vellumory-gui-saveplace-leaves-documents-alone ()
  "With `save-place-mode' a document reopens at its exact position, kept by
Vellumory, and saveplace's list gets no entry for it; an old entry is dropped
when the document is next closed."
  (skip-unless (display-graphic-p))
  (require 'saveplace)
  (let* ((file (expand-file-name "target/review/long-reader.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (existing (find-buffer-visiting file))
         (save-place-file (make-temp-file "vellumory-places-"))
         (save-place-alist (list (cons file 219)))
         (save-place-loaded t)
         (was save-place-mode) position)
    (when existing (vellumory-gui--kill existing))
    (save-place-mode 1)
    (save-window-excursion
      (unwind-protect
          (progn
            (let ((buffer (find-file file)))
              (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
              (vellumory-goto-page 37)
              (vellumory-scroll 333)
              (redisplay t)
              (setq position (vellumory--reading-position))
              (vellumory-gui--kill buffer))
            (should-not (assoc file save-place-alist))
            (let ((buffer (find-file file)))
              (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
              (should (vellumory-test-gui--wait (lambda () (redisplay t) (equal (vellumory--reading-position) position))))
              (vellumory-gui--kill buffer))
            (should-not (assoc file save-place-alist)))
        (save-place-mode (if was 1 -1))
        (delete-file save-place-file)))))

(ert-deftest vellumory-gui-fit-content-width ()
  "Fitting to the content zooms until the page's ink, without its white
margins, fills the window less the padding on each side, and keeps it there
when the window changes size; another zoom ends it."
  (skip-unless (vellumory-gui--can-capture-p))
  (let* ((file (expand-file-name "target/review/margins.pdf" (getenv "VELLUMORY_REVIEW_ROOT")))
         (existing (find-buffer-visiting file)) buffer)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (vellumory-open file))
            (delete-other-windows)
            (should (vellumory-test-gui--wait (lambda () vellumory--pages)))
            (vellumory-goto-page 1)
            ;; Doom's horizontal scrolling settings, which redisplay once applied.
            (setq-local hscroll-margin 2 hscroll-step 1)
            (vellumory-fit-content-width)
            (should (vellumory-test-gui--wait (lambda () vellumory--content-fit)))
            (vellumory-scroll 96)
            (cl-flet ((check ()
                        (vellumory-gui--settle)
                        (redisplay t)
                        (let* ((edges (window-body-pixel-edges)) (y (/ (+ (nth 1 edges) (nth 3 edges)) 2))
                               (char (frame-char-width)) (pad vellumory-content-padding))
                          (vellumory-gui--with-frame-pixels pixel
                            ;; Paper at both edges, ink just inside the padding (to a column).
                            (should (equal (funcall pixel (+ (nth 0 edges) 2) y) '(255 255 255)))
                            (should (equal (funcall pixel (- (nth 2 edges) 3) y) '(255 255 255)))
                            (should (equal (funcall pixel (+ (nth 0 edges) pad char) y) '(0 0 0)))
                            (should (equal (funcall pixel (- (nth 2 edges) pad char) y) '(0 0 0)))))))
              ;; 412 of 612 points are content: about 1.49 times the page width.
              (should (< 1.4 vellumory--zoom 1.55))
              (check)
              ;; Narrower: still fitted.
              (split-window-right (/ (window-total-width) 3))
              (other-window 1)
              (should (eq (current-buffer) buffer))
              (delete-other-windows)
              (check)
              (vellumory-fit-width)
              (should-not vellumory--content-fit)
              (should (= (window-hscroll) 0))))
        (when (and (buffer-live-p buffer) (not existing)) (vellumory-gui--kill buffer))))))

(ert-deftest vellumory-gui-sidebar-goes-with-its-document ()
  "A document's sidebar closes when the document leaves the frame (Dired in
its window, as after find-file on a directory) and returns with it."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (let ((reader (current-buffer)) directory)
      (unwind-protect
          (progn
            (vellumory--show-thumbnails)
            (vellumory-gui--settle)
            (redisplay t)
            (should (vellumory--shown vellumory--thumbnail-buffer))
            (setq directory (dired-noselect (file-name-directory buffer-file-name)))
            (switch-to-buffer directory)
            (redisplay t)
            (should-not (with-current-buffer reader (vellumory--shown vellumory--thumbnail-buffer)))
            (switch-to-buffer reader)
            (redisplay t)
            (vellumory-gui--settle)
            (redisplay t)
            (should (vellumory--shown vellumory--thumbnail-buffer)))
        (switch-to-buffer reader)
        (when-let* ((sidebar (vellumory--shown vellumory--thumbnail-buffer))) (delete-window sidebar))
        (when (buffer-live-p directory) (kill-buffer directory))))))

(ert-deftest vellumory-gui-recenter-alternates-with-the-zoom ()
  "0 fits a zoomed view to the window and, pressed again, returns to the zoom
and horizontal scroll, on the same page."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (unwind-protect
        (progn
          (vellumory-fit-width)
          (vellumory-first-page)
          (vellumory--set-zoom 2.0)
          (vellumory-gui--settle)
          (set-window-hscroll nil 7)
          (let ((page (alist-get 'page (vellumory--reading-position))))
            (vellumory-recenter)
            (vellumory-gui--settle)
            (should (= vellumory--zoom 1.0))
            (should (= (window-hscroll) 0))
            (should (equal (alist-get 'page (vellumory--reading-position)) page))
            (vellumory-recenter)
            (vellumory-gui--settle)
            (should (= vellumory--zoom 2.0))
            (should (= (window-hscroll) 7))
            (should (equal (alist-get 'page (vellumory--reading-position)) page))))
      (setq vellumory--recenter-saved nil)
      (vellumory-fit-width)
      (vellumory-first-page)
      (vellumory-gui--settle))))

(defun vellumory-gui--reflowed (reflow)
  "Pump worker output until REFLOW shows its target, or ten seconds pass."
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (float-time) deadline)
                (with-current-buffer reflow
                  (or vellumory-reflow--target vellumory-reflow--pending (null vellumory-reflow--first))))
      (accept-process-output nil .03)
      (redisplay t))
    (with-current-buffer reflow (should-not vellumory-reflow--target))))

(ert-deftest vellumory-gui-reflow-reads-and-returns ()
  "Reading mode shows the page read as text at a measure, centred in the
window, goes on to the next page, and returns the pages at the page reached."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (let ((source (current-buffer)))
      (unwind-protect
          (progn
            (vellumory-goto-page 2)
            (vellumory-gui--settle)
            (vellumory-reflow)
            (let ((reflow (window-buffer)))
              (should (eq (buffer-local-value 'major-mode reflow) 'vellumory-reflow-mode))
              (vellumory-gui--reflowed reflow)
              (with-current-buffer reflow
                (should (equal (car (vellumory-reflow--here (selected-window))) 1))
                (should (string-search "Vellumory / 2" (buffer-string)))
                (should (<= vellumory-reflow--width (* vellumory-reflow-measure (frame-char-width))))
                (should (> (car (window-margins)) 0))
                (vellumory-reflow-next-page 1)
                (vellumory-gui--reflowed reflow)
                (should (equal (car (vellumory-reflow--here (selected-window))) 2))
                (vellumory-reflow-quit))
              (should (eq (window-buffer) source))
              (should (equal (window-margins) '(nil)))
              (vellumory-gui--settle)
              (should (equal (alist-get 'page (vellumory--reading-position)) 2))))
        (when (derived-mode-p 'vellumory-reflow-mode) (vellumory-reflow-quit))
        (with-current-buffer source
          (vellumory-first-page)
          (vellumory-gui--settle))))))

(ert-deftest vellumory-gui-reflow-in-narrow-windows ()
  "With `vellumory-reflow-narrow-width', a window too narrow for the pages
shows them reflowed, and the pages again once it is wide.  The threshold is
half the window's width, so that the 30 columns split off are below it and
the rest above, however wide the frame is."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (skip-unless (> (window-total-width) 70))
    (let ((source (current-buffer)) (vellumory-reflow-narrow-width (/ (window-total-width) 2)) (narrow nil))
      (unwind-protect
          (progn
            (setq narrow (split-window-right (- (window-total-width) 30)))
            (with-selected-window narrow (switch-to-buffer source))
            (let ((deadline (+ (float-time) 5)))
              (while (and (< (float-time) deadline)
                          (not (eq (buffer-local-value 'major-mode (window-buffer narrow)) 'vellumory-reflow-mode)))
                (accept-process-output nil .03) (redisplay t)))
            (should (eq (buffer-local-value 'major-mode (window-buffer narrow)) 'vellumory-reflow-mode))
            (should (eq (window-buffer (selected-window)) source))
            (delete-other-windows narrow)
            (let ((deadline (+ (float-time) 5)))
              (while (and (< (float-time) deadline) (not (eq (window-buffer narrow) source)))
                (accept-process-output nil .03) (redisplay t)))
            (should (eq (window-buffer narrow) source)))
        (when (window-live-p narrow)
          (with-selected-window narrow
            (unless (eq (window-buffer) source) (switch-to-buffer source)))
          (unless (one-window-p) (delete-window narrow)))
        (with-current-buffer source (vellumory-gui--settle))))))

(ert-deftest vellumory-gui-link-preview-shows-the-target ()
  "Resting on a link within the document shows its target in a child frame
by it: rendered from the target page, with its header; hiding closes it."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    (let ((window (selected-window)) link)
      (unwind-protect
          (progn
            (vellumory-first-page)
            (vellumory-gui--settle)
            (dotimes (page (length vellumory--pages))
              (vellumory--ensure-links page))
            (let ((deadline (+ (float-time) 5)))
              (while (and (< (float-time) deadline)
                          (not (setq link (cl-loop for page below (length vellumory--pages)
                                                   for links = (gethash page vellumory--links)
                                                   thereis (and (vectorp links)
                                                                (seq-find (lambda (l) (natnump (alist-get 'page l)))
                                                                          links))))))
                (accept-process-output vellumory--process .03)))
            (skip-unless link)
            (vellumory--link-preview-hover window link)
            ;; The mouse is not on the link in a test: no watching.
            (cancel-timer (plist-get vellumory--link-preview :watch))
            (cancel-timer (plist-get vellumory--link-preview :timer))
            (vellumory--link-preview-show window link)
            (let ((deadline (+ (float-time) 5)))
              (while (and (< (float-time) deadline)
                          (not (and (frame-live-p vellumory--link-preview-frame)
                                    (frame-visible-p vellumory--link-preview-frame))))
                (accept-process-output vellumory--process .03)
                (redisplay t)))
            (should (frame-visible-p vellumory--link-preview-frame))
            (should (eq (frame-parent vellumory--link-preview-frame) (window-frame window)))
            (with-current-buffer (window-buffer (frame-root-window vellumory--link-preview-frame))
              (should (eq (car-safe (get-text-property (point-min) 'display)) 'image))
              (should (string-search "Page" (format-mode-line header-line-format))))
            (vellumory-link-preview-hide)
            (should-not (frame-visible-p vellumory--link-preview-frame))
            (should-not vellumory--link-preview))
        (vellumory-link-preview-hide)))))

(ert-deftest vellumory-gui-released-strips-leave-the-image-cache ()
  "Strips scrolled away are freed from Emacs's image cache once reading
rests, rather than kept there for `image-cache-eviction-delay'."
  (skip-unless (and (display-graphic-p) (fboundp 'image-cache-size)))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    ;; An inactive timer in its place: the flush happens below, not in between.
    (let ((vellumory--flush-timer (timer-create)) (vellumory-smooth-scroll nil))
     (unwind-protect
        (progn
          (vellumory-first-page)
          ;; Tall pages: scrolling releases the strips behind.
          (vellumory--set-zoom 3.0)
          (vellumory-gui--settle)
          (dotimes (_ 12) (vellumory-next-screen) (vellumory-gui--settle))
          (should vellumory--released-files)
          (let ((before (image-cache-size)))
            (vellumory--free-released)
            (should-not vellumory--released-files)
            (should (< (image-cache-size) before))))
      (vellumory-fit-width)
      (vellumory-first-page)
      (vellumory-gui--settle)))))

(ert-deftest vellumory-gui-wheel-notches-scroll-alike ()
  "A notch scrolls the same distance however fast notches come: Emacs
reports quick ones as double and triple events, which once multiplied it."
  (skip-unless (display-graphic-p))
  (with-selected-window (get-buffer-window "reader.pdf" t)
    ;; The pointer over the page, as a real notch's position: inside the
    ;; window's body, wherever point is.
    (let ((posn (posn-at-x-y 10 10 (selected-window))) distances)
      (should (eq (posn-window posn) (selected-window)))
      (cl-letf (((symbol-function 'vellumory-scroll)
                 (lambda (pixels &rest _)
                   (push (cons pixels (round (expt (window-body-height nil t) (/ 2.0 3)))) distances))))
        (dolist (kind '(wheel-down double-wheel-down triple-wheel-down))
          (vellumory-wheel (list kind posn (pcase kind ('wheel-down 1) ('double-wheel-down 2) (_ 3))))))
      (should (= (length distances) 3))
      ;; Each notch: the window's height to the power 2/3, as GTK.
      (dolist (distance distances) (should (= (car distance) (cdr distance)))))))

(defun vellumory-gui--wait-for (predicate &optional seconds)
  "Pump process output and redisplay until PREDICATE holds or SECONDS pass.
Return PREDICATE's last value."
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (redisplay t))
    (funcall predicate)))

(ert-deftest vellumory-gui-history-tree-restores-every-state ()
  "The time machine by keys: on an edited copy with a branch in its history,
\\`c' on each state makes the file that state again, byte for byte (its
SHA-256 is the state's content address); the tree marks it current and the
reader shows it."
  (let* ((dir (make-temp-file "vellumory-history-" t))
         (file (expand-file-name "paper.pdf" dir))
         (source (with-current-buffer "reader.pdf" vellumory--source-file))
         (reader nil) (history nil)
         (file-hash (lambda () (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally file)
                                 (secure-hash 'sha256 (current-buffer))))))
    (unwind-protect
        (progn
          (copy-file source file)
          (set-file-modes file #o644)
          (setq reader (find-file-noselect file))
          (with-current-buffer reader
            (should (vellumory-gui--wait-for (lambda () vellumory--hash)))
            ;; Rotate, compress, undo the compression, rotate back: a branch.
            (dolist (edit (list #'vellumory-rotate-pages #'vellumory-compress #'vellumory-undo
                                #'vellumory-rotate-pages-counterclockwise))
              (let ((before vellumory--hash))
                (call-interactively edit)
                (should (vellumory-gui--wait-for
                         (lambda () (and vellumory--hash (not (equal before vellumory--hash))))
                         60)))))
          (save-window-excursion
            (with-current-buffer reader (vellumory-history))
            (setq history (get-buffer (format "*History: %s*" (buffer-name reader))))
            (should (vellumory-gui--wait-for
                     (lambda () (buffer-local-value 'vellumory--history history))))
            (let ((nodes (alist-get 'nodes (buffer-local-value 'vellumory--history history)))
                  (window (get-buffer-window history t)))
              (should (= (length nodes) 4))
              ;; Every state, the oldest first and the branch last, by its key.
              (dotimes (state (length nodes))
                (let ((hash (alist-get 'hash (aref nodes state))))
                  (with-selected-window window
                    (goto-char (text-property-any (point-min) (point-max) 'vellumory-state state))
                    (execute-kbd-macro (kbd "c")))
                  (should (vellumory-gui--wait-for
                           (lambda () (and (equal (funcall file-hash) hash)
                                           (eql (alist-get 'head (buffer-local-value 'vellumory--history history))
                                                state)
                                           (equal (buffer-local-value 'vellumory--hash reader) hash)))
                           60))
                  (should (equal (funcall file-hash) hash))
                  (should (eql (alist-get 'head (buffer-local-value 'vellumory--history history)) state))
                  (should (equal (buffer-local-value 'vellumory--hash reader) hash)))))))
      (when (buffer-live-p history) (kill-buffer history))
      (when (buffer-live-p reader)
        (with-current-buffer reader (set-buffer-modified-p nil))
        (vellumory-gui--kill reader))
      (delete-directory dir t))))
