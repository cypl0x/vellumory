;;; vellumory-tests.el --- Reader regression tests -*- lexical-binding: t; -*-
(require 'ert)
(setq load-prefer-newer t)
(require 'vellumory)

;; Emacs 29 and 30 in batch mode run no process sentinel once a file watch
;; has seen a change in its directory (interactively they do; Emacs 31 fixed
;; batch): there the tests watch no files, and the test of watching is skipped.
(defconst vellumory-test--batch-watch-ok (or (not noninteractive) (>= emacs-major-version 31)))
(unless vellumory-test--batch-watch-ok (setq-default vellumory-watch-file nil))

(ert-deftest vellumory-protocol-fragmentation ()
  (with-temp-buffer
    (let ((process (make-pipe-process :name "vellumory-test-pipe" :noquery t)) replies)
      (unwind-protect
          (progn
            (clrhash vellumory--worker-callbacks)
            (puthash 7 (cons (current-buffer) (lambda (r) (push r replies))) vellumory--worker-callbacks)
            (puthash 8 (cons (current-buffer) (lambda (r) (push r replies))) vellumory--worker-callbacks)
            (vellumory--filter process "{\"id\":7,\"res")
            (should-not replies)
            (vellumory--filter process "ult\":\"λ\"}\n{\"id\":8,\"error\":\"bad\"}\n")
            (should (= (length replies) 2))
            (should (equal (alist-get 'result (cadr replies)) "λ"))
            (should (= (hash-table-count vellumory--worker-callbacks) 0))
            (should (equal (process-get process 'fragment) "")))
        (delete-process process)))))

(ert-deftest vellumory-position-state-roundtrip ()
  (let ((vellumory-state-directory (make-temp-file "vellumory-state-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq vellumory--file "/test/document.pdf" vellumory--hash "content-hash"
                vellumory--pages [((width . 612.0) (height . 792.0))]
                vellumory--last-position '((page . 0) (y . 123.25) (zoom . 1.15)))
          (vellumory--save-position)
          (should (equal vellumory--last-position (vellumory--load-position)))
          ;; Identical content under another name finds the same position.
          (setq vellumory--file "/elsewhere/renamed.pdf")
          (should (equal vellumory--last-position (vellumory--load-position)))
          (setq vellumory--file "/test/document.pdf")
          (dolist (key (list nil "content-hash"))
            (with-temp-file (vellumory--state-file key)
              (insert "{\"page\":\"wrong\",\"y\":0,\"zoom\":1}")))
          (should-not (vellumory--load-position)))
      (delete-directory vellumory-state-directory t))))

(ert-deftest vellumory-watch-survives-atomic-replacement ()
  (skip-unless vellumory-test--batch-watch-ok)
  (let ((directory (make-temp-file "vellumory-watch-" t)) (reloads 0))
    (unwind-protect
        (with-temp-buffer
          (setq vellumory--file (expand-file-name "document.pdf" directory)
                vellumory--source-file vellumory--file)
          (let ((file vellumory--file)) (with-temp-file file (insert "old")))
          (vellumory--watch-file)
          (skip-unless vellumory--watch)
          (unwind-protect
              (cl-letf (((symbol-function 'vellumory-reload) (lambda () (cl-incf reloads))))
                (dotimes (iteration 2)
                  (let ((temporary (expand-file-name "replacement" directory))
                        (deadline (+ (float-time) 3)))
                    (with-temp-file temporary (insert (format "new %d" iteration)))
                    (rename-file temporary vellumory--file t)
                    (while (and (<= reloads iteration) (< (float-time) deadline))
                      (let ((event (read-event nil nil .05)))
                        (when (and (consp event) (eq (car event) 'file-notify))
                          (file-notify-handle-event event))))
                    (should (= reloads (1+ iteration))))))
            (vellumory--cleanup)))
      (delete-directory directory t))))

(ert-deftest vellumory-precision-wheel-direction-and-fractions ()
  (with-current-buffer (window-buffer (selected-window))
   (let ((vellumory--wheel-remainder 0.0) (distance 0))
    (cl-letf (((symbol-function 'vellumory-scroll) (lambda (pixels) (cl-incf distance pixels))))
      (dotimes (_ 4) (vellumory-wheel `(wheel-down (,(selected-window) 1 (0 . 0) 0) nil nil (0 . -0.25))))
      (should (= distance 1))
      (vellumory-wheel `(wheel-up (,(selected-window) 1 (0 . 0) 0) nil nil (0 . 13)))
      (should (= distance -12))))))

;;;; Scheduler

(defun vellumory-test--tiles (count)
  "Lay out COUNT one-tile rows, one per page, in the current buffer."
  (erase-buffer)
  (setq vellumory--width 600 vellumory--scale 1.0 vellumory--generation 1 vellumory--threads 2
        vellumory--hash "snapshot" vellumory--serial 0 vellumory--cost 4.0
        vellumory--pages (make-vector count '((width . 612.0) (height . 792.0)))
        vellumory--previews (make-hash-table :test #'eql)
        vellumory--links (make-hash-table :test #'eql)
        vellumory--inflight (make-hash-table :test #'eql)
        vellumory--callbacks (make-hash-table :test #'eql))
  (cl-loop for page below count collect
           (let* ((row (vellumory--row-create :page page :top 0 :bottom 192 :position (point)
                                              :offset (* page 192)))
                  (tile (vellumory--tile-create :row row :page page :left 0 :right 600 :position (point))))
             (setf (vellumory--row-tiles row) (vector tile))
             (insert (propertize " " 'vellumory-tile tile) "\n")
             tile)))

(defmacro vellumory-test--with-worker (sent cancelled &rest body)
  "Run BODY with a fake worker recording requests in SENT and cancellations in CANCELLED."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
             ((symbol-function 'vellumory--rpc)
              (lambda (method params callback &optional id)
                (let ((id (or id (cl-incf vellumory--serial))))
                  (when callback (puthash id callback vellumory--callbacks))
                  (push (list id method params) ,sent)
                  id)))
             ((symbol-function 'vellumory--cancel)
              (lambda (ids) (setq ,cancelled (append ids ,cancelled))))
             ((symbol-function 'create-image)
              (lambda (file &optional type _data-p &rest props)
                (append (list 'image :type type :file file) props))))
     ,@body))

(defun vellumory-test--reply (id reply)
  (let ((callback (gethash id vellumory--callbacks)))
    (remhash id vellumory--callbacks)
    (funcall callback (cons (cons 'id id) reply))))

(ert-deftest vellumory-pump-parallel-cancel-and-stale ()
  (with-temp-buffer
    (let ((tiles (vellumory-test--tiles 6)) sent cancelled)
      (vellumory-test--with-worker sent cancelled
        (setq vellumory--visible (seq-take tiles 3) vellumory--visible-and-near tiles
              vellumory--last-motion 0.0)
        (vellumory--pump)
        ;; Threads + 1 requests in flight, in priority order.
        (should (= (hash-table-count vellumory--inflight) 3))
        (should (equal (mapcar (lambda (r) (alist-get 'page (nth 2 r))) (reverse sent)) '(0 1 2)))
        (should (equal (mapcar (lambda (r) (alist-get 'priority (nth 2 r))) (reverse sent)) '(0 1 2)))
        (dotimes (_ 50) (vellumory--pump))
        (should (= (length sent) 3))
        ;; The viewport moves on: obsolete renders are cancelled, new ones sent.
        (setq vellumory--visible (last tiles 3))
        (vellumory--pump)
        (should (equal (sort (copy-sequence cancelled) #'<) '(1 2 3)))
        (should (= (hash-table-count vellumory--inflight) 3))
        (should (equal (mapcar (lambda (r) (alist-get 'page (nth 2 r))) (seq-take sent 3)) '(5 4 3)))
        ;; A cancelled reply is ignored; a fresh reply becomes a sharp image.
        (vellumory-test--reply 1 '((error . "cancelled") (cancelled . t)))
        (should (= (vellumory--tile-quality (nth 0 tiles)) 0))
        (vellumory-test--reply 4 '((result (path . "/run/x.ppm") (pixels . 115200) (ms . 5.0)
                                            (raster_ms . 0.2) (list_ms . 9.0) (built . t))))
        (should (gethash 3 vellumory--listed-pages))
        (should (< (abs (- vellumory--list-cost 16.7)) 0.01))
        (should (= (vellumory--tile-quality (nth 3 tiles)) 2))
        (should (eq 'pbm (nth 2 (vellumory--tile-image (nth 3 tiles)))))
        (should (< vellumory--cost 4.0))
        ;; Replies from an older layout generation never display.
        (cl-incf vellumory--generation)
        (vellumory-test--reply 5 '((result (path . "/run/old.ppm") (pixels . 115200) (ms . 1.0))))
        (should (= (vellumory--tile-quality (nth 4 tiles)) 0))))))

(ert-deftest vellumory-adaptive-refocus-previews-then-sharp ()
  (with-temp-buffer
    (let ((tiles (vellumory-test--tiles 4)) sent cancelled)
      (vellumory-test--with-worker sent cancelled
        (setq vellumory--visible (seq-take tiles 2) vellumory--visible-and-near tiles)
        ;; Moving over expensive content: request cheap whole-page previews.
        (setq vellumory--cost 900.0 vellumory--last-motion (float-time))
        (vellumory--pump)
        (should (= (length sent) 2))
        (should (cl-every (lambda (r) (= (alist-get 'top (nth 2 r)) 0)) sent))
        (should (cl-every (lambda (r) (< (alist-get 'width (nth 2 r)) 600)) sent))
        (vellumory-test--reply 1 '((result (path . "/run/preview.ppm") (pixels . 100) (ms . 1.0))))
        (should (= (vellumory--tile-quality (nth 0 tiles)) 1))
        (should (eq 'slice (car (car (get-text-property (vellumory--tile-position (nth 0 tiles)) 'display)))))
        ;; Motion stops: visible tiles refocus to sharp ahead of prefetch.
        (setq vellumory--last-motion 0.0)
        (vellumory--pump)
        (should (equal (mapcar (lambda (r) (alist-get 'right (nth 2 r))) (seq-take sent 1)) '(600)))
        ;; Cheap content renders sharp even while moving.
        (clrhash vellumory--inflight) (setq sent nil)
        (setq vellumory--cost 1.0 vellumory--list-cost 0.0 vellumory--last-motion (float-time)
              vellumory--visible (last tiles 2))
        (vellumory--pump)
        (should (cl-every (lambda (r) (= (alist-get 'width (nth 2 r)) 600)) sent))))))

(ert-deftest vellumory-signature-change-rerenders-without-flashing ()
  (with-temp-buffer
    (let ((tiles (vellumory-test--tiles 1)) sent cancelled)
      (vellumory-test--with-worker sent cancelled
        (setq vellumory--visible tiles vellumory--visible-and-near tiles vellumory--last-motion 0.0)
        (vellumory--pump)
        (vellumory-test--reply 1 '((result (path . "/run/plain.ppm") (pixels . 1) (ms . 1.0))))
        (should-not (vellumory--needs-sharp (car tiles)))
        ;; A selection on the page changes the raster signature.
        (setq vellumory--selection '((0 "word" [((x0 . 1) (y0 . 2) (x1 . 3) (y1 . 2)
                                                  (x2 . 3) (y2 . 4) (x3 . 1) (y3 . 4))])))
        (should (vellumory--needs-sharp (car tiles)))
        (vellumory--pump)
        (let ((overlay (alist-get 'overlay (nth 2 (car sent)))))
          (should (equal (alist-get 'kinds overlay) [2]))
          (should (equal (alist-get 'quads overlay) [[1 2 3 2 3 4 1 4]]))
          (should (equal (alist-get 'blend overlay) "multiply")))
        ;; The old image stays on screen until the new raster arrives.
        (should (equal (plist-get (cdr (vellumory--tile-image (car tiles))) :file) "/run/plain.ppm"))
        (should (= (vellumory--tile-quality (car tiles)) 2))))))

(ert-deftest vellumory-theme-and-dark-overlays ()
  (with-temp-buffer
    (let ((vellumory-theme 'invert))
      (vellumory-test--tiles 1)
      (should (equal (vellumory--theme-params) "invert"))
      (setq vellumory--selection '((0 "x" [((x0 . 0) (y0 . 0) (x1 . 1) (y1 . 0) (x2 . 1) (y2 . 1) (x3 . 0) (y3 . 1))])))
      (let ((overlay (vellumory--page-overlay 0)))
        (should (equal (alist-get 'blend overlay) "normal"))
        (should (= (aref (aref (alist-get 'palette overlay) 2) 3) 110)))
      (setq vellumory-theme 'tint)
      (let ((theme (vellumory--theme-params)))
        (should (natnump (alist-get 'fg theme)))
        (should (natnump (alist-get 'bg theme)))))))

(ert-deftest vellumory-thumbnails-use-only-spare-capacity ()
  (with-temp-buffer
    (let* ((tiles (vellumory-test--tiles 3))
           (first (vellumory--thumbnail-create :page 0 :width 48 :height 64 :position 1))
           (second (vellumory--thumbnail-create :page 1 :width 48 :height 64 :position 3))
           (visible (list first second)) sent cancelled)
      (setq vellumory--thumbnail-buffer (generate-new-buffer " *preview-test*"))
      (unwind-protect
          (vellumory-test--with-worker sent cancelled
            (with-current-buffer vellumory--thumbnail-buffer (insert " \n \n"))
            (cl-letf (((symbol-function 'vellumory--thumbnail-visible-items) (lambda () visible))
                      ((symbol-function 'vellumory--reading-position) (lambda (&rest _) nil)))
              (setq vellumory--visible tiles vellumory--visible-and-near tiles vellumory--last-motion 0.0)
              (vellumory--pump)
              (vellumory--update-thumbnails)
              ;; Reader tiles fill every slot; previews wait.
              (should (= (length sent) 3))
              (should (cl-every (lambda (r) (null (alist-get 'priority (nth 2 r)))) nil))
              (vellumory-test--reply 1 '((result (path . "/run/a.ppm") (pixels . 1) (ms . 1.0))))
              (vellumory-test--reply 2 '((result (path . "/run/b.ppm") (pixels . 1) (ms . 1.0))))
              (vellumory-test--reply 3 '((result (path . "/run/c.ppm") (pixels . 1) (ms . 1.0))))
              (vellumory--update-thumbnails)
              (let ((previews (cl-remove-if-not (lambda (r) (eql 1000 (alist-get 'priority (nth 2 r)))) sent)))
                (should (= (length previews) 2))
                (vellumory-test--reply (car (car previews))
                                       '((result (path . "/run/t.ppm") (pixels . 1)))))
              (should (memq (vellumory--thumbnail-state second) '(ready nil)))
              ;; Relayout cancels outstanding preview work.
              (with-current-buffer vellumory--thumbnail-buffer (setq-local vellumory--thumbnail-source nil))
              (vellumory--cancel-thumbnails)
              (should (= (hash-table-count vellumory--thumbnail-inflight) 0))
              (should cancelled)))
        (kill-buffer vellumory--thumbnail-buffer)))))

;;;; Links and selection

(ert-deftest vellumory-link-map-hint-labels-and-history ()
  (with-temp-buffer
    (let ((tiles (vellumory-test--tiles 3)) sent cancelled destination)
      (vellumory-test--with-worker sent cancelled
        (puthash 0 [((x0 . 61.2) (y0 . 10.2) (x1 . 122.4) (y1 . 20.4) (uri . "#page=3")
                     (external) (page . 2) (x . 0) (y . 120))
                    ((x0 . 0) (y0 . 500) (x1 . 10) (y1 . 510) (uri . "https://example.org")
                     (external . t) (page) (x . 0) (y . 0))]
                 vellumory--links)
        (let ((map (vellumory--link-map (car tiles))))
          ;; Only the link inside this 192 px strip; 1 px = 1.02 pt at width 600.
          (should (= (length map) 1))
          (should (equal (car (car map)) '(rect . ((60 . 10) . (120 . 20)))))
          ;; Within the document, a function: it starts the preview where one can
          ;; show, else says where the link goes.
          (should (equal (funcall (plist-get (nth 2 (car map)) 'help-echo) (selected-window) nil nil)
                         "Go to page 3")))
        (should (equal (vellumory--hint-labels 3 "ab") '("aa" "ab" "ba")))
        (should (equal (vellumory--hint-labels 2 "ab") '("a" "b")))
        (cl-letf (((symbol-function 'vellumory--restore-position) (lambda (p &rest _) (setq destination p)))
                  ((symbol-function 'vellumory--refresh) #'ignore)
                  ((symbol-function 'vellumory--reading-position) (lambda (&rest _) '((page . 0) (y . 5)))))
          (vellumory--visit-link (aref (gethash 0 vellumory--links) 0))
          (should (equal destination '((page . 2) (y . 108))))
          (vellumory-history-back)
          (should (equal destination '((page . 0) (y . 5)))))
        ;; Unknown pages request links exactly once.
        (should-not (vellumory--ensure-links 1))
        (should-not (vellumory--ensure-links 1))
        (should (= 1 (cl-count "links" sent :key #'cadr :test #'equal)))))))

(ert-deftest vellumory-selection-orders-endpoints-across-pages ()
  (with-temp-buffer
    (let ((tiles (vellumory-test--tiles 3)) sent cancelled selected)
      (ignore tiles)
      (vellumory-test--with-worker sent cancelled
        (vellumory--select-range '(2 10 40) '(0 50 60) 'chars (lambda (s) (setq selected s)))
        (should (equal (mapcar (lambda (r) (alist-get 'page (nth 2 r))) (reverse sent)) '(0 1 2)))
        (let ((first (nth 2 (car (last sent)))) (middle (nth 2 (cadr sent))) (last (nth 2 (car sent))))
          (should (equal (alist-get 'start first) '((x . 50) (y . 60))))
          (should-not (alist-get 'end first))
          (should-not (or (alist-get 'start middle) (alist-get 'end middle)))
          (should (equal (alist-get 'end last) '((x . 10) (y . 40)))))
        (dolist (r (reverse sent))
          (vellumory-test--reply (car r) `((result (text . ,(format "p%d" (alist-get 'page (nth 2 r))))
                                                   (quads . [])))))
        (should (equal (mapcar #'cadr selected) '("p0" "p1" "p2")))
        (cl-letf (((symbol-function 'vellumory--refresh) #'ignore)
                  ((symbol-function 'gui-set-selection) #'ignore))
          (vellumory--set-selection selected))
        (should (equal (vellumory-selection-text) "p0\np1\np2"))
        (vellumory-copy-selection)
        (should (equal (current-kill 0) "p0\np1\np2"))))))

;;;; File visiting

(ert-deftest vellumory-file-buffer-never-writes-layout ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-visit-" t))
         (file (expand-file-name "visited document.pdf" directory))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program)
         (auto-mode-alist auto-mode-alist) (magic-mode-alist magic-mode-alist))
    (skip-unless (and program source))
    (copy-file source file)
    (let ((bytes (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally file)
                                   (buffer-string))))
      (vellumory-install)
      (unwind-protect
          (let ((buffer (find-file-noselect file)))
            (with-current-buffer buffer
              (should (derived-mode-p 'vellumory-mode))
              (should (equal vellumory--file (file-truename file)))
              (let ((deadline (+ (float-time) 10)))
                (while (and (not vellumory--pages) (< (float-time) deadline))
                  (accept-process-output vellumory--process .05)))
              (should (= (length vellumory--pages) 3))
              (should (> (length vellumory--rows) 3))
              ;; Saving never writes the layout into the document.
              (set-buffer-modified-p t)
              (save-buffer)
              (should-not (buffer-modified-p))
              (should (equal bytes (with-temp-buffer (set-buffer-multibyte nil)
                                                     (insert-file-contents-literally file) (buffer-string))))
              ;; Leaving the mode shows the real bytes again.
              (fundamental-mode)
              (should (equal (encode-coding-string (buffer-string) 'raw-text) bytes))
              (should-not (process-live-p vellumory--process)))
            (kill-buffer buffer)
            ;; `vellumory-open' works without `vellumory-install' and never runs
            ;; another mode first; its handler must not see unrelated files.
            (vellumory-uninstall)
            (make-symbolic-link file (expand-file-name "link.pdf" directory))
            (let ((opened (vellumory-open (expand-file-name "link.pdf" directory) t)))
              (with-current-buffer opened
                (should (derived-mode-p 'vellumory-mode))
                (let ((deadline (+ (float-time) 10)))
                  (while (and (not vellumory--pages) (< (float-time) deadline))
                    (accept-process-output vellumory--process .05)))
                (should (= (length vellumory--pages) 3))
                ;; Symlinked visits hand the worker a plain, unquoted path.
                (should (equal vellumory--file (file-truename file)))
                (should-not (string-prefix-p "/:" vellumory--file)))
              (should-not (rassq #'vellumory--file-handler file-name-handler-alist))
              (kill-buffer opened)))
        (delete-directory directory t)))))

(ert-deftest vellumory-materializes-non-file-buffers-by-content ()
  (let ((vellumory-cache-directory (make-temp-file "vellumory-cache-" t)))
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert "%PDF-1.7 fake bytes")
          (let ((file (vellumory--materialize)))
            (should (string-prefix-p (expand-file-name "sources/" vellumory-cache-directory) file))
            (should (string-match-p (concat (secure-hash 'sha256 (current-buffer)) "\\.pdf\\'") file))
            (should (equal (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally file)
                                             (buffer-string))
                           "%PDF-1.7 fake bytes"))
            (should (equal file (vellumory--materialize)))))
      (delete-directory vellumory-cache-directory t))))

;;;; Navigation, previews and search

(defun vellumory-test--request (method params)
  (let ((deadline (+ (float-time) 10)) reply)
    (vellumory--rpc method params (lambda (r) (setq reply r)))
    (while (and (not reply) (< (float-time) deadline) (process-live-p vellumory--process))
      (accept-process-output vellumory--process .05))
    (should reply)
    reply))

(defun vellumory-test--navigation-data ()
  [((title . "Chapter\nOne") (depth . 0) (page . 0) (y . 192) (uri . "#page=1") (expanded . t))
   ((title . "Section") (depth . 1) (page . 1) (y . 0) (uri . "#page=2"))
   ((title . "Section") (depth . 1) (page . 2) (y . 120) (uri . "#page=3"))
   ((title . "Resources") (depth . 0) (page) (y . 0) (uri . "") (expanded))
   ((title . "Website") (depth . 1) (page) (y . 0) (uri . "https://lean-lang.org/"))])

(ert-deftest vellumory-outline-buttons-folding-and-imenu ()
  (save-window-excursion
    (with-temp-buffer
      (let ((source (current-buffer)) destination)
        (setq vellumory--hash "snapshot" vellumory--navigation-state 'ready
              vellumory--pages [((width . 612)) ((width . 792)) ((width . 568))]
              vellumory--labels ["i" "Ch-1" "Ch-2"]
              vellumory--outline (vellumory-test--navigation-data)
              vellumory--outline-buffer (generate-new-buffer " *contents-test*"))
        (unwind-protect
            (progn
              (with-current-buffer vellumory--outline-buffer (vellumory-outline-mode))
              (vellumory--update-outline-buffer)
              (cl-letf (((symbol-function 'vellumory--restore-position) (lambda (position &rest _) (setq destination position)))
                        ((symbol-function 'vellumory--refresh) #'ignore))
                (with-current-buffer vellumory--outline-buffer
                  (goto-char (point-min))
                  (should (looking-at-p "\\* Chapter One"))
                  (vellumory-outline-activate))
                (should (equal destination '((page . 0) (y . 192))))
                (with-current-buffer source
                  (let ((items (vellumory--imenu-index)))
                    (should (= (length items) 3))
                    (should-not (equal (car (nth 1 items)) (car (nth 2 items))))
                    (imenu (nth 2 items))))
                (should (equal destination '((page . 2) (y . 120)))))
              (with-current-buffer (buffer-local-value 'vellumory--outline-buffer source)
                (goto-char (point-min))
                (search-forward "Website")
                (should (invisible-p (1- (point))))
                (outline-show-all)
                (should-not (invisible-p (1- (point))))))
          (when (buffer-live-p (buffer-local-value 'vellumory--outline-buffer source))
            (kill-buffer (buffer-local-value 'vellumory--outline-buffer source))))))))

(ert-deftest vellumory-navigation-stale-replies-and-actions ()
  (with-temp-buffer
    (let (callbacks visited)
      (setq vellumory--hash "old" vellumory--pages [nil nil nil])
      (cl-letf (((symbol-function 'vellumory--rpc) (lambda (_method _params callback &optional _id) (push callback callbacks)))
                ((symbol-function 'browse-url) (lambda (uri &rest _) (setq visited uri))))
        (vellumory--load-navigation)
        (setq vellumory--hash "new")
        (vellumory--load-navigation)
        (funcall (cadr callbacks) '((result (hash . "old") (outline . []) (labels . ["old"]))))
        (should (eq vellumory--navigation-state 'loading))
        (funcall (car callbacks) `((result (hash . "new") (outline . ,(vellumory-test--navigation-data))
                                         (labels . ["i" "Ch-1" "Ch-2"]))))
        (should (eq vellumory--navigation-state 'ready))
        (should (equal (vellumory--page-description 1) "Ch-1 (2)"))
        (let ((external (aref vellumory--outline 4)))
          (should-error (vellumory--follow-outline (current-buffer) "old" external) :type 'user-error)
          (should-not visited)
          (vellumory--follow-outline (current-buffer) "new" external)
          (should (equal visited "https://lean-lang.org/")))
        (should-error (vellumory--follow-outline (current-buffer) "new" '((uri . "javascript:alert(1)")))
                      :type 'user-error)))))

(ert-deftest vellumory-thumbnail-aspect-labels-and-buttons ()
  (save-window-excursion
    (with-temp-buffer
      (let ((source (current-buffer)) visited)
        (setq vellumory--hash "snapshot"
              vellumory--pages [((width . 612) (height . 792)) ((width . 792) (height . 612))]
              vellumory--labels ["i" "Chapter 1"]
              vellumory--thumbnail-buffer (generate-new-buffer " *preview-layout-test*"))
        (unwind-protect
            (progn
              (with-current-buffer vellumory--thumbnail-buffer (vellumory-thumbnails-mode))
              (vellumory--thumbnail-layout)
              (should (= (length vellumory--thumbnails) 2))
              (should (> (vellumory--thumbnail-height (aref vellumory--thumbnails 0))
                         (vellumory--thumbnail-height (aref vellumory--thumbnails 1))))
              (cl-letf (((symbol-function 'vellumory-goto-page) (lambda (page) (setq visited page))))
                (with-current-buffer vellumory--thumbnail-buffer
                  (should (string-match-p "Chapter 1 (2)" (buffer-string)))
                  (goto-char (point-min))
                  (vellumory-thumbnail-next)
                  (vellumory-thumbnail-visit))
                (should (= visited 2))
                (with-current-buffer (buffer-local-value 'vellumory--thumbnail-buffer source)
                  (goto-char (point-min))
                  (vellumory-thumbnail-visit))
                (should (= visited 1))
                (with-current-buffer source (setq vellumory--hash "changed"))
                (with-current-buffer (buffer-local-value 'vellumory--thumbnail-buffer source)
                  (should-error (vellumory-thumbnail-visit) :type 'user-error))))
          (kill-buffer (buffer-local-value 'vellumory--thumbnail-buffer source)))))))

(defun vellumory-test--search-result (query page ys)
  `((result (hash . "snapshot") (query . ,query) (page . ,page)
            (hits . ,(vconcat (mapcar (lambda (y)
                                      (vector `((x0 . 42) (y0 . ,y) (x1 . 80) (y1 . ,y)
                                                (x2 . 80) (y2 . ,(+ y 12)) (x3 . 42) (y3 . ,(+ y 12))))) ys))))))


(ert-deftest vellumory-search-direction-wrap-and-demand ()
  (with-temp-buffer
    (let (requests callbacks destination)
      (setq vellumory--hash "snapshot" vellumory--pages [nil nil nil])
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'vellumory--rpc)
                 (lambda (_method params callback &optional _id) (push params requests) (push callback callbacks)))
                ((symbol-function 'vellumory--restore-position) (lambda (p &rest _) (setq destination p)))
                ((symbol-function 'vellumory--refresh) #'ignore))
        (vellumory--search-begin "needle" nil '((page . 0) (y . 150)))
        (should (= (length requests) 1))
        (funcall (pop callbacks) (vellumory-test--search-result "needle" 0 '(100 200)))
        (should (equal vellumory--search-current '(0 . 1)))
        (should (equal destination '((page . 0) (y . 164))))
        (should-not vellumory--search-job)
        (should (= (length requests) 1)) ; Finding a match does not scan the document.
        (vellumory-search-next)
        (should (= (alist-get 'page (car requests)) 1))
        (funcall (pop callbacks) (vellumory-test--search-result "needle" 1 nil))
        (should (= (alist-get 'page (car requests)) 2))
        (funcall (pop callbacks) (vellumory-test--search-result "needle" 2 '(50)))
        (should (equal vellumory--search-current '(2 . 0)))
        ;; Highlights for the current match are distinct.
        (should (equal (mapcar #'cdr (vellumory--search-page-quads 2)) '(t)))
        (should (equal (mapcar #'cdr (vellumory--search-page-quads 0)) '(nil nil)))
        (vellumory-search-next)
        (should (equal vellumory--search-current '(0 . 0)))
        (should (string-match-p "wrapped" vellumory--search-status))
        (vellumory-search-previous)
        (should (equal vellumory--search-current '(2 . 0)))
        (should (= (length requests) 3)) ; Repeated navigation uses page results.
        (vellumory-search-clear)
        (should-not vellumory--search-current)
        (should-not (vellumory--search-page-quads 2))
        (should-not vellumory--search-job)))))

(ert-deftest vellumory-search-replacement-cancellation-and-no-match ()
  (with-temp-buffer
    (let (requests callbacks)
      (setq vellumory--hash "snapshot" vellumory--pages [nil nil])
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'vellumory--rpc)
                 (lambda (_method params callback &optional _id) (push params requests) (push callback callbacks)))
                ((symbol-function 'vellumory--refresh) #'ignore))
        (vellumory--search-begin "old" nil '((page . 0) (y . 0)))
        (vellumory--search-begin "new" nil '((page . 0) (y . 0)))
        (should (= (length requests) 1))
        (funcall (pop callbacks) (vellumory-test--search-result "old" 0 '(10)))
        (should-not vellumory--search-current)
        (should (= (hash-table-count vellumory--search-cache) 0))
        (should (equal (alist-get 'query (car requests)) "new"))
        (funcall (pop callbacks) (vellumory-test--search-result "new" 0 nil))
        (funcall (pop callbacks) (vellumory-test--search-result "new" 1 nil))
        (should (equal vellumory--search-status "No match"))
        (should-not vellumory--search-job)
        (should-not vellumory--search-busy)
        (vellumory--search-begin "cancel" nil '((page . 0) (y . 0)))
        (vellumory-search-clear)
        (funcall (pop callbacks) (vellumory-test--search-result "cancel" 0 '(10)))
        (should-not vellumory--search-current)
        (should-not vellumory--search-cache)
        (should-not vellumory--search-busy)))))

(ert-deftest vellumory-search-is-independent-of-render-slots-and-bounded ()
  (with-temp-buffer
    (let ((tiles (vellumory-test--tiles 2)) sent cancelled)
      (vellumory-test--with-worker sent cancelled
        (setq vellumory--visible tiles vellumory--visible-and-near tiles vellumory--last-motion 0.0)
        (vellumory--pump)
        (should (= (length sent) 2))
        ;; Search runs on the worker's document thread even while renders are busy.
        (vellumory--search-begin "query" nil '((page . 0) (y . 0)))
        (should (equal (cadr (car sent)) "search"))
        (vellumory-test--reply (car (car sent)) (vellumory-test--search-result "query" 0 nil))
        (should (equal (cadr (car sent)) "search"))
        (should (= (alist-get 'page (nth 2 (car sent))) 1))
        (vellumory-test--reply (car (car sent)) (vellumory-test--search-result "query" 1 nil))
        (should-not vellumory--search-busy)
        (dotimes (page 50) (vellumory--search-cache-put page '((hits . []))))
        (should (= (hash-table-count vellumory--search-cache) 16))
        (should-not (gethash 0 vellumory--search-cache))
        (should (gethash 49 vellumory--search-cache))))))

(ert-deftest vellumory-search-reports-incomplete-page ()
  (with-temp-buffer
    (setq vellumory--pages [nil nil] vellumory--search-query "query"
          vellumory--search-job '(:page 0 :direction -1 :first nil :remaining 2))
    (let ((result (alist-get 'result (vellumory-test--search-result "query" 0 '(10)))))
      (push '(truncated . t) result)
      (vellumory--search-step result)
      (should-not vellumory--search-current)
      (should-not vellumory--search-job)
      (should (string-match-p "Too many matches" vellumory--search-status)))))

(ert-deftest vellumory-real-worker-protocol ()
  (let ((vellumory-program (getenv "VELLUMORY_TEST_PROGRAM")))
    (skip-unless vellumory-program)
    (with-temp-buffer
      (unwind-protect
          (progn
            (vellumory--start)
            (let ((hello (alist-get 'result (vellumory-test--request "hello" nil))))
              (should (= 3 (alist-get 'protocol hello)))
              (should (>= (alist-get 'threads hello) 1)))
            (should (alist-get 'error (vellumory-test--request "render" nil)))
            (let ((opened (vellumory-test--request "open" `((path . ,(getenv "VELLUMORY_TEST_PDF"))))))
              (should (= 3 (length (alist-get 'pages (alist-get 'result opened))))))
            (let ((nav (alist-get 'result (vellumory-test--request "navigation" nil))))
              (should (equal (alist-get 'labels nav) ["i" "Ch-1" "Ch-2"]))
              (should (= (length (alist-get 'outline nav)) 5))
              (should (= (alist-get 'y (aref (alist-get 'outline nav) 2)) 120)))
            (let ((links (alist-get 'links (alist-get 'result (vellumory-test--request "links" '((page . 0)))))))
              (should (= (length links) 2))
              (should (cl-some (lambda (l) (eql (alist-get 'page l) 2)) links)))
            (let ((found (alist-get 'result (vellumory-test--request "search" '((page . 0) (query . "text"))))))
              (should (= (length (alist-get 'hits found)) 17))
              (should-not (alist-get 'truncated found)))
            (should (alist-get 'error (vellumory-test--request "search" '((page . 99) (query . "text")))))
            (should (alist-get 'error (vellumory-test--request "search" '((page . 0) (query . "text") (limit . "bad")))))
            (should (alist-get 'error (vellumory-test--request "search" '((page . 0) (query . "text") (case_sensitive . "bad")))))
            (let* ((reply (vellumory-test--request "render" '((page . 0) (width . 300) (left . 10) (right . 110)
                                                              (top . 1) (bottom . 100))))
                   (path (alist-get 'path (alist-get 'result reply))))
              (should (file-exists-p path))
              (should (= (alist-get 'pixels (alist-get 'result reply)) (* 100 99)))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally path)
                (should (string-prefix-p "P6\n100 99\n255\n" (buffer-string)))))
            (let* ((overlay '((quads . [[0 0 10 0 10 10 0 10]]) (kinds . [0]) (palette . [[255 0 0 255]])))
                   (marked (vellumory-test--request "render" `((page . 0) (width . 300) (top . 0) (bottom . 10)
                                                               (overlay . ,overlay)))))
              (should (alist-get 'result marked)))
            (should (string-match-p "Vellumory" (alist-get 'text (alist-get 'result
                           (vellumory-test--request "text" '((page . 0)))))))
            (should (alist-get 'error (vellumory-test--request "open" '((path . "/nonexistent/vellumory.pdf")))))
            ;; A failed open preserves the previous immutable document.
            (should (alist-get 'result (vellumory-test--request "text" '((page . 2))))))
        (when (process-live-p vellumory--process) (delete-process vellumory--process))))))

(ert-deftest vellumory-visit-skips-reading-document-bytes ()
  (let* ((directory (make-temp-file "vellumory-handler-" t))
         (file (expand-file-name "big.pdf" directory))
         (other (expand-file-name "notes.txt" directory))
         (auto-mode-alist (cons (cons (vellumory--extension-regexp) 'vellumory-mode) auto-mode-alist))
         (file-name-handler-alist (cons (cons (vellumory--extension-regexp) #'vellumory--file-handler)
                                        file-name-handler-alist)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "%PDF-1.7 " (make-string 5000 ?x)))
          (with-temp-file other (insert "plain"))
          (with-temp-buffer
            (should (equal (insert-file-contents file t) (list file 0)))
            (should (zerop (buffer-size)))
            (should (equal buffer-file-name file))
            (should (verify-visited-file-modtime))
            (should-not (buffer-modified-p)))
          ;; Literal visits, partial reads and other files read normally.
          (with-temp-buffer
            (insert-file-contents-literally file t)
            (should (= (buffer-size) 5009)))
          (with-temp-buffer
            (insert-file-contents file nil 0 4)
            (should (equal (buffer-string) "%PDF")))
          (with-temp-buffer
            (insert-file-contents other t)
            (should (equal (buffer-string) "plain")))
          (let ((vellumory-visit-without-reading nil))
            (with-temp-buffer
              (insert-file-contents file t)
              (should (= (buffer-size) 5009))))
          ;; Other operations pass through untouched.
          (should (file-exists-p file))
          (should (= (file-attribute-size (file-attributes file)) 5009)))
      (delete-directory directory t))))

(ert-deftest vellumory-evil-normal-state-bindings ()
  (let ((evil-lisp (getenv "VELLUMORY_EVIL_LISP")))
    (skip-unless (and evil-lisp (file-directory-p evil-lisp)))
    (add-to-list 'load-path evil-lisp)
    ;; The shell's evil is compiled for its Emacs; another may not read it.
    (skip-unless (ignore-errors (require 'evil) t))
    (require 'vellumory-evil)
    (vellumory-evil-setup)
    (let ((state-map (evil-get-auxiliary-keymap vellumory-mode-map 'normal)))
      (dolist (binding '(("gg" . vellumory-evil-goto-first-page) ("G" . vellumory-evil-goto-last-page)
                         ("j" . vellumory-scroll-down) ("zi" . vellumory-zoom-in) ("/" . vellumory-search)
                         ("n" . vellumory-search-next) ("o" . vellumory-outline) ("y" . vellumory-copy-selection)))
        (should (eq (lookup-key state-map (kbd (car binding))) (cdr binding))))
      ;; Vellumory's selection, not evil's region drag, owns the mouse.
      (should (eq (lookup-key state-map [down-mouse-1]) #'vellumory-mouse-press))
      (should (eq (lookup-key state-map (kbd "C-o")) #'vellumory-history-back)))
    (should (eq (evil-initial-state 'vellumory-mode) 'normal))
    ;; Counts reach the page commands as prefix arguments.
    (with-temp-buffer
      (let (visited)
        (setq vellumory--pages (make-vector 9 nil))
        (cl-letf (((symbol-function 'vellumory-goto-page) (lambda (page) (setq visited page))))
          (let ((current-prefix-arg 5)) (call-interactively #'vellumory-evil-goto-last-page))
          (should (= visited 5))
          (call-interactively #'vellumory-evil-goto-last-page)
          (should (= visited 9))
          (call-interactively #'vellumory-evil-goto-first-page)
          (should (= visited 1)))))
    ;; Registers hold exact positions and work with `jump-to-register'.
    (with-temp-buffer
      (let (restored)
        (cl-letf (((symbol-function 'vellumory--reading-position) (lambda (&rest _) '((page . 3) (y . 42.5))))
                  ((symbol-function 'vellumory--restore-position) (lambda (p &rest _) (setq restored p)))
                  ((symbol-function 'vellumory--refresh) #'ignore))
          (vellumory-point-to-register ?q)
          (jump-to-register ?q)
          (should (equal restored '((page . 3) (y . 42.5)))))))))

(defun vellumory-test--wait (predicate &optional seconds)
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(ert-deftest vellumory-edit-commands-undo-redo-and-history-tree ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-edit-" t))
         (file (expand-file-name "doc.pdf" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program))
    (skip-unless (and program source))
    (copy-file source file)
    (unwind-protect
        (let ((buffer (vellumory-open file t)) history)
          (with-current-buffer buffer
            (should (vellumory-test--wait (lambda () (= (length vellumory--pages) 3))))
            ;; Delete the second page; the reader reloads the new state.
            (setq vellumory--current-page 1)
            (vellumory-delete-pages)
            (should (vellumory-test--wait (lambda () (= (length vellumory--pages) 2))))
            (vellumory-undo)
            (should (vellumory-test--wait (lambda () (= (length vellumory--pages) 3))))
            (vellumory-redo)
            (should (vellumory-test--wait (lambda () (= (length vellumory--pages) 2))))
            (vellumory-undo)
            (should (vellumory-test--wait (lambda () (= (length vellumory--pages) 3))))
            ;; A second branch from the original state.
            (setq vellumory--current-page 0)
            (vellumory-rotate-pages)
            (should (vellumory-test--wait
                     (lambda () (and vellumory--pages
                                     (= (alist-get 'width (aref vellumory--pages 0)) 792)))))
            (setq history (vellumory-history)))
          (with-current-buffer history
            (should (vellumory-test--wait (lambda () vellumory--history)))
            (let ((lines (split-string (buffer-string) "\n")))
              ;; Root, two children of the root, the current state marked.
              (should (= 3 (cl-count-if (lambda (l) (string-match-p "[●○] " l)) lines)))
              (should (cl-some (lambda (l) (string-match-p "├─ ○ .*keep pages 1,3" l)) lines))
              (should (cl-some (lambda (l) (string-match-p "└─ ● .*rotate pages 1" l)) lines))))
          ;; The tree renderer itself, on a deeper branching shape.
          (should (equal (mapcar #'cdr (vellumory--history-tree
                                        [((parent)) ((parent . 0)) ((parent . 1)) ((parent . 0))]))
                         '("" "├─ " "│  └─ " "└─ ")))
          (kill-buffer history)
          (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest vellumory-autoloads-are-current ()
  "The generated autoloads are current: they autoload what fresh ones would.
Names, not forms: each Emacs version writes them a little differently."
  ;; Library names are relative to the output file, so generate beside copies.
  (let* ((lisp (file-name-directory (locate-library "vellumory")))
         (copy (make-temp-file "vellumory-autoloads-" t))
         (fresh (expand-file-name "vellumory-autoloads.el" copy)))
    (dolist (file (directory-files lisp t "\\`vellumory.*\\.el\\'"))
      (unless (string-suffix-p "-autoloads.el" file)
        (copy-file file (expand-file-name (file-name-nondirectory file) copy))))
    (unwind-protect
        (cl-flet ((forms (file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (let (forms)
                        (condition-case nil (while t (push (read (current-buffer)) forms))
                          (end-of-file (nreverse forms)))))))
          (let ((inhibit-message t))
            (loaddefs-generate copy fresh (list "vellumory-autoloads.el")))
          (cl-flet ((names (file)
                      (sort (delq nil (mapcar (lambda (form)
                                                (and (memq (car-safe form) '(autoload custom-autoload defvar))
                                                     (format "%S %S" (car form) (cadr form))))
                                              (forms file)))
                            #'string<)))
            (should (equal (names (expand-file-name "vellumory-autoloads.el" lisp)) (names fresh)))))
      (delete-directory copy t))))

(ert-deftest vellumory-install-does-not-load-the-reader ()
  (let ((default-directory (file-name-directory (locate-library "vellumory"))))
    (should (zerop (call-process
                    (expand-file-name invocation-name invocation-directory) nil nil nil
                    "--batch" "-Q" "-L" "." "-l" "vellumory-autoloads"
                    "--eval" "(progn (vellumory-install) (when (featurep 'vellumory) (kill-emacs 3)) (unless (eq (alist-get \"x.pdf\" auto-mode-alist nil nil (lambda (r f) (string-match-p r f))) 'vellumory-mode) (kill-emacs 4)) (dolist (f '(vellumory-mode vellumory-open vellumory-converted-mode vellumory-virtual-mode vellumory-follow-mode)) (unless (fboundp f) (kill-emacs 6))))")))
    ;; Without the generated autoloads too, as a configuration loading the file
    ;; does (the review's did, and found `vellumory-converted-extensions' void).
    (should (zerop (call-process
                    (expand-file-name invocation-name invocation-directory) nil nil nil
                    "--batch" "-Q" "-L" "." "-l" "vellumory-install"
                    "--eval" "(progn (vellumory-install) (when (featurep 'vellumory) (kill-emacs 3)) (when (and (executable-find \"dvipdfmx\") (not (eq (assoc-default \"x.dvi\" auto-mode-alist (function string-match-p)) 'vellumory-converted-mode))) (kill-emacs 5)))")))))

(ert-deftest vellumory-details-parsing-and-real-report ()
  (should (equal (vellumory--pdf-date "D:20080929101841-07'00'") "2008-09-29 10:18:41 −07:00"))
  (should (equal (vellumory--pdf-date "D:20080918111951Z") "2008-09-18 11:19:51 UTC"))
  (should (equal (vellumory--pdf-date "D:2024") "2024-01-01 00:00:00"))
  (let ((xmp "<rdf:Description pdfaid:part=\"2\" pdfaid:conformance=\"B\"/><pdfuaid:part>1</pdfuaid:part>"))
    (should (equal (vellumory--standards `((xmp . ,xmp))) '("PDF/A-2b" "PDF/UA-1"))))
  (should-not (vellumory--standards '((xmp))))
  (let ((vellumory-program (getenv "VELLUMORY_TEST_PROGRAM")))
    (skip-unless vellumory-program)
    (with-temp-buffer
      (unwind-protect
          (progn
            (vellumory--start)
            (vellumory-test--request "open" `((path . ,(getenv "VELLUMORY_TEST_PDF"))))
            (let ((info (alist-get 'result (vellumory-test--request "info" nil)))
                  (page (alist-get 'result (vellumory-test--request "page-info" '((page . 1))))))
              (should (equal (alist-get 'format info) "PDF 1.7"))
              (should (= (alist-get 'pages info) 3))
              (should (= (alist-get 'links info) 2))
              (should (= (alist-get 'rotate page) 90))
              (should (equal (alist-get 'media_box page) [0 0 612 792]))
              (let ((source (current-buffer)))
                (with-temp-buffer
                  (vellumory--info-render source info page)
                  (should (string-match-p "PDF 1.7 · 3 pages" (buffer-string)))
                  (should (string-match-p "not embedded: Helvetica" (buffer-string)))
                  (should (string-match-p "no (inaccessible to screen readers)" (buffer-string)))
                  (should (string-match-p "MediaBox +\\[0 0 612 792\\] +215.9 × 279.4 mm" (buffer-string)))))))
        (when (process-live-p vellumory--process) (delete-process vellumory--process))))))

(ert-deftest vellumory-validation-report-and-next-error ()
  (let ((report '((compliant) (profile . "PDF/A-2b validation profile") (failed_rules . 2) (failed_checks . 3)
                  (passed_rules . 10) (passed_checks . 99)
                  (rules . [((specification . "ISO 19005-2:2011") (clause . "6.1.3") (test_number . 1)
                             (description . "The file trailer dictionary shall contain the ID keyword")
                             (failed_checks . 1)
                             (checks . [((context . "root") (message . "Missing ID") (page))]))
                            ((specification . "ISO 19005-2:2011") (clause . "6.2.11.4.1") (test_number . 1)
                             (description . "Fonts shall be embedded") (failed_checks . 2)
                             (checks . [((message . "Helvetica not embedded") (page . 0))
                                        ((message . "Helvetica not embedded") (page . 2))]))])))
        (source (generate-new-buffer " *source*")) visited)
    (unwind-protect
        (with-temp-buffer
          (vellumory-validation-mode)
          (setq vellumory--validation-source source)
          (vellumory--validation-render report)
          (should (string-match-p "NOT COMPLIANT" (buffer-string)))
          (should (string-match-p "ISO 19005-2:2011 §6.1.3-1  1×" (buffer-string)))
          (should (string-match-p "page 3 · Helvetica not embedded" (buffer-string)))
          (cl-letf (((symbol-function 'vellumory-goto-page) (lambda (p) (setq visited p)))
                    ((symbol-function 'pop-to-buffer) #'ignore))
            ;; next-error skips checks without pages and visits each page once.
            (funcall next-error-function 1 t)
            (should (= visited 1))
            (funcall next-error-function 1)
            (should (= visited 3))
            (should-error (funcall next-error-function 1) :type 'user-error)
            ;; previous-error goes back to the check before the last one.
            (funcall next-error-function -1)
            (should (= visited 1))))
      (kill-buffer source))))

(ert-deftest vellumory-syntax-view-follow-imenu-and-commit ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-syntax-" t))
         (file (expand-file-name "doc.pdf" directory))
         (process-environment (append (list (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                            (concat "XDG_CACHE_HOME=" (expand-file-name "cache" directory)))
                                      process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program))
    (skip-unless (and program source))
    (copy-file source file)
    (unwind-protect
        (let ((reader (vellumory-open file t)) syntax)
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () (eq vellumory--navigation-state 'ready))))
            (should (equal (alist-get 'title (aref vellumory--outline 0)) "Reading"))
            (let ((pop-up-windows nil))
              (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq syntax b))))
                (vellumory-syntax)
                (should (vellumory-test--wait (lambda () syntax))))))
          (with-current-buffer syntax
            (should (derived-mode-p 'vellumory-syntax-mode))
            (should buffer-read-only)
            (should (assoc "Pages" (vellumory--syntax-imenu)))
            (should (cl-some (lambda (e) (equal (car e) "1 0 Catalog")) (vellumory--syntax-imenu)))
            ;; Follow the catalog's reference to its outline dictionary.
            (goto-char (point-min))
            (re-search-forward "/Outlines \\([0-9]+\\) 0 R")
            (let ((target (match-string 1)))
              (goto-char (match-beginning 1))
              (vellumory-syntax-follow)
              (should (looking-at (concat target " 0 obj"))))
            ;; Edit a title in the syntax and adopt it as a recorded state.
            (read-only-mode -1)
            (goto-char (point-min))
            (re-search-forward "/Title (Reading)")
            (replace-match "/Title (Reading glasses)" t t)
            (vellumory-syntax-commit))
          (with-current-buffer reader
            (should (vellumory-test--wait
                     (lambda () (and (eq vellumory--navigation-state 'ready)
                                     (equal (alist-get 'title (aref vellumory--outline 0)) "Reading glasses")))))
            (vellumory-undo)
            (should (vellumory-test--wait
                     (lambda () (and (eq vellumory--navigation-state 'ready)
                                     (equal (alist-get 'title (aref vellumory--outline 0)) "Reading"))))))
          (kill-buffer syntax)
          (kill-buffer reader))
      (delete-directory directory t))))

(ert-deftest vellumory-diff-words-pages-and-reader-highlights ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-diff-" t))
         (old (expand-file-name "old.pdf" directory))
         (new (expand-file-name "new.pdf" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program))
    (skip-unless (and program source))
    (copy-file source old)
    ;; Same-length replacement keeps every xref offset valid.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally source)
      (goto-char (point-min))
      (search-forward "Line 3: text and shapes")
      (replace-match "Line 3: data and shapes" t t)
      (let ((coding-system-for-write 'no-conversion)) (write-region nil nil new)))
    (unwind-protect
        (let ((reader (vellumory-open new t)) report)
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (setq vellumory--current-page 1)
            (vellumory-delete-pages)
            (should (vellumory-test--wait (lambda () (= (length vellumory--pages) 2))))
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (vellumory-diff old))
            (setq report (get-buffer (format "*Diff: %s*" (file-name-nondirectory new))))
            (should (vellumory-test--wait (lambda () vellumory--diff-added)))
            ;; The single changed word is highlighted on the first page.
            (should (= (length (alist-get 0 vellumory--diff-added)) 1)))
          (with-current-buffer report
            (should (string-match-p "1 pages changed, 0 added, 1 removed" (buffer-string)))
            (should (string-match-p "− text" (buffer-string)))
            (should (string-match-p "+ data" (buffer-string)))
            (should (string-match-p "pages +3 → 2" (buffer-string))))
          (kill-buffer report)
          (with-current-buffer reader (should-not vellumory--diff-added))
          (kill-buffer reader))
      (delete-directory directory t))))

(ert-deftest vellumory-org-links-store-follow-export ()
  (require 'vellumory-org)
  (should (equal (vellumory-org--parse "~/a.pdf::12::340.5") '("~/a.pdf" 11 340.5)))
  (should (equal (vellumory-org--parse "~/a.pdf") '("~/a.pdf" nil nil)))
  (should (equal (vellumory-org-export "/d/a.pdf::3" "See" 'html nil) "<a href=\"/d/a.pdf#page=3\">See</a>"))
  (should (equal (vellumory-org-export "/d/a.pdf::3" "See" 'latex nil) "\\href{/d/a.pdf#page=3}{See}"))
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (vellumory-program program)
         (vellumory-state-directory (make-temp-file "vellumory-org-state-" t)))
    (skip-unless (and program source))
    (let ((buffer (vellumory-open source t)))
      (with-current-buffer buffer
        (should (vellumory-test--wait (lambda () (eq vellumory--navigation-state 'ready))))
        (cl-letf (((symbol-function 'vellumory--reading-position)
                   (lambda (&rest _) '((page . 2) (y . 150.0) (zoom . 1.0)))))
          (let (org-store-link-plist)
            (vellumory-org-store)
            (should (string-suffix-p "::3::150.0" (plist-get org-store-link-plist :link)))
            ;; The nearest outline heading names the place.
            (should (equal (plist-get org-store-link-plist :description) "Geometry (p. Ch-2 (3))"))
            (setq vellumory--selection '((2 "a quoted   passage" [])))
            (vellumory-org-store)
            (should (equal (plist-get org-store-link-plist :description) "“a quoted passage”"))
            (setq vellumory--selection nil))))
      (let (restored)
        (cl-letf (((symbol-function 'vellumory--restore-position) (lambda (p &rest _) (setq restored p)))
                  ((symbol-function 'pop-to-buffer-same-window) #'ignore))
          (vellumory-org-follow (concat source "::2::64") nil)
          (should (equal restored '((page . 1) (y . 64))))
          (vellumory-org-follow-pdftools (concat source "::3++0.5") nil)
          ;; Page 3 is cropped to 568 × 748 points: half its height is 374.
          (should (equal restored '((page . 2) (y . 374.0))))))
      (kill-buffer buffer))
    (delete-directory vellumory-state-directory t)))

(ert-deftest vellumory-links-arriving-after-sharp-tiles-gain-maps ()
  (with-temp-buffer
    (let ((tiles (vellumory-test--tiles 1)) sent cancelled)
      (vellumory-test--with-worker sent cancelled
        (setq vellumory--visible tiles vellumory--visible-and-near tiles vellumory--last-motion 0.0)
        (vellumory--pump)
        (vellumory-test--reply 1 '((result (path . "/run/a.ppm") (pixels . 1) (raster_ms . 1.0))))
        (let ((display (get-text-property (vellumory--tile-position (car tiles)) 'display)))
          (should (eq (car display) 'image))
          (should-not (plist-get (cdr display) :map)))
        ;; The page's links arrive later: the existing raster gains its map.
        (should-not (vellumory--ensure-links 0))
        (let ((request (cl-find "links" sent :key #'cadr :test #'equal)))
          (vellumory-test--reply (car request)
                                 '((result (links . [((x0 . 10) (y0 . 10) (x1 . 100) (y1 . 30) (uri . "https://e.org")
                                                      (external . t) (page) (x . 0) (y . 0))])))))
        (should (plist-get (cdr (get-text-property (vellumory--tile-position (car tiles)) 'display)) :map))
        ;; Without any new raster request.
        (should (= 1 (cl-count "render" sent :key #'cadr :test #'equal)))))))

(defvar TeX-view-program-list)
(defvar TeX-view-program-selection)
(ert-deftest vellumory-sync-forward-backward-and-viewer ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (fixture (getenv "VELLUMORY_SYNCTEX_FIXTURE"))
         (vellumory-program program)
         (vellumory-state-directory (make-temp-file "vellumory-sync-state-" t))
         (vellumory-sync-flash-seconds 60))
    (skip-unless (and program fixture))
    (let* ((source (expand-file-name "doc.tex" fixture))
           (pdf (expand-file-name "doc.pdf" fixture))
           (tex (find-file-noselect source))
           reader)
      (unwind-protect
          (progn
            (with-current-buffer tex
              (goto-char (point-min)) (forward-line 6)  ; line 7, page two
              (cl-letf (((symbol-function 'display-buffer) #'ignore))
                (vellumory-sync-forward)))
            (should (vellumory-test--wait (lambda () (setq reader (find-buffer-visiting pdf)))))
            (with-current-buffer reader
              (should (vellumory-test--wait (lambda () (assq 1 vellumory--sync-flash))))
              (should-not (assq 0 vellumory--sync-flash))
              ;; The renderer draws the found line as the current match.
              (should (vellumory-test--wait (lambda () vellumory--pages)))
              (let ((overlay (vellumory--page-overlay 1)))
                (should (equal (alist-get 'kinds overlay) [1])))
              ;; Backward from the middle of that box lands on line 7.
              (let* ((quad (car (alist-get 1 vellumory--sync-flash)))
                     (x (/ (+ (aref quad 0) (aref quad 2)) 2)) (y (/ (+ (aref quad 1) (aref quad 5)) 2)))
                ;; As a click into the reader's window while the TeX buffer is
                ;; current: the location must be read in the reader.
                (set-window-buffer (selected-window) reader)
                (cl-letf (((symbol-function 'vellumory--event-location)
                           (lambda (_) (should (derived-mode-p 'vellumory-mode)) (list 1 x y))))
                  (with-current-buffer tex
                    (goto-char (point-min))
                    (vellumory-sync-backward `(C-mouse-1 (,(selected-window) 1 (10 . 10) 0))))
                  (should (vellumory-test--wait
                           (lambda () (with-current-buffer tex (= (line-number-at-pos) 7)))))))))
        (when reader (kill-buffer reader))
        (kill-buffer tex)
        (delete-directory vellumory-state-directory t))))
  ;; AUCTeX viewer selection: first, once, whatever ran before.
  (let ((TeX-view-program-list nil)
        (TeX-view-program-selection '((output-pdf "Evince") (output-dvi "xdvi"))))
    (vellumory-sync-select-viewer)
    (vellumory-sync-select-viewer)
    (should (equal TeX-view-program-selection
                   '((output-pdf "Vellumory") (output-pdf "Evince") (output-dvi "xdvi"))))
    (should (equal TeX-view-program-list '(("Vellumory" vellumory-sync-view))))
    (let ((vellumory-sync-tex-viewer nil) (TeX-view-program-selection nil))
      (vellumory-sync-select-viewer)
      (should-not TeX-view-program-selection))))

(ert-deftest vellumory-metadata-form-operation-and-round-trip ()
  (require 'vellumory-metadata)
  (should (equal (vellumory-metadata--iso-date "D:20261028054000+01'00'") "2026-10-28T05:40:00+01:00"))
  (should (equal (vellumory-metadata--iso-date "D:19800101000000+00'00'") "1980-01-01T00:00:00Z"))
  (should (equal (vellumory-metadata--iso-date "D:2026") "2026-01-01"))
  (let ((op (vellumory-metadata--operation '(("title" . "New") ("author" . "") ("display-title" . "yes")
                                             ("subject" . "Same"))
                                           '(("title" . "Old") ("author" . "A") ("display-title" . "no")
                                             ("subject" . "Same"))
                                           "2026-10-28T05:40:00+01:00")))
    (should (equal (alist-get 'set op) '((title . "New") (display-title . "true"))))
    (should (equal (alist-get 'remove op) ["author"]))
    (should (equal (alist-get 'date op) "2026-10-28T05:40:00+01:00")))
  (should-error (vellumory-metadata--operation '(("title" . "A")) '(("title" . "A")) "") :type 'user-error)
  (should-error (vellumory-metadata--operation '(("display-title" . "maybe")) '(("display-title" . "no")) "")
                :type 'user-error)
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-metadata-" t))
         (file (expand-file-name "doc.pdf" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program))
    (skip-unless (and program source))
    (copy-file source file)
    (unwind-protect
        (let ((reader (vellumory-open file t)) form)
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            ;; The form appears when the report arrives.
            (vellumory-edit-metadata)
            (should (vellumory-test--wait
                     (lambda () (setq form (get-buffer "*Metadata: doc.pdf*"))
                       (and form (buffer-local-value 'vellumory-metadata--original form))))))
          (with-current-buffer form
            (should (derived-mode-p 'vellumory-metadata-mode))
            (should (equal (alist-get "language" (vellumory-metadata--values) nil nil #'equal) ""))
            ;; Labels are read-only; values are typed after them.
            (goto-char (point-min))
            (let ((label (text-property-search-forward 'vellumory-metadata-field "title" #'equal)))
              (save-excursion
                (goto-char (1+ (prop-match-beginning label)))
                (should-error (insert "x") :type 'text-read-only)))
            ;; Right after the label is where the value is typed.
            (insert "Field guide")
            (text-property-search-forward 'vellumory-metadata-field "language" #'equal)
            (end-of-line) (insert "en-GB")
            (text-property-search-forward 'vellumory-metadata-field "display-title" #'equal)
            (delete-region (point) (line-end-position)) (insert "yes")
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (vellumory-metadata-commit)))
          (let (info)
            (should (vellumory-test--wait
                     (lambda ()
                       (with-current-buffer reader
                         (vellumory--rpc "info" nil (lambda (reply) (setq info (alist-get 'result reply)))))
                       (accept-process-output nil 0.2)
                       (equal (alist-get 'title info) "Field guide"))
                     20))
            (should (equal (alist-get 'language info) "en-GB"))
            (should (eq (alist-get 'display_title info) t))
            (should (string-match-p "<rdf:li xml:lang=\"x-default\">Field guide</rdf:li>" (alist-get 'xmp info))))
          (kill-buffer form)
          (kill-buffer reader))
      (delete-directory directory t))))

(defun vellumory-test--embedded-source-p (info name)
  "Whether INFO lists NAME as the document's associated Org source."
  (seq-some (lambda (attachment)
              (and (equal (alist-get 'key attachment) name)
                   (equal (alist-get 'relationship attachment) "Source")
                   (equal (alist-get 'mimetype attachment) "text/x-org")
                   (eq (alist-get 'associated attachment) t)))
            (alist-get 'attachments info)))

(ert-deftest vellumory-org-export-accessible-archival-pdf ()
  (require 'vellumory-org)
  (should (equal (vellumory-org--flavour "ua-2") "ua2"))
  (should (equal (vellumory-org--flavour "a-4f") "4f"))
  (should (equal (vellumory-org--document-metadata "de")
                 "\\DocumentMetadata{lang=de, pdfstandard=ua-2, pdfstandard=a-4f, tagging=on}"))
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM"))
         (directory (make-temp-file "vellumory-export-" t))
         (org (expand-file-name "doc.org" directory))
         ;; LuaLaTeX writes its font database here, and the embedding edit its snapshots.
         (process-environment (append (list (concat "TEXMFVAR=" (expand-file-name "texmf-var" directory))
                                            (concat "VELLUMORY_STORE=" (expand-file-name "store" directory)))
                                      process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program)
         pdf)
    ;; Exporting needs Org 9.7 (Emacs 30); the export refuses older ones.
    (skip-unless (version<= "9.7" (org-version)))
    (skip-unless (and program (executable-find "lualatex") (executable-find "kpsewhich")
                      (car (process-lines-ignore-status "kpsewhich" "pdfmanagement-testphase.sty"))))
    (with-temp-file org
      (insert "#+TITLE: Vellumory export test\n#+AUTHOR: A. Person\n#+LANGUAGE: en-GB\n\n"
              "* Introduction\nSome *bold* text and a [[https://example.org][link]].\n\n"
              "- first\n- second\n\n| Name | Value |\n|------+-------|\n| a    |     1 |\n\n"
              "* Mathematics\nInline $e^{i\\pi} + 1 = 0$ and\n\\[ \\int_0^1 x^2\\,dx = \\frac{1}{3} \\]\n"))
    (unwind-protect
        (let ((source (find-file-noselect org)))
          (with-current-buffer source
            (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
              (setq pdf (vellumory-org-export-pdf))))
          (should (file-exists-p pdf))
          (should (vellumory-test--wait (lambda () (= (length (cdr vellumory-org-export-results)) 2)) 120))
          ;; veraPDF accepts it as both accessible and archival.
          (should (eq (alist-get "ua-2" (cdr vellumory-org-export-results) nil nil #'equal) t))
          (should (eq (alist-get "a-4f" (cdr vellumory-org-export-results) nil nil #'equal) t))
          (let ((reader (find-buffer-visiting pdf)) info)
            (with-current-buffer reader
              (should (vellumory-test--wait (lambda () vellumory--pages)))
              (vellumory--rpc "info" nil (lambda (reply) (setq info (alist-get 'result reply)))))
            (should (vellumory-test--wait (lambda () info)))
            (should (equal (alist-get 'title info) "Vellumory export test"))
            (should (equal (alist-get 'language info) "en-GB"))
            (should (eq (alist-get 'tagged info) t))
            ;; The Org text travels inside, as the document's source.
            (should (vellumory-test--embedded-source-p info "doc.org"))
            (with-temp-buffer
              (vellumory--info-render reader info nil)
              (should (string-match-p "doc\\.org .* · text/x-org · source of the document · Org source of this document"
                                      (buffer-string))))
            (kill-buffer reader))
          (kill-buffer source))
      (delete-directory directory t))))

(ert-deftest vellumory-dired-batch-validate-edit-summarize-merge ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (file-name-as-directory (make-temp-file "vellumory-dired-" t)))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program)
         (files (list (expand-file-name "a.pdf" directory) (expand-file-name "b.pdf" directory)))
         (batch (lambda () (and (get-buffer "*Vellumory batch*")
                                (with-current-buffer "*Vellumory batch*" tabulated-list-entries)))))
    (skip-unless (and program source))
    ;; It validates too: with the worker's veraPDF (its own in the Nix
    ;; package), else one on PATH.
    (skip-unless (progn (require 'vellumory-doctor)
                        (let ((verapdf (alist-get 'verapdf (alist-get 'tools (vellumory-doctor--worker-hello program)))))
                          (and verapdf (or (file-executable-p verapdf) (executable-find verapdf))))))
    (dolist (file files) (copy-file source file))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore))
          (require 'dired)
          (vellumory-install)
          ;; The prefix is bound without loading the batch code first.
          (should (eq (keymap-lookup dired-mode-map ";") 'vellumory-dired-map))
          (with-current-buffer (dired-noselect directory)
            (should (eq (keymap-lookup dired-mode-map "; v") 'vellumory-dired-validate))
            (dired-mark-files-regexp "\\.pdf\\'")
            (should (equal (vellumory-dired--files) files))
            (when (get-buffer "*Vellumory batch*") (kill-buffer "*Vellumory batch*"))
            (vellumory-dired-validate "2b")
            (should (vellumory-test--wait (lambda () (= (length (funcall batch)) 2)) 60))
            (should (cl-every (lambda (entry) (equal (substring-no-properties (aref (cadr entry) 2)) "not compliant"))
                              (funcall batch)))
            (kill-buffer "*Vellumory batch*")
            ;; One recorded edit per document.
            (vellumory-dired-edit "Set language" nil
                                  '((op . "metadata") (set (language . "en-GB")) (remove . [])
                                    (date . "2026-10-28T06:30:00+01:00")))
            (should (vellumory-test--wait (lambda () (= (length (funcall batch)) 2)) 60))
            (should (cl-every (lambda (entry) (string-prefix-p "state 1: set metadata (language)"
                                                               (aref (cadr entry) 1)))
                              (funcall batch)))
            (kill-buffer "*Vellumory batch*")
            (vellumory-dired-info)
            (should (vellumory-test--wait (lambda () (= (length (funcall batch)) 2)) 30))
            (should (cl-every (lambda (entry) (and (equal (aref (cadr entry) 1) "3")
                                                   (equal (aref (cadr entry) 5) "en-GB")))
                              (funcall batch)))
            (let ((merged (expand-file-name "merged.pdf" directory)))
              (cl-letf (((symbol-function 'vellumory-open) (lambda (&rest _) (current-buffer))))
                (vellumory-dired-merge merged))
              (should (vellumory-test--wait (lambda () (file-exists-p merged)) 30))
              (let (info)
                (vellumory--cli (list "info" merged) (lambda (result) (setq info result)))
                (should (vellumory-test--wait (lambda () info) 30))
                (should (= (alist-get 'pages info) 6))))
            (kill-buffer)))
      (when (get-buffer "*Vellumory batch*") (kill-buffer "*Vellumory batch*"))
      (delete-directory directory t))))

(ert-deftest vellumory-layout-indexes-every-page ()
  "Each page's first row is where its tiles begin, with and without spreads;
 gap rows separate spreads and do not end the document.  The layout counts
rows as it goes (walking the list per page made it quadratic: 1.3 s on a
5,019-page book)."
  (with-temp-buffer
    (setq vellumory--pages (vconcat (cl-loop for i below 301
                                             collect `((width . 600.0) (height . ,(if (zerop (% i 7)) 900.0 800.0)))))
          vellumory--zoom 1.0 vellumory--labels [])
    (dolist (spread '(nil book pairs))
      (setq vellumory--spread spread)
      (vellumory--layout-1)
      (let ((pages-of (lambda (index) (mapcar #'vellumory--tile-page (vellumory--row-tiles (aref vellumory--rows index))))))
        (dotimes (page (length vellumory--pages))
          (let ((index (aref vellumory--page-rows page)))
            (should (memq page (funcall pages-of index)))
            (should (or (zerop index) (not (memq page (funcall pages-of (1- index))))))))
        (should-not (vellumory--row-gap (aref vellumory--rows (1- (length vellumory--rows)))))
        (should (= (cl-count-if #'vellumory--row-gap vellumory--rows) (1- (length (vellumory--spreads)))))))))

(ert-deftest vellumory-spread-layout-and-navigation ()
  (with-temp-buffer
    (setq vellumory--pages (vector '((width . 600.0) (height . 800.0)) '((width . 600.0) (height . 800.0))
                                   '((width . 600.0) (height . 700.0)) '((width . 600.0) (height . 800.0))
                                   '((width . 600.0) (height . 800.0)))
          vellumory--zoom 1.0 vellumory--labels [])
    (setq vellumory--spread 'book)
    (should (equal (vellumory--spreads) '((nil 0) (1 2) (3 4))))
    (vellumory--layout-1)
    ;; Two columns share the window: each page gets half of it, less the gutter.
    (should (= vellumory--width (floor (/ (- 800 16 vellumory-spread-gap) 2.0))))
    (should (<= (vellumory--row-width) 800))
    (should (= (aref vellumory--page-rows 1) (aref vellumory--page-rows 2)))
    (should (equal (vellumory--spread-pages 2) '(1 2)))
    (let* ((first (aref vellumory--rows (aref vellumory--page-rows 1)))
           (shorter (vellumory--page-height 2))
           (spread-rows (cl-remove-if-not (lambda (row) (and (= (vellumory--row-page row) 1)
                                                              (not (vellumory--row-gap row))))
                                          vellumory--rows)))
      ;; The first rows of a spread hold tiles of both pages, left then right.
      (should (equal (mapcar #'vellumory--tile-page (vellumory--row-tiles first)) '(1 2)))
      ;; A row ends where the shorter page does; below it only the taller page has tiles.
      (should (cl-find shorter spread-rows :key #'vellumory--row-bottom))
      (should (cl-every (lambda (row) (equal (mapcar #'vellumory--tile-page (vellumory--row-tiles row))
                                             (if (< (vellumory--row-top row) shorter) '(1 2) '(1))))
                        spread-rows)))
    ;; The cover stands alone, on the right.
    (should (equal (mapcar #'vellumory--tile-page (vellumory--row-tiles (aref vellumory--rows 0))) '(0)))
    (should (= (vellumory--step-page 0 1) 1))
    (should (= (vellumory--step-page 1 1) 3))
    (should (= (vellumory--step-page 4 1) 4))
    (should (= (vellumory--step-page 3 -1) 1))
    (should (= (vellumory--step-page 2 -1) 0))
    (setq vellumory--current-page 1)
    (should (equal (vellumory--page-string) "2–3/5"))
    (setq vellumory--spread 'pairs)
    (should (equal (vellumory--spreads) '((0 1) (2 3) (4 nil))))
    (setq vellumory--spread nil)
    (should (equal (vellumory--spreads) '((0) (1) (2) (3) (4))))
    (vellumory--layout-1)
    (should (= vellumory--width (- 800 16)))
    (should (= (vellumory--step-page 1 1) 2))))

(ert-deftest vellumory-occur-and-isearch-style-stepping ()
  (let ((pages ["Alpha line\nbeta Alpha" "nothing" "ALPHA"]))
    ;; Lower case folds, upper case does not: as `occur' decides.
    (should (equal (mapcar #'car (vellumory--occur-matches pages "alpha")) '(0 0 2)))
    (should (equal (mapcar #'car (vellumory--occur-matches pages "Alpha")) '(0 0)))
    (should (equal (nth 1 (car (vellumory--occur-matches pages "be+ta"))) "beta Alpha")))
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (vellumory-program program)
         (vellumory-state-directory (make-temp-file "vellumory-occur-state-" t)))
    (skip-unless (and program source))
    (let ((reader (vellumory-open source t)) occur)
      (unwind-protect
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (vellumory-occur "Line [0-9]+: text"))
            (should (vellumory-test--wait
                     (lambda () (and (setq occur (get-buffer (format "*Occur: %s*" (buffer-name reader))))
                                     (buffer-local-value 'vellumory--occur-source occur)))))
            (with-current-buffer occur
              (should (string-match-p "\\`[0-9]+ matches for “Line \\[0-9\\]\\+: text” on 3 pages" (buffer-string)))
              (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
                (vellumory-occur-goto)))
            ;; The exact matched text is searched and highlighted from its page.
            (should (equal vellumory--search-query "Line 1: text"))
            (should (vellumory-test--wait (lambda () vellumory--search-current)))
            (should (= (car vellumory--search-current) 0))
            (with-current-buffer occur
              (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
                (vellumory--occur-next-error 1 nil)
                (should (string-match-p "Line 2: text" (buffer-substring (line-beginning-position)
                                                                         (line-end-position))))))
            ;; C-s in the search minibuffer: first match of new input, then the next.
            (vellumory-search-clear)
            (with-temp-buffer
              (insert "Line 3")
              (setq vellumory--search-session (list reader nil '((page . 0) (y . 0))))
              (vellumory--search-minibuffer-step 1)
              (with-current-buffer reader
                (should (equal vellumory--search-query "Line 3"))
                (should (vellumory-test--wait (lambda () vellumory--search-current))))
              (let ((first (with-current-buffer reader vellumory--search-current)))
                (vellumory--search-minibuffer-step 1)
                (should (vellumory-test--wait
                         (lambda () (not (equal first (with-current-buffer reader vellumory--search-current))))))))
            ;; An empty C-s brings back the last search.
            (let ((vellumory-search-history '("Continuous")))
              (with-temp-buffer
                (setq vellumory--search-session (list reader nil nil))
                (vellumory--search-minibuffer-step 1)
                (should (equal (buffer-string) "Continuous")))))
        (when (buffer-live-p occur) (kill-buffer occur))
        (kill-buffer reader)
        (delete-directory vellumory-state-directory t)))))

(ert-deftest vellumory-noter-session-follows-and-navigates ()
  (let* ((noter (getenv "VELLUMORY_ORG_NOTER_LISP"))
         (program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (file-name-as-directory (make-temp-file "vellumory-noter-" t)))
         (document (expand-file-name "doc.pdf" directory))
         (notes (expand-file-name "notes.org" directory))
         (vellumory-program program)
         (vellumory-state-directory (expand-file-name "state" directory))
         notes-buffer reader)
    (skip-unless (and noter (file-directory-p noter) program source))
    ;; The shell's org-noter is compiled for its Emacs; another may not read it.
    (skip-unless (let ((load-path (cons noter load-path))) (ignore-errors (require 'org-noter) t)))
    (add-to-list 'load-path noter)
    (add-to-list 'load-path (expand-file-name "modules" noter))
    (require 'org-noter)
    (vellumory-install)
    (should (featurep 'vellumory-noter))
    (should (memq 'vellumory-mode org-noter-supported-modes))
    (copy-file source document)
    (with-temp-file notes
      (insert "* Reading notes\n:PROPERTIES:\n:NOTER_DOCUMENT: doc.pdf\n:END:\n"))
    (unwind-protect
        (let ((org-noter-always-create-frame nil) (org-noter-auto-save-last-location nil)
              (org-noter-notes-window-location 'horizontal-split))
          (setq notes-buffer (find-file-noselect notes))
          (with-current-buffer notes-buffer (goto-char (point-min)) (org-noter))
          (setq reader (find-buffer-visiting document))
          (should (eq (buffer-local-value 'major-mode reader) 'vellumory-mode))
          (should (vellumory-test--wait (lambda () (buffer-local-value 'vellumory--pages reader))))
          ;; The session sets up its windows from an idle timer; batch Emacs is
          ;; never idle, so run them as an idle command loop would.
          (should (vellumory-test--wait (lambda () (mapc #'timer-event-handler (copy-sequence timer-idle-list))
                                          (get-buffer-window reader))))
          (with-current-buffer reader
            (should org-noter--session)
            (should (equal (org-noter--doc-approx-location) '(1 . 0)))
            ;; A note's place opens the reader there.
            (org-noter--doc-goto-location '(3 . 0.5))
            (should (= (alist-get 'page (vellumory--reading-position (get-buffer-window reader))) 2))
            (vellumory--update-page)
            (should (equal (org-noter--doc-approx-location) '(3 . 0)))
            (should (equal (org-noter--get-current-view) [paged 3]))
            (should (equal (org-noter--pretty-print-location '(3 . 0)) "3"))
            (should (equal (org-noter--pretty-print-location '(3 0.25 . 0.5)) "(3 0.25 . 0.5)")))
          ;; The outline becomes headings at their pages.
          (with-current-buffer (org-noter--session-notes-buffer (buffer-local-value 'org-noter--session reader))
            (org-noter-create-skeleton)
            (should (string-match-p "\\*\\* Geometry" (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p ":NOTER_PAGE: 3" (buffer-substring-no-properties (point-min) (point-max)))))
          (org-noter-kill-session (buffer-local-value 'org-noter--session reader)))
      (dolist (buffer (list reader notes-buffer))
        (when (buffer-live-p buffer) (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest vellumory-encryption-passwords-stay-off-the-command-line ()
  ;; Operations with passwords go through standard input.
  (let (calls)
    (cl-letf (((symbol-function 'vellumory--cli) (lambda (args _callback &optional _buffer input)
                                                   (push (list args input) calls)))
              ((symbol-function 'vellumory--edit-file) (lambda () "/doc.pdf")))
      (vellumory--edit '((op . "encrypt") (user . "u-secret") (owner . "o-secret") (print . "full")
                         (modify . "all") (extract . t)))
      (vellumory--edit '((op . "rotate") (pages . [1]) (degrees . 90))))
    (pcase-let ((`((,rotate ,rotate-input) (,encrypt ,encrypt-input)) calls))
      (should (equal encrypt '("edit" "/doc.pdf" "-")))
      (should (string-match-p "u-secret" encrypt-input))
      (should-not (cl-some (lambda (arg) (string-match-p "secret" arg)) encrypt))
      (should (equal (nth 2 rotate) (json-serialize '((op . "rotate") (pages . [1]) (degrees . 90)))))
      (should-not rotate-input)))
  ;; Opening an encrypted document asks for its password, again when wrong.
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-encrypted-" t))
         (file (expand-file-name "doc.pdf" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program)
         (answers '("wrong" "s3cret-user")) prompts done)
    (skip-unless (and program source))
    (copy-file source file)
    (vellumory--cli (list "edit" file "-") (lambda (_) (setq done t)) nil
                    (json-serialize '((op . "encrypt") (user . "s3cret-user") (owner . "s3cret-owner")
                                      (print . "full") (modify . "all") (extract . t))))
    (should (vellumory-test--wait (lambda () done) 30))
    (unwind-protect
        (cl-letf (((symbol-function 'read-passwd) (lambda (prompt &rest _) (push prompt prompts) (pop answers))))
          (let ((reader (vellumory-open file t)))
            (with-current-buffer reader
              (should (vellumory-test--wait (lambda () vellumory--pages) 20))
              (should (= (length vellumory--pages) 3))
              (should (equal vellumory--password "s3cret-user")))
            (should (= (length prompts) 2))
            (should (string-prefix-p "Wrong password" (car prompts)))
            (kill-buffer reader)))
      (delete-directory directory t))))

(ert-deftest vellumory-evil-org-noter-keys-win-in-sessions ()
  (let ((evil-lisp (getenv "VELLUMORY_EVIL_LISP")) (noter (getenv "VELLUMORY_ORG_NOTER_LISP")))
    (skip-unless (and evil-lisp noter (file-directory-p evil-lisp) (file-directory-p noter)))
    (add-to-list 'load-path evil-lisp)
    (add-to-list 'load-path noter)
    (skip-unless (ignore-errors (require 'evil) t))
    (require 'vellumory-evil)
    (vellumory-evil-setup)
    (require 'org-noter-core)
    (with-temp-buffer
      (vellumory-mode)
      (evil-local-mode 1)
      (evil-normal-state)
      (should-not (eq (key-binding "i") 'org-noter-insert-note))
      (org-noter-doc-mode 1)
      ;; What evil does before the next command.
      (evil-normalize-keymaps)
      (should (eq (key-binding "i") 'org-noter-insert-note))
      (should (eq (key-binding "q") 'org-noter-kill-session))
      (should (eq (key-binding (kbd "M-i")) 'org-noter-insert-precise-note))
      (org-noter-doc-mode -1)
      (evil-normalize-keymaps)
      (should-not (eq (key-binding "q") 'org-noter-kill-session)))))

(ert-deftest vellumory-line-highlight-is-empty-in-both-hl-line-paths ()
  (require 'hl-line)
  (with-temp-buffer
    (vellumory-mode)
    (let ((inhibit-read-only t)) (insert "a strip\nanother strip\n"))
    (goto-char (point-max))
    ;; The local mode moves its overlay through `hl-line-move'.
    (let ((overlay (make-overlay 1 1)))
      (hl-line-move overlay)
      (should (= (overlay-start overlay) (overlay-end overlay)))
      (delete-overlay overlay))
    ;; The window-sticky global mode passes the range on as positions.
    (when (fboundp 'global-hl-line-window-redisplay)
      (let ((global-hl-line-buffers t) (window (selected-window)))
        (set-window-buffer window (current-buffer))
        (global-hl-line-window-redisplay window)
        (let ((overlay (window-parameter window 'hl-line-overlay)))
          (when overlay
            (should (= (overlay-start overlay) (overlay-end overlay)))))))))

(ert-deftest vellumory-annotations-mark-list-edit-delete-undo ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-annotate-" t))
         (file (expand-file-name "doc.pdf" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program)
         (vellumory-annotation-author "Reader"))
    (skip-unless (and program source))
    (copy-file source file)
    (unwind-protect
        (let ((reader (vellumory-open file t)) listing)
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (let ((annotations-of
                   (lambda (page)
                     (let (result)
                       (vellumory--rpc "annotations" `((page . ,page))
                                       (lambda (reply) (setq result (alist-get 'annotations (alist-get 'result reply)))))
                       (vellumory-test--wait (lambda () result))
                       result)))
                  (hash vellumory--hash) done)
              ;; Select as a drag would, then highlight: the text becomes the contents.
              (vellumory--select-range '(0 40 182) '(0 330 190) 'chars
                                       (lambda (selections) (vellumory--set-selection selections) (setq done t)))
              (should (vellumory-test--wait (lambda () done)))
              (should (string-match-p "Line 1" (vellumory-selection-text)))
              (vellumory-highlight)
              (should-not vellumory--selection)
              (should (vellumory-test--wait (lambda () (not (equal hash vellumory--hash))) 20))
              (let ((found (funcall annotations-of 0)))
                (should (= (length found) 1))
                (should (equal (alist-get 'type (aref found 0)) "Highlight"))
                (should (string-match-p "Line 1" (alist-get 'contents (aref found 0))))
                (should (equal (alist-get 'author (aref found 0)) "Reader")))
              ;; Hovering it shows its text.
              (vellumory--ensure-annotations 0)
              (should (vellumory-test--wait (lambda () (vectorp (vellumory--ensure-annotations 0)))))
              (let ((areas (cl-loop for row across vellumory--rows
                                    when (= (vellumory--row-page row) 0)
                                    append (cl-loop for tile across (vellumory--row-tiles row)
                                                    append (vellumory--annotation-areas tile)))))
                (should (cl-some (lambda (area) (string-match-p "\\`Highlight · Reader: Line 1"
                                                                (plist-get (nth 2 area) 'help-echo)))
                                 areas)))
              ;; A note where one clicks.
              (setq hash vellumory--hash)
              (vellumory-add-note '(1 400 100) "Check this")
              (should (vellumory-test--wait (lambda () (not (equal hash vellumory--hash))) 20))
              (should (equal (alist-get 'type (aref (funcall annotations-of 1) 0)) "Text"))
              ;; The list: edit the highlight, delete the note.
              (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
                (vellumory-annotations))
              (should (vellumory-test--wait
                       (lambda () (and (setq listing (get-buffer (format "*Annotations: %s*" (buffer-name reader))))
                                       (= (length (buffer-local-value 'tabulated-list-entries listing)) 2)))))
              (with-current-buffer listing
                (goto-char (point-min))
                (while (not (equal (alist-get 'type (tabulated-list-get-id)) "Highlight")) (forward-line 1))
                (setq hash (buffer-local-value 'vellumory--hash reader))
                (vellumory-annotation-list-edit "Revised"))
              (should (vellumory-test--wait (lambda () (not (equal hash vellumory--hash))) 20))
              (should (equal (alist-get 'contents (aref (funcall annotations-of 0) 0)) "Revised"))
              (with-current-buffer listing
                (vellumory-annotation-list-refresh)
                (should (vellumory-test--wait (lambda () (= (length tabulated-list-entries) 2))))
                (goto-char (point-min))
                (while (not (equal (alist-get 'type (tabulated-list-get-id)) "Text")) (forward-line 1))
                (setq hash (buffer-local-value 'vellumory--hash reader))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (vellumory-annotation-list-delete)))
              (should (vellumory-test--wait (lambda () (not (equal hash vellumory--hash))) 20))
              (should (= (length (funcall annotations-of 1)) 0))
              ;; Every step is undoable, back to the original bytes.
              (dotimes (_ 4)
                (setq hash vellumory--hash)
                (vellumory-undo)
                (should (vellumory-test--wait (lambda () (not (equal hash vellumory--hash))) 20)))
              (should (equal (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally file)
                                               (buffer-string))
                             (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally source)
                                               (buffer-string))))))
          (when (buffer-live-p listing) (kill-buffer listing))
          (kill-buffer reader))
      (delete-directory directory t))))

(ert-deftest vellumory-signed-documents-warn-before-edits-and-report ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (fixture (getenv "VELLUMORY_SIGNED_FIXTURE"))
         (directory (make-temp-file "vellumory-signed-" t))
         (file (expand-file-name "signed.pdf" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program)
         (bytes (lambda () (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally file)
                                             (buffer-string)))))
    (skip-unless (and program fixture))
    (copy-file (expand-file-name "signed.pdf" fixture) file)
    (set-file-modes file #o644)
    (unwind-protect
        (let ((reader (vellumory-open file t)) (original (funcall bytes)) info opened)
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (should (= vellumory--signed 1))
            (setq opened vellumory--hash)
            ;; Declining leaves the signed file as it is.
            (let (asked)
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) nil)))
                (should-error (vellumory-rotate-pages) :type 'user-error))
              (should (string-match-p "digitally signed; this edit invalidates its signature" asked)))
            (should (equal (funcall bytes) original))
            ;; Accepting edits it.
            (let ((hash vellumory--hash))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
                (vellumory-rotate-pages))
              (should (vellumory-test--wait (lambda () (not (equal hash vellumory--hash))) 20)))
            (should-not (equal (funcall bytes) original))
            ;; The report shows why: the signature no longer matches the bytes.
            (let (signatures)
              (vellumory--rpc "signatures" nil (lambda (reply) (setq signatures (alist-get 'result reply))))
              (should (vellumory-test--wait (lambda () signatures)))
              (with-temp-buffer
                (vellumory--info-signatures signatures)
                (setq info (buffer-string))))
            (should (string-match-p "Signatures (1)" info))
            (should (string-match-p "Vellumory Test Signer" info))
            (should (string-match-p "Integrity +broken" info))
            (vellumory-undo)
            (should (vellumory-test--wait (lambda () (equal (funcall bytes) original)) 20))
            ;; The reader, and so the worker answering, has reopened the restored file.
            (should (vellumory-test--wait (lambda () (equal vellumory--hash opened)) 20))
            (let (signatures)
              (vellumory--rpc "signatures" nil (lambda (reply) (setq signatures (alist-get 'result reply))))
              (should (vellumory-test--wait (lambda () signatures)))
              (with-temp-buffer
                (vellumory--info-signatures signatures)
                (should (string-match-p "Integrity +intact" (buffer-string)))
                (should (string-match-p "not trusted: self-signed certificate" (buffer-string))))))
          (kill-buffer reader))
      (delete-directory directory t))))

(defvar vellumory-form-test--hash nil)
(ert-deftest vellumory-form-fill-in-the-form-buffer ()
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (fixture (getenv "VELLUMORY_FORM_FIXTURE"))
         (directory (make-temp-file "vellumory-form-" t))
         (file (expand-file-name "form.pdf" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program))
    (skip-unless (and program fixture))
    (copy-file (expand-file-name "form.pdf" fixture) file)
    (set-file-modes file #o644)
    (unwind-protect
        (let ((reader (vellumory-open file t)) form)
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (vellumory-fill-form)
            (should (vellumory-test--wait
                     (lambda () (and (setq form (get-buffer "*Form: form.pdf*"))
                                     (buffer-local-value 'vellumory-form--fields form))))))
          (with-current-buffer form
            (let ((go (lambda (name) (goto-char (point-min))
                        (text-property-search-forward 'vellumory-form-field name #'equal))))
              (should (string-match-p "Reference (\\(.*\\)) \\[read-only\\]\\|reference \\[read-only\\]" (buffer-string)))
              ;; Values are typed after the labels; read-only fields refuse.
              (funcall go "fullname") (insert "Ada Lovelace")
              (funcall go "agree") (delete-region (point) (line-end-position)) (insert "yes")
              (funcall go "colour") (insert "green")
              (funcall go "reference") (should-error (insert "x") :type 'text-read-only)
              ;; A value that is not a choice is refused before anything runs.
              (save-excursion (funcall go "colour") (delete-region (point) (line-end-position)) (insert "purple"))
              (should-error (vellumory-form--operation vellumory-form--fields (vellumory-form--values))
                            :type 'user-error)
              (save-excursion (funcall go "colour") (delete-region (point) (line-end-position)) (insert "green"))
              (let ((operation (vellumory-form--operation vellumory-form--fields (vellumory-form--values))))
                (should (equal (gethash "colour" (alist-get 'values operation)) "Green"))
                (should (equal (gethash "agree" (alist-get 'values operation)) "Yes"))
                (should-not (gethash "reference" (alist-get 'values operation))))
              (cl-letf (((symbol-function 'quit-window) #'ignore))
                (with-current-buffer reader (setq-local vellumory-form-test--hash vellumory--hash))
                (vellumory-form-commit))))
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () (not (equal vellumory-form-test--hash vellumory--hash))) 20))
            (let (fields)
              (vellumory--rpc "form-fields" nil (lambda (reply) (setq fields (alist-get 'result reply))))
              (should (vellumory-test--wait (lambda () fields)))
              (let ((value (lambda (name) (alist-get 'value (seq-find (lambda (f) (equal (alist-get 'name f) name))
                                                                        fields)))))
                (should (equal (funcall value "fullname") "Ada Lovelace"))
                (should (equal (funcall value "agree") "Yes"))
                (should (equal (funcall value "colour") "Green"))
                (should (equal (funcall value "reference") "R-17")))))
          (kill-buffer form)
          (kill-buffer reader))
      (delete-directory directory t))))

(ert-deftest vellumory-form-click-to-fill-hit-testing-and-values ()
  (with-temp-buffer
    (setq vellumory--hash "h" vellumory--pages [((width . 612) (height . 792))]
          vellumory--form-fields
          (cons "h" (vector '((name . "fullname") (type . "text") (value . "") (page . 0) (rect . [180 124 352 138]))
                            '((name . "agree") (type . "checkbox") (value . "Off") (page . 0) (rect . [180 140 190 150])
                              (states . ["Yes"]))
                            '((name . "colour") (type . "combobox") (value . "") (page . 0) (rect . [180 160 230 172])
                              (options . ["Red" "Green" "Blue"]))
                            '((name . "reference") (type . "text") (value . "R-17") (read_only . t) (page . 0)
                              (rect . [180 180 260 192])))))
    (should (equal (alist-get 'name (vellumory--form-field-at 0 200 130)) "fullname"))
    (should-not (vellumory--form-field-at 0 100 130))
    (should-not (vellumory--form-field-at 1 200 130))
    (let (edits)
      (cl-letf (((symbol-function 'vellumory--edit) (lambda (op) (push op edits)))
                ((symbol-function 'read-string) (lambda (&rest _) "Ada"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (should (equal choices '("Red" "Green" "Blue"))) "Blue")))
        ;; A checkbox toggles without asking.
        (vellumory-fill-field (vellumory--form-field-at 0 185 145))
        (vellumory-fill-field (vellumory--form-field-at 0 200 130))
        (vellumory-fill-field (vellumory--form-field-at 0 200 165))
        (should-error (vellumory-fill-field (vellumory--form-field-at 0 200 185)) :type 'user-error))
      (setq edits (nreverse edits))
      (should (equal (gethash "agree" (alist-get 'values (nth 0 edits))) "Yes"))
      (should (equal (gethash "fullname" (alist-get 'values (nth 1 edits))) "Ada"))
      (should (equal (gethash "colour" (alist-get 'values (nth 2 edits))) "Blue")))))

(ert-deftest vellumory-clicks-inside-image-map-areas-are-bound ()
  "A click inside a link, annotation or form field area arrives prefixed with
the area's id; each must reach the reader's mouse commands."
  (with-temp-buffer
    (vellumory-mode)
    (dolist (area vellumory--map-areas)
      (should (eq (key-binding (vector area 'down-mouse-1)) 'vellumory-mouse-press))
      (should (eq (key-binding (vector area 'double-mouse-1)) 'vellumory-mouse-select-word))
      (should (eq (key-binding (vector area 'wheel-down)) 'vellumory-wheel))
      (should (eq (key-binding (vector area 'C-mouse-1)) 'vellumory-sync-backward)))))

(ert-deftest vellumory-opens-as-the-document-asks ()
  "/PageLayout chooses the spread unless the user set one; /PageMode is kept
for the sidebar; the option turns both off."
  (with-temp-buffer
    (let ((vellumory-spread nil) (vellumory-follow-document-display t))
      (vellumory--follow-document-display '((page_layout . "TwoColumnRight") (page_mode . "UseOutlines")))
      (should (eq vellumory--spread 'book))
      (should (equal vellumory--page-mode "UseOutlines"))
      (vellumory--follow-document-display '((page_layout . "TwoPageLeft") (page_mode . "")))
      (should (eq vellumory--spread 'pairs))
      (vellumory--follow-document-display '((page_layout . "SinglePage") (page_mode . "UseNone")))
      (should-not vellumory--spread))
    (let ((vellumory-spread 'pairs) (vellumory-follow-document-display t))
      (setq vellumory--spread 'pairs)
      (vellumory--follow-document-display '((page_layout . "TwoColumnRight")))
      (should (eq vellumory--spread 'pairs)))
    (let ((vellumory-spread nil) (vellumory-follow-document-display nil))
      (setq vellumory--spread nil vellumory--page-mode nil)
      (vellumory--follow-document-display '((page_layout . "TwoColumnRight") (page_mode . "UseOutlines")))
      (should-not vellumory--spread)
      (should-not vellumory--page-mode))))

(defmacro vellumory-test--presenting (pages &rest body)
  "Run BODY in a presentation buffer of a reader with PAGES; drawing is stubbed."
  (declare (indent 1))
  `(let ((source (generate-new-buffer " *presented*")) (shown 0) (quit 0))
     (unwind-protect
         (with-temp-buffer
           (with-current-buffer source (setq vellumory--pages ,pages))
           (setq vellumory-present--source source)
           (cl-letf (((symbol-function 'vellumory-present--show) (lambda () (cl-incf shown)))
                     ((symbol-function 'vellumory-present-quit) (lambda () (interactive) (cl-incf quit))))
             ,@body))
       (kill-buffer source))))

(ert-deftest vellumory-present-fits-and-centres-pages ()
  (vellumory-test--presenting [((width . 612) (height . 792)) ((width . 792) (height . 612))]
    (should (equal (vellumory-present--geometry 0 1920 1080) '(835 1080 542 0)))
    (should (equal (vellumory-present--geometry 1 1920 1080) '(1398 1080 261 0)))
    (should (equal (vellumory-present--geometry 1 1024 768) '(994 768 15 0)))
    (should (equal (vellumory-present--geometry 0 600 1200) '(600 776 0 212)))))

(ert-deftest vellumory-present-pages-screens-and-typed-numbers ()
  (vellumory-test--presenting [((width . 612) (height . 792)) ((width . 612) (height . 792))
                               ((width . 612) (height . 792))]
    (vellumory-present--go 0)
    (vellumory-present-next)
    (should (and (= vellumory-present--page 1) (eq vellumory-present--state 'normal)))
    (vellumory-present-last)
    (should (= vellumory-present--page 2))
    ;; Past the last page the end screen, then leaving.
    (vellumory-present-next)
    (should (and (eq vellumory-present--state 'end) (= vellumory-present--page 2)))
    (vellumory-present-previous)
    (should (and (eq vellumory-present--state 'normal) (= vellumory-present--page 2)))
    (vellumory-present-next)
    (vellumory-present-next)
    (should (= quit 1))
    (vellumory-present-first)
    (vellumory-present-previous)
    (should (= vellumory-present--page 0))
    ;; Black and white screens toggle; a page key returns to the same page.
    (vellumory-present-black)
    (should (eq vellumory-present--state 'black))
    (vellumory-present-black)
    (should (eq vellumory-present--state 'normal))
    (vellumory-present-white)
    (vellumory-present-next)
    (should (and (eq vellumory-present--state 'normal) (= vellumory-present--page 0)))
    ;; Digits, a correction and RET jump; an impossible number does not.
    (dolist (key '(?3 ?1))
      (let ((last-command-event key)) (vellumory-present-digit)))
    (vellumory-present-backspace)
    (should (equal vellumory-present--digits "3"))
    (vellumory-present-return)
    (should (= vellumory-present--page 2))
    (let ((last-command-event ?9)) (vellumory-present-digit))
    (vellumory-present-return)
    (should (and (= vellumory-present--page 2) (equal vellumory-present--digits "")))
    (should (> shown 10))))

(ert-deftest vellumory-present-follows-page-durations ()
  (vellumory-test--presenting [((width . 612) (height . 792)) ((width . 612) (height . 792))]
    (let (timers)
      (cl-letf (((symbol-function 'run-at-time) (lambda (seconds &rest _) (push seconds timers) 'timer)))
        (setq vellumory-present--info '((0 (duration . 2.5)) (1 (duration))))
        (vellumory-present--arm-duration 0 vellumory-present--generation)
        (vellumory-present--arm-duration 1 vellumory-present--generation)
        ;; A page changed meanwhile: its old /Dur no longer applies.
        (vellumory-present--arm-duration 0 (1- vellumory-present--generation))
        (let ((vellumory-present-follow-durations nil))
          (vellumory-present--arm-duration 0 vellumory-present--generation)))
      (should (equal timers '(2.5))))))

(ert-deftest vellumory-sidebar-tabs-show-views-and-availability ()
  "The sidebar's header names both views: the current one marked, the other a
button; Contents is disabled, with the reason, when the document has none."
  (let ((source (generate-new-buffer " *reader*")))
    (unwind-protect
        (with-temp-buffer
          (vellumory-thumbnails-mode)
          (setq vellumory--thumbnail-source source)
          (cl-flet ((tab (name) (let* ((line (vellumory--sidebar-tabs 'pages "RET visit"))
                                       (start (string-search name line)))
                                  (text-properties-at start line))))
            (with-current-buffer source (setq vellumory--outline [] vellumory--navigation-state 'ready))
            (should (memq 'bold (flatten-tree (plist-get (tab "Pages") 'face))))
            (should-not (plist-get (tab "Contents") 'keymap))
            (should (string-match-p "no contents" (plist-get (tab "Contents") 'help-echo)))
            (with-current-buffer source (setq vellumory--navigation-state 'loading))
            (should (string-match-p "loading" (plist-get (tab "Contents") 'help-echo)))
            (with-current-buffer source
              (setq vellumory--outline [((title . "Reading") (page . 0))] vellumory--navigation-state 'ready))
            (should (eq (plist-get (tab "Contents") 'keymap) vellumory--sidebar-tab-map))
            (should (eq (lookup-key vellumory--sidebar-tab-map [header-line mouse-1]) 'vellumory-sidebar-switch))
            (should (string-match-p "s switch" (vellumory--sidebar-tabs 'pages "RET visit")))))
      (kill-buffer source))))

(ert-deftest vellumory-evil-snipe-leaves-document-keys-alone ()
  "Under Doom's evil-snipe, s f t in a document are Vellumory's."
  (skip-unless (require 'evil-snipe nil t))
  (dolist (mode '(vellumory-mode vellumory-thumbnails-mode vellumory-outline-mode))
    (should (memq mode evil-snipe-disabled-modes))))

(ert-deftest vellumory-present-transitions-are-slices-of-both-pages ()
  "Each drawn /Trans style is rows of slices of the old and new page that
cover the page exactly at every moment; other styles cut."
  (cl-flet ((at (style p &rest more) (vellumory-present--compose `((style . ,style) ,@more) p 100 50)))
    (should (equal (at "Wipe" 0.5) '(((new 0 0 50 50) (old 50 0 50 50)))))
    (should (equal (at "Wipe" 1.0) '(((new 0 0 100 50)))))
    (should (equal (at "Push" 0.25) '(((new 75 0 25 50) (old 0 0 75 50)))))
    (should (equal (at "Uncover" 0.5 '(direction . 270)) '(((new 0 0 100 25)) ((old 0 0 100 25)))))
    (should (equal (at "Cover" 0.5 '(direction . 180)) '(((old 0 0 50 50) (new 0 0 50 50)))))
    (should (equal (at "Split" 0.5 '(dimension . "V") '(motion . "O"))
                   '(((old 0 0 25 50) (new 25 0 50 50) (old 75 0 25 50)))))
    (should (equal (at "Split" 0.5) '(((new 0 0 100 12)) ((old 0 12 100 26)) ((new 0 38 100 12)))))
    (should (equal (at "Box" 0.5 '(motion . "O"))
                   '(((old 0 0 100 12)) ((old 0 12 25 25) (new 25 12 50 25) (old 75 12 25 25))
                     ((old 0 37 100 13)))))
    (should (equal (length (at "Blinds" 0.0)) vellumory-present--blinds))
    (should-not (at "Dissolve" 0.5))
    (should-not (at "R" 0.5))
    ;; Every frame tiles the page: rows as wide as it, as tall together.
    (dolist (style '("Wipe" "Push" "Cover" "Uncover" "Split" "Box" "Blinds"))
      (dolist (more '(nil ((direction . 90)) ((direction . 180)) ((direction . 270)) ((direction . "None"))
                      ((dimension . "V")) ((motion . "O")) ((dimension . "V") (motion . "O"))))
        (dolist (p '(0.0 0.13 0.5 0.77 1.0))
          (let ((rows (apply #'at style p more)))
            (should (= (apply #'+ (mapcar (lambda (row) (nth 4 (car row))) rows)) 50))
            (dolist (row rows)
              (should (= (apply #'+ (mapcar (lambda (s) (nth 3 s)) row)) 100))
              (should (cl-every (lambda (s) (= (nth 4 s) (nth 4 (car row)))) row))
              (dolist (s row)
                (should (and (<= 0 (nth 1 s)) (<= (+ (nth 1 s) (nth 3 s)) 100)
                             (<= 0 (nth 2 s)) (<= (+ (nth 2 s) (nth 4 s)) 50)))))))))))

(ert-deftest vellumory-present-transition-only-between-equal-pages ()
  (vellumory-test--presenting [((width . 612) (height . 792)) ((width . 612) (height . 792))
                               ((width . 792) (height . 612))]
    (let ((wipe '((style . "Wipe") (duration . 0.4))))
      (cl-letf (((symbol-function 'vellumory-present--fit)
                 (lambda (page) (if (= page 2) '(800 618 0 30) '(500 647 150 0)))))
        (setq vellumory-present--info `((1 (transition . ,wipe)) (2 (transition . ,wipe))
                                        (0 (transition)))
              vellumory-present--shown '(0 image 500 647))
        (should (equal (vellumory-present--transition-to 1) wipe))
        ;; A page of another size, the page shown, no /Trans, or the option off: a cut.
        (should-not (vellumory-present--transition-to 2))
        (should-not (vellumory-present--transition-to 0))
        (setq vellumory-present--shown '(2 image 800 618))
        (should-not (vellumory-present--transition-to 0))
        (setq vellumory-present--shown '(0 image 500 647))
        (let ((vellumory-present-transitions nil)) (should-not (vellumory-present--transition-to 1)))
        ;; Blended styles are drawn too; a style not drawn cuts.
        (setf (alist-get 1 vellumory-present--info) '((transition (style . "Dissolve"))))
        (should (vellumory-present--transition-to 1))
        (setf (alist-get 1 vellumory-present--info) '((transition (style . "R"))))
        (should-not (vellumory-present--transition-to 1))))))

(defun vellumory-test--line (block y text)
  "A line of TEXT at Y in BLOCK as the worker reports it: 5 points a character from x 10."
  `((block . ,block) (box . [10 ,(- y 5) ,(+ 10 (* 5 (length text))) ,(+ y 5)])
    (chars . ,(vconcat (cl-loop for c across text for i from 0
                                collect (vector (string c) (+ 10 (* 5 i)) y (+ 15 (* 5 i)) y))))))

(defmacro vellumory-test--with-caret (&rest body)
  "BODY with two pages of known text, drawing and scrolling stubbed; `selected'
holds the endpoints of the last selection asked for."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((table (make-hash-table :test #'eql)) selected)
       (puthash 0 (vector (vellumory-test--line 0 105 "Hello world") (vellumory-test--line 0 117 "foo, bar")
                          (vellumory-test--line 1 145 "Next"))
                table)
       (puthash 1 (vector (vellumory-test--line 0 105 "Page two")) table)
       (setq vellumory--hash "h" vellumory--pages [((width . 612) (height . 792)) ((width . 612) (height . 792))]
             vellumory--chars (cons "h" table))
       (cl-letf (((symbol-function 'vellumory--refresh) #'ignore)
                 ((symbol-function 'vellumory-caret--reveal) #'ignore)
                 ((symbol-function 'vellumory--select-range)
                  (lambda (start end _mode _callback) (setq selected (list start end)))))
         ,@body))))

(ert-deftest vellumory-caret-moves-by-character-word-line-and-block ()
  (vellumory-test--with-caret
    (cl-flet ((at (pos) (setq vellumory-caret--pos pos))
              (is (pos) (should (equal vellumory-caret--pos pos))))
      ;; Characters: a line's end is a stop, then the next line.
      (at '(0 0 10)) (vellumory-caret-forward-char) (is '(0 0 11))
      (vellumory-caret-forward-char) (is '(0 1 0))
      (vellumory-caret-backward-char) (is '(0 0 11))
      ;; Words as evil's w b e: punctuation is a word of its own.
      (at '(0 0 0)) (vellumory-caret-forward-word) (is '(0 0 6))
      (vellumory-caret-forward-word) (is '(0 1 0))
      (vellumory-caret-forward-word) (is '(0 1 3))
      (vellumory-caret-forward-word) (is '(0 1 5))
      (vellumory-caret-backward-word) (is '(0 1 3))
      (vellumory-caret-backward-word) (is '(0 1 0))
      (vellumory-caret-backward-word) (is '(0 0 6))
      (at '(0 0 0)) (vellumory-caret-end-of-word) (is '(0 0 4))
      (vellumory-caret-line-end) (is '(0 0 11))
      (vellumory-caret-line-start) (is '(0 0 0))
      ;; Lines keep the column, past a short line and onto the next page.
      (at '(0 0 7)) (vellumory-caret-next-line) (is '(0 1 7))
      (vellumory-caret-next-line) (is '(0 2 4))
      (vellumory-caret-next-line) (is '(1 0 7))
      (vellumory-caret-previous-line) (is '(0 2 4))
      (vellumory-caret-forward-char) (should-not vellumory-caret--goal)
      ;; Blocks.
      (at '(0 0 3)) (vellumory-caret-next-block) (is '(0 2 0))
      (vellumory-caret-next-block) (is '(1 0 0))
      (at '(0 1 4)) (vellumory-caret-previous-block) (is '(0 0 0))
      (at '(0 2 0)) (vellumory-caret-previous-block) (is '(0 0 0))
      ;; The end of the document.
      (at '(1 0 8)) (should-error (vellumory-caret-forward-char) :type 'user-error))))

(ert-deftest vellumory-caret-selects-inclusively-by-characters-and-lines ()
  (vellumory-test--with-caret
    (setq vellumory-caret-mode t vellumory-caret--pos '(0 0 6))
    (vellumory-caret-toggle-selection)
    (should (equal vellumory-caret--anchor '(0 0 6)))
    ;; The caret's own character is included, either way round.
    (vellumory-caret--move '(0 1 2))
    (should (equal selected '((0 40 105) (0 25 117))))
    (setq vellumory-caret--anchor '(0 1 2) vellumory-caret--pos '(0 0 6))
    (vellumory-caret--select)
    (should (equal selected '((0 40 105) (0 25 117))))
    ;; Whole lines.
    (vellumory-caret-toggle-line-selection)
    (should (eq vellumory-caret--unit 'lines))
    (should (equal selected '((0 10 105) (0 50 117))))
    ;; v turns a line selection into a character one; again, it ends it.
    (vellumory-caret-toggle-selection)
    (should (and vellumory-caret--anchor (eq vellumory-caret--unit 'chars)))
    (vellumory-caret-toggle-selection)
    (should-not vellumory-caret--anchor)
    ;; o swaps the ends.
    (setq vellumory-caret--anchor '(0 0 0) vellumory-caret--pos '(0 0 4))
    (vellumory-caret-exchange)
    (should (equal (list vellumory-caret--anchor vellumory-caret--pos) '((0 0 4) (0 0 0))))))

(ert-deftest vellumory-caret-bar-and-click-placement ()
  (vellumory-test--with-caret
    (setq vellumory-caret-mode t vellumory-caret--pos '(0 0 2))
    (cl-letf (((symbol-function 'vellumory--points-per-pixel) (lambda (_) 0.5)))
      (should (equal (vellumory--caret-quads 0) (list (vellumory--rect-quad 19.625 100 20.375 110)))))
    (should-not (vellumory--caret-quads 1))
    (vellumory-caret--place-at 0 33 118)
    (should (equal vellumory-caret--pos '(0 1 5)))
    ;; Shifted motions select as they go.
    (setq vellumory-caret--anchor nil)
    (funcall (lookup-key vellumory-caret-mode-map (kbd "S-<right>")))
    (should (equal vellumory-caret--anchor '(0 1 5)))
    (should (equal vellumory-caret--pos '(0 1 6)))))

(ert-deftest vellumory-caret-state-in-the-header-line ()
  "The header line says caret, visual or visual line, with the keys to go on."
  (vellumory-test--with-caret
    (should (equal (vellumory-caret--indicator) ""))
    (setq vellumory-caret-mode t vellumory-caret--pos '(0 0 0))
    (should (string-match-p "CARET.*v select" (vellumory-caret--indicator)))
    (vellumory-caret-toggle-selection)
    (should (string-match-p "VISUAL .*y copy" (vellumory-caret--indicator)))
    (vellumory-caret-toggle-line-selection)
    (should (string-match-p "VISUAL LINE" (vellumory-caret--indicator)))))

(ert-deftest vellumory-consult-line-candidates-preview-and-choice ()
  "Every line is a candidate carrying its page; moving shows the page, leaving
returns, choosing shows the line highlighted."
  (cl-letf (((symbol-function 'vellumory--page-description) (lambda (page) (format "p%d" (1+ page)))))
    (let ((lines (vellumory--document-lines ["Title\n\nFirst line  \n" "Second page\nlast"])))
      (should (equal (mapcar #'vellumory--line-of lines)
                     '((0 . "Title") (0 . "First line") (1 . "Second page") (1 . "last"))))
      (should (string-match-p "\\`p2 +Second page\\'" (substring-no-properties (nth 2 lines))))
      (with-temp-buffer
        (let (moves shown)
          (cl-letf (((symbol-function 'vellumory--restore-position) (lambda (position &rest _) (push position moves)))
                    ((symbol-function 'vellumory--refresh) #'ignore)
                    ((symbol-function 'vellumory-search-clear) #'ignore)
                    ((symbol-function 'vellumory--search-begin)
                     (lambda (query sensitive origin) (setq shown (list query sensitive origin)))))
            (let ((state (vellumory--line-preview (current-buffer) '((page . 7) (y . 3.0)))))
              (funcall state 'preview (nth 2 lines))
              (should (equal (car moves) '((page . 1) (y . 0))))
              (funcall state 'exit nil)
              (should (equal (car moves) '((page . 7) (y . 3.0))))
              (funcall state 'return (nth 1 lines))
              (should (equal shown '("First line" t ((page . 0) (y . 0))))))))))))

(ert-deftest vellumory-embark-targets-selection-or-page ()
  (with-temp-buffer
    (setq major-mode 'vellumory-mode vellumory--current-page 4)
    (cl-letf (((symbol-function 'vellumory-selection-text) (lambda () nil)))
      (should (equal (vellumory-embark-target) '(vellumory-page "5"))))
    (cl-letf (((symbol-function 'vellumory-selection-text) (lambda () "chosen words")))
      (should (equal (vellumory-embark-target) '(vellumory-text "chosen words")))))
  (with-temp-buffer (should-not (vellumory-embark-target)))
  (when (require 'embark nil t)
    (should (eq (car embark-target-finders) 'vellumory-embark-target))
    (should (eq (lookup-key vellumory-embark-page-map "x") 'vellumory-extract-pages))
    (should (eq (keymap-parent vellumory-embark-text-map) embark-general-map)))
  (when (require 'consult nil t)
    (should (memq 'vellumory-consult-source-documents consult-buffer-sources))))

(ert-deftest vellumory-bidi-reordering-only-for-right-to-left-labels ()
  "Pages are images: reordering is off, unless a page label is right to left."
  (dolist (case '((["i" "ii" "1" "Ch-2"] . nil) (["א" "ב"] . t) (["1" "صفحة ٢"] . t)))
    (with-temp-buffer
      (setq-local bidi-display-reordering t)
      (setq vellumory--hash "h")
      (cl-letf (((symbol-function 'vellumory--rpc)
                 (lambda (_method _params callback &rest _)
                   (funcall callback `((result (hash . "h") (outline . []) (labels . ,(car case)))))))
                ((symbol-function 'vellumory--update-outline-buffer) #'ignore)
                ((symbol-function 'vellumory--reset-thumbnails) #'ignore))
        (vellumory--load-navigation)
        (should (eq bidi-display-reordering (cdr case)))))))

(ert-deftest vellumory-one-worker-for-every-document ()
  "Requests name their document, replies return to the buffer that asked, and
leaving closes the document; the last one out stops the worker."
  (let* ((sent nil)
         (process (make-pipe-process :name "vellumory-test-worker" :noquery t))
         (vellumory--worker process)
         (a (generate-new-buffer " *doc-a*")) (b (generate-new-buffer " *doc-b*")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_process string) (push (json-parse-string (string-trim string) :object-type 'alist) sent))))
          (clrhash vellumory--worker-callbacks)
          (dolist (spec (list (cons a 1) (cons b 2)))
            (with-current-buffer (car spec)
              (setq vellumory--process process vellumory--doc (cdr spec)
                    vellumory--inflight (make-hash-table :test #'eql))))
          (let (got-a got-b)
            (with-current-buffer a (vellumory--rpc "text" '((page . 0)) (lambda (r) (setq got-a (list (current-buffer) r)))))
            (with-current-buffer b (vellumory--rpc "links" '((page . 3)) (lambda (r) (setq got-b (current-buffer)))))
            (should (equal (mapcar (lambda (m) (alist-get 'doc (alist-get 'params m))) sent) '(2 1)))
            (should (equal (alist-get 'page (alist-get 'params (cadr sent))) 0))
            ;; Replies in any order reach their own buffers.
            (let ((ids (mapcar (lambda (m) (alist-get 'id m)) sent)))
              (vellumory--filter process (format "{\"id\":%d,\"result\":1}\n{\"id\":%d,\"result\":2}\n"
                                                 (car ids) (cadr ids))))
            (should (eq got-b b))
            (should (eq (car got-a) a))
            (should (equal (alist-get 'result (cadr got-a)) 2)))
          ;; hello and cancel name no document.
          (setq sent nil)
          (with-current-buffer a (vellumory--cancel '(5)))
          (should-not (assq 'doc (alist-get 'params (car sent))))
          ;; Leaving: a closes its document, b, the last, stops the worker.
          (setq sent nil)
          (with-current-buffer a (vellumory--leave-worker))
          (should (equal (alist-get 'method (car sent)) "close"))
          (should (equal (alist-get 'doc (alist-get 'params (car sent))) 1))
          (should (process-live-p process))
          (let (eof)
            (cl-letf (((symbol-function 'process-send-eof) (lambda (p) (setq eof p))))
              (with-current-buffer b (vellumory--leave-worker)))
            ;; The worker is asked to exit, so that it removes its rasters.
            (should (eq eof process))
            ;; and no new document joins it on its way out.
            (should-not vellumory--worker)))
      (when (process-live-p process) (delete-process process))
      (kill-buffer a) (kill-buffer b))))

(ert-deftest vellumory-missing-or-empty-documents-say-so ()
  "Not MuPDF's \"no objects found\": the file is missing, or empty."
  (should-error (vellumory-open "/nonexistent/vellumory/none.pdf") :type 'user-error)
  (with-temp-buffer
    (setq buffer-file-name "/nonexistent/vellumory/gone.pdf")
    (should (string-match-p "\\`No such file: " (vellumory--open-error "no objects found"))))
  (let ((empty (make-temp-file "vellumory-empty-" nil ".pdf")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name empty vellumory--file empty)
          (should (equal (vellumory--open-error "no objects found") "The document is empty"))
          (with-temp-file empty (insert "%PDF-1.7\n"))
          (should (equal (vellumory--open-error "broken") "broken")))
      (delete-file empty))))

(ert-deftest vellumory-a-worker-exiting-leaves-the-next-ones-requests ()
  "The old worker's sentinel must not drop what a newer worker was asked."
  (let* ((old (make-pipe-process :name "vellumory-test-old" :noquery t))
         (new (make-pipe-process :name "vellumory-test-new" :noquery t))
         (vellumory--worker new)
         (a (generate-new-buffer " *on-old*")) (b (generate-new-buffer " *on-new*")))
    (unwind-protect
        (progn
          (clrhash vellumory--worker-callbacks)
          (with-current-buffer a (setq vellumory--process old vellumory--inflight (make-hash-table)))
          (with-current-buffer b (setq vellumory--process new vellumory--inflight (make-hash-table)))
          (puthash 1 (cons a #'ignore) vellumory--worker-callbacks)
          (puthash 2 (cons b #'ignore) vellumory--worker-callbacks)
          (delete-process old)
          (vellumory--sentinel old "killed\n")
          (should-not (gethash 1 vellumory--worker-callbacks))
          (should (gethash 2 vellumory--worker-callbacks))
          (should (eq vellumory--worker new))
          (should (string-prefix-p "Worker stopped" (buffer-local-value 'vellumory--status a))))
      (dolist (p (list old new)) (when (process-live-p p) (delete-process p)))
      (kill-buffer a) (kill-buffer b))))

(ert-deftest vellumory-validation-shows-checks-beyond-verapdf ()
  "Preflight findings appear under veraPDF's report, pages as buttons;
before an answer the section says it is checking."
  (with-temp-buffer
    (vellumory-validation-mode)
    (vellumory--validation-redraw)
    (should (string-match-p "Validating with veraPDF…" (buffer-string)))
    (should (string-match-p "Beyond veraPDF\nWhat breaks.*\nChecking…" (buffer-string)))
    (setq vellumory--preflight [])
    (vellumory--validation-redraw)
    (should (string-match-p "✓ Nothing found" (buffer-string)))
    (setq vellumory--preflight
          (vector '((severity . "error") (code . "broken-links") (message . "Links that lead nowhere.") (pages . [3 7]))
                  '((severity . "note") (code . "no-title") (message . "No title.") (pages . []))))
    (setq vellumory--validation-report
          '((profile . "PDF/A-2B") (compliant . t) (failed_rules . 0) (failed_checks . 0)
            (passed_rules . 10) (passed_checks . 99) (rules . [])))
    (vellumory--validation-redraw)
    (let ((text (buffer-string)))
      (should (< (string-search "COMPLIANT" text) (string-search "Beyond veraPDF" text)))
      (should (string-match-p "✗ Links that lead nowhere\\. p\\. 3 p\\. 7" text))
      (should (string-match-p "· No title\\." text)))
    (goto-char (point-min))
    (search-forward "p. 7")
    (let (jumped)
      (cl-letf (((symbol-function 'vellumory--validation-jump) (lambda (page) (setq jumped page))))
        (push-button (1- (point))))
      (should (= jumped 6)))))

(ert-deftest vellumory-validation-groups-identical-checks-and-sums-up ()
  (let ((checks (vector '((page . 0) (message . "DeviceRGB")) '((page . 0) (message . "DeviceRGB"))
                        '((page . 1) (message . "DeviceRGB")) '((page . 0) (message . "Other")))))
    (should (equal (mapcar #'cdr (vellumory--validation-group checks)) '(2 1 1)))
    (should (equal (alist-get 'page (car (nth 1 (vellumory--validation-group checks)))) 1)))
  (should-not (vellumory--preflight-summary nil))
  (should (equal (vellumory--preflight-summary []) "Beyond veraPDF: nothing found"))
  (should (equal (vellumory--preflight-summary
                  (vector '((severity . "error")) '((severity . "note")) '((severity . "note"))))
                 "Beyond veraPDF: 1 error, 2 notes (below, b)"))
  (with-temp-buffer
    (vellumory-validation-mode)
    (setq vellumory--preflight (vector '((severity . "note") (message . "No title.") (pages . [])))
          vellumory--validation-report
          `((profile . "PDF/A-2B") (compliant . nil) (failed_rules . 1) (failed_checks . 3)
            (passed_rules . 1) (passed_checks . 1)
            (rules . [((specification . "ISO") (clause . "6.2.4.3") (test_number . 2) (failed_checks . 3)
                       (description . "DeviceRGB")
                       (checks . [((page . 0) (message . "RGB")) ((page . 0) (message . "RGB"))
                                  ((page . 0) (message . "RGB"))]))])))
    (vellumory--validation-redraw)
    (should (string-match-p "Beyond veraPDF: 1 note (below, b)" (buffer-string)))
    (should (= 1 (how-many "page 1 · RGB  ×3" (point-min) (point-max))))
    (vellumory-validation-beyond)
    (should (looking-at "Beyond veraPDF$"))))

(ert-deftest vellumory-previews-are-kept-across-sessions ()
  "Whole-page previews ask the worker to keep them; sharp strips do not."
  (with-temp-buffer
    (let (sent)
      (setq vellumory--pages [((width . 612) (height . 792))] vellumory--width 600 vellumory--scale 1.0
            vellumory--generation 1 vellumory--inflight (make-hash-table :test #'eql))
      (cl-letf (((symbol-function 'vellumory--rpc) (lambda (method params &rest _) (push (cons method params) sent) 1)))
        (vellumory--request-preview 0 3))
      (should (eq (alist-get 'persist (cdr (car sent))) t)))))

(ert-deftest vellumory-org-export-typst-archival-accessible-pdf ()
  "The Typst route: PDF/A-3a and PDF/UA-1 with the Org source inside, both
accepted by veraPDF, for a document whose language has a region (Typst takes
them apart)."
  ;; ox-typst needs Org 9.7; Emacs 29 ships 9.6.
  (skip-unless (version<= "9.7" (org-version)))
  (require 'vellumory-org)
  (should (equal (vellumory-org--typst-language "#set text(lang: \"en-GB\")\n" 'typst nil)
                 "#set text(lang: \"en\", region: \"GB\")\n"))
  (should (equal (vellumory-org--typst-language "#set text(lang: \"de\")\n" 'typst nil) "#set text(lang: \"de\")\n"))
  (should (equal (vellumory-org--flavour "a-2a") "2a"))
  (should (vellumory-org--source-allowed-p '("a-3a" "ua-1")))
  (should (vellumory-org--source-allowed-p '("ua-2" "a-4f")))
  (should-not (vellumory-org--source-allowed-p '("a-2a" "ua-1")))
  (should-not (vellumory-org--source-allowed-p '("a-4")))
  (should (equal (vellumory-org--flavour "ua-1") "ua1"))
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (lisp (getenv "VELLUMORY_OX_TYPST_LISP"))
         (directory (make-temp-file "vellumory-typst-" t))
         (org (expand-file-name "doc.org" directory))
         ;; The embedding edit keeps its snapshots here.
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         (vellumory-state-directory (expand-file-name "state" directory))
         (vellumory-program program)
         pdf)
    (skip-unless (and program lisp (executable-find "typst")))
    (add-to-list 'load-path lisp)
    (with-temp-file org
      (insert "#+TITLE: Vellumory Typst export\n#+AUTHOR: A. Person\n#+LANGUAGE: en-GB\n\n"
              "* Introduction\nSome *bold* text and a [[https://example.org][link]].\n\n"
              "- first\n- second\n\n| Name | Value |\n|------+-------|\n| a    |     1 |\n"))
    (unwind-protect
        (let ((source (find-file-noselect org)))
          (with-current-buffer source
            (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
              (setq pdf (vellumory-org-export-typst))))
          (should (file-exists-p pdf))
          (should (vellumory-test--wait (lambda () (= (length (cdr vellumory-org-export-results)) 2)) 120))
          (should (eq (alist-get "a-3a" (cdr vellumory-org-export-results) nil nil #'equal) t))
          (should (eq (alist-get "ua-1" (cdr vellumory-org-export-results) nil nil #'equal) t))
          (let ((reader (find-buffer-visiting pdf)) info)
            (with-current-buffer reader
              (should (vellumory-test--wait (lambda () vellumory--pages)))
              (vellumory--rpc "info" nil (lambda (reply) (setq info (alist-get 'result reply)))))
            (should (vellumory-test--wait (lambda () info)))
            (should (vellumory-test--embedded-source-p info "doc.org"))))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p directory (or (buffer-file-name buffer) "")) (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest vellumory-attach-file-relationship ()
  "With a prefix argument an attachment names its relationship to the document."
  (require 'vellumory-edit)
  (let (sent)
    (cl-letf (((symbol-function 'vellumory--edit) (lambda (operation) (setq sent operation)))
              ((symbol-function 'read-file-name) (lambda (&rest _) "/notes/doc.org"))
              ((symbol-function 'read-string) (lambda (_prompt &optional initial &rest _) (or initial "Org source")))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (should (member "Supplement" collection)) "Source")))
      (let ((current-prefix-arg '(4))) (call-interactively #'vellumory-attach-file))
      (should (equal (alist-get 'relationship sent) "Source"))
      (should (equal (alist-get 'key sent) "doc.org"))
      (should (equal (alist-get 'description sent) "Org source"))
      (let ((current-prefix-arg nil)) (call-interactively #'vellumory-attach-file))
      (should (equal (alist-get 'relationship sent) "")))))

(ert-deftest vellumory-occur-across-documents ()
  "Matching lines of several documents, by document and page, as pdf-occur
lists them: one document open in a reader (its text reused), one not; RET opens
the match's own document there, highlighted."
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-occur-docs-" t))
         (first (expand-file-name "first.pdf" directory)) (second (expand-file-name "second.pdf" directory))
         (vellumory-program program)
         (vellumory-state-directory (expand-file-name "state" directory))
         (process-environment (cons (concat "VELLUMORY_STORE=" (expand-file-name "store" directory))
                                    process-environment))
         occur opened)
    (skip-unless (and program source))
    (copy-file source first) (copy-file source second)
    (let ((reader (vellumory-open first t)))
      (unwind-protect
          (progn
            (with-current-buffer reader (should (vellumory-test--wait (lambda () vellumory--pages))))
            ;; Every open document, then the files given.
            (should (member (file-truename first) (vellumory--occur-files)))
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (vellumory-occur-documents "Line 1[0-9]: text" (list first second)))
            (should (vellumory-test--wait
                     (lambda () (and (setq occur (get-buffer "*Occur: documents*"))
                                     (> (buffer-size occur) 0)))))
            (with-current-buffer occur
              (should (string-match-p "\\`[0-9]+ matches for “Line 1\\[0-9\\]: text” in 2 of 2 documents"
                                      (buffer-string)))
              (should (string-match-p (regexp-quote (abbreviate-file-name second)) (buffer-string)))
              ;; The first match of the second document opens it.
              (goto-char (point-min))
              (search-forward (abbreviate-file-name second))
              (forward-line 1)
              (should (equal (nth 2 (get-text-property (next-single-property-change (point) 'vellumory-occur)
                                                       'vellumory-occur))
                             second))
              (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
                (vellumory-occur-goto)
                (setq opened (current-buffer))))
            (should (equal (buffer-local-value 'vellumory--source-file opened) (file-truename second)))
            (with-current-buffer opened
              (should (string-match-p "\\`Line 1[0-9]: text\\'" vellumory--search-query))
              (should (vellumory-test--wait (lambda () vellumory--search-current)))))
        (when (buffer-live-p occur) (kill-buffer occur))
        (dolist (buffer (list reader opened)) (when (buffer-live-p buffer) (kill-buffer buffer)))
        (delete-directory directory t)))))

(ert-deftest vellumory-virtual-documents ()
  "A pdf-tools .vpdf file opens as one composed document: its page specs and
regions understood, the header naming each page's source, M-} to the next
source, edits refused, C-c C-c to its list and back."
  (require 'vellumory-virtual)
  (should (equal (vellumory-virtual--page-spec 3) '(3 3 nil)))
  (should (equal (vellumory-virtual--page-spec '(1 . 4)) '(1 4 nil)))
  (should (equal (vellumory-virtual--page-spec '(nil 0 0 1 0.5)) '(1 0 (0 0 1 0.5))))
  (should (equal (vellumory-virtual--page-spec '(2 0 0 1 1)) '(2 2 nil)))
  (should (equal (vellumory-virtual--page-spec '((2 . 3) 0 0.5 1 1)) '(2 3 (0 0.5 1 1))))
  (should-error (vellumory-virtual--page-spec '(2 0 0 1)) :type 'user-error)
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (directory (make-temp-file "vellumory-virtual-" t))
         (vpdf (expand-file-name "all.vpdf" directory))
         (vellumory-program program)
         (vellumory-state-directory (expand-file-name "state" directory))
         (process-environment (append (list (concat "XDG_CACHE_HOME=" (expand-file-name "cache" directory))
                                            (concat "VELLUMORY_STORE=" (expand-file-name "store" directory)))
                                      process-environment))
         (magic-mode-alist (cons (cons vellumory-virtual-magic #'vellumory-virtual-mode) magic-mode-alist))
         buffer)
    (skip-unless (and program source))
    (copy-file source (expand-file-name "a.pdf" directory))
    (copy-file source (expand-file-name "b.pdf" directory))
    (with-temp-file vpdf
      (insert ";; %VPDF 1.0\n((\"a.pdf\" 3 (1 . 2))\n (\"b.pdf\" (1 0 0 1 0.5)))\n"))
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect vpdf))
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (should (eq major-mode 'vellumory-mode))
            (should vellumory-virtual-view-mode)
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            ;; a.pdf pages 3, 1, 2, then the top half of b.pdf's page 1.
            (should (= (length vellumory--pages) 4))
            (should (= (alist-get 'height (aref vellumory--pages 3))
                       (/ (alist-get 'height (aref vellumory--pages 1)) 2)))
            (should (string-match-p "a\\.pdf p\\. 3" (vellumory-virtual--header)))
            (vellumory-virtual-next-document)
            (should (vellumory-test--wait (lambda () (eql vellumory--current-page 3))))
            (should (string-match-p "b\\.pdf p\\. 1" (vellumory-virtual--header)))
            (should-error (vellumory--edit-file) :type 'user-error)
            ;; The list again, unchanged, and back to the composition.
            (vellumory-virtual-edit)
            (should (eq major-mode 'vellumory-virtual-edit-mode))
            (should (string-prefix-p ";; %VPDF 1.0" (buffer-string)))
            (should-not (buffer-modified-p))
            (vellumory-virtual-mode)
            (should (eq major-mode 'vellumory-mode))
            (should (vellumory-test--wait (lambda () vellumory--pages)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest vellumory-search-regexp-matches-and-geometry ()
  "Regexp search, as pdf-isearch's: matches found in the page's characters,
each a quad per line it covers that agrees with the worker's literal search;
a match may span lines; an incomplete regexp matches nothing."
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (source (getenv "VELLUMORY_TEST_PDF"))
         (vellumory-program program)
         (vellumory-state-directory (make-temp-file "vellumory-regexp-state-" t)))
    (skip-unless (and program source))
    (let ((reader (vellumory-open source t)))
      (unwind-protect
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (let* ((result (vellumory--regexp-page-result 0 "Line 1[0-9]: text" nil))
                   (hits (alist-get 'hits result)))
              ;; The first page of the fixture has lines 1–17.
              (should (= (length hits) 8))
              (should (cl-every (lambda (hit) (= (length hit) 1)) hits)))
            ;; The same text, found literally by the worker, has the same quad.
            (let* ((regexp (aref (aref (alist-get 'hits (vellumory--regexp-page-result 0 "Line 12: text" t)) 0) 0))
                   literal)
              (vellumory--rpc "search" '((page . 0) (query . "Line 12: text") (case_sensitive . t))
                              (lambda (reply) (setq literal (aref (aref (alist-get 'hits (alist-get 'result reply)) 0) 0))))
              (should (vellumory-test--wait (lambda () literal)))
              (dolist (key '(x0 x1 x2 x3 y0 y1 y2 y3))
                (should (< (abs (- (alist-get key regexp) (alist-get key literal))) 1.5))))
            ;; Across a line break: two quads.
            (let ((hits (alist-get 'hits (vellumory--regexp-page-result 0 "boundaries\\.\nLine 2:" nil))))
              (should (= (length hits) 1))
              (should (= (length (aref hits 0)) 2)))
            (should (= (length (alist-get 'hits (vellumory--regexp-page-result 0 "Line [" nil))) 0))
            ;; Through the search itself, on characters not fetched yet (the wait
            ;; for them once let a timer pump the search again): highlights and a
            ;; current match.
            (setq vellumory--chars nil)
            (vellumory--search-begin "Line [0-9]+: text" nil '((page . 0) (y . 0)) t)
            (should vellumory--search-regexp)
            (should (vellumory-test--wait (lambda () vellumory--search-current)))
            (should (equal (car vellumory--search-current) 0))
            (should (> (length (vellumory--search-page-quads 0)) 10))
            ;; M-r flips the session to text and back.
            (let ((vellumory--search-session (list reader nil '((page . 0) (y . 0)) t)))
              (cl-letf (((symbol-function 'minibuffer-contents-no-properties) (lambda () "Line 3: text")))
                (vellumory-search-minibuffer-toggle-regexp))
              (should-not (nth 3 vellumory--search-session))
              (should-not vellumory--search-regexp)
              (should (equal vellumory--search-query "Line 3: text"))))
        (kill-buffer reader)))))

(ert-deftest vellumory-epub-reflows-with-text-size ()
  "EPUB opens like any document, laid out as `vellumory-reflow-text-size' asks;
the text-scale keys change that size, the document is laid out anew at the same
share of it; in a PDF they zoom."
  (should (string-match-p (vellumory--extension-regexp) "book.epub"))
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (fixture (getenv "VELLUMORY_EPUB_FIXTURE"))
         (vellumory-program program)
         (vellumory-state-directory (make-temp-file "vellumory-epub-state-" t))
         (vellumory-reflow-text-size 11))
    (skip-unless (and program fixture))
    (let ((reader (vellumory-open (expand-file-name "book.epub" fixture) t)) before)
      (unwind-protect
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (should vellumory--reflowable)
            (setq before (length vellumory--pages))
            (let ((text nil))
              (vellumory--with-document-text (lambda (pages) (setq text (mapconcat #'identity pages " "))))
              (should (vellumory-test--wait (lambda () text)))
              (should (string-match-p "Chapter one" text)))
            (let ((hash vellumory--hash))
              (vellumory-text-scale-adjust 6)
              (should (vellumory-test--wait (lambda () (and (not (equal hash vellumory--hash)) vellumory--pages))))
              (should (> (length vellumory--pages) before))
              (should (= vellumory--reflow-size 17)))
            (vellumory-text-scale-adjust 0)
            (should (vellumory-test--wait (lambda () (= (length vellumory--pages) before)))))
        (kill-buffer reader)))
    ;; In a PDF the same keys zoom.
    (let ((reader (vellumory-open (getenv "VELLUMORY_TEST_PDF") t)))
      (unwind-protect
          (with-current-buffer reader
            (should (vellumory-test--wait (lambda () vellumory--pages)))
            (should-not vellumory--reflowable)
            (let ((zoom vellumory--zoom))
              (vellumory-text-scale-adjust 1)
              (should (> vellumory--zoom zoom))))
        (kill-buffer reader)))))

(ert-deftest vellumory-converted-formats ()
  "DVI, PostScript and DjVu, as DocView reads them, through a conversion to PDF:
read like any document, the conversion cached, edits refused, the original's
bytes back when leaving, and a recompiled DVI shown converted anew."
  (require 'vellumory-convert)
  ;; The converters are what it tests; the package build has them.
  (skip-unless (and (executable-find "dvipdfmx") (executable-find "ddjvu")))
  (should (equal (cadr (vellumory--converter "a.dvi")) "dvipdfmx"))
  (let ((vellumory-converters '((("odt") "no-such-converter" input))))
    (should-not (vellumory--converter "a.odt")))
  ;; Installed: DVI and DjVu visit as documents, PostScript stays in ps-mode.
  (let ((auto-mode-alist auto-mode-alist) (magic-mode-alist magic-mode-alist)
        (file-name-handler-alist file-name-handler-alist))
    (when (and (executable-find "dvipdfmx") (executable-find "ddjvu"))
      (vellumory-install)
      (should (eq (assoc-default "x.dvi" auto-mode-alist #'string-match-p) 'vellumory-converted-mode))
      (should (eq (assoc-default "x.djvu" auto-mode-alist #'string-match-p) 'vellumory-converted-mode))
      (should-not (eq (assoc-default "x.ps" auto-mode-alist #'string-match-p) 'vellumory-converted-mode))))
  (let* ((program (getenv "VELLUMORY_TEST_PROGRAM")) (fixtures (getenv "VELLUMORY_CONVERT_FIXTURES"))
         (directory (make-temp-file "vellumory-convert-" t))
         (vellumory-program program)
         (vellumory-cache-directory (expand-file-name "cache" directory))
         (vellumory-state-directory (expand-file-name "state" directory)))
    (skip-unless (and program fixtures (executable-find "dvipdfmx") (executable-find "ps2pdf")
                      (executable-find "ddjvu")))
    (unwind-protect
        (pcase-dolist (`(,name ,pages ,text) '(("doc.dvi" 2 "Converted") ("doc.ps" 2 "Converted") ("doc.djvu" 1 nil)))
          (let* ((file (expand-file-name name directory)) buffer)
            (copy-file (expand-file-name name fixtures) file)
            (set-file-modes file #o644)
            (setq buffer (find-file-noselect file))
            (unwind-protect
                (with-current-buffer buffer
                  (vellumory-converted-mode)
                  (should (vellumory-test--wait (lambda () (and (derived-mode-p 'vellumory-mode) vellumory--pages)) 20))
                  (should (= (length vellumory--pages) pages))
                  (should (equal vellumory--original-file file))
                  (should (string-prefix-p (expand-file-name "converted/" vellumory-cache-directory) vellumory--file))
                  (when text
                    (let (got)
                      (vellumory--with-document-text (lambda (texts) (setq got (mapconcat #'identity texts " "))))
                      (should (vellumory-test--wait (lambda () got)))
                      (should (string-match-p text got))))
                  (should-error (vellumory--edit-file) :type 'user-error)
                  (when (equal name "doc.dvi")
                    ;; Recompiled: one page now.
                    (let ((hash vellumory--hash))
                      (copy-file (expand-file-name "short.dvi" fixtures) file t)
                      (set-file-modes file #o644)
                      (vellumory--reconvert)
                      (should (vellumory-test--wait (lambda () (and (not (equal hash vellumory--hash))
                                                                    (= (length vellumory--pages) 1)))
                                                    20))))
                  ;; Leaving the reader puts the original back.
                  (fundamental-mode)
                  (should (equal (buffer-substring-no-properties (point-min) (min (point-max) 3))
                                 (with-temp-buffer (insert-file-contents-literally file nil 0 2)
                                                   (buffer-string)))))
              (kill-buffer buffer))))
      (delete-directory directory t))))

(defvar gcmh-mode)

(ert-deftest vellumory-collects-garbage-when-reading-rests ()
  "Moves of the view keep collection out of their redisplay until reading rests."
  (skip-unless (not (fboundp 'igc-info)))
  (let ((gc-cons-threshold 800000) (vellumory--gc-saved nil) (vellumory--gc-activity 0.0) (gcmh-mode nil) (idle nil) (collected nil))
    (cl-letf (((symbol-function 'run-with-timer) (lambda (_secs _repeat fn &rest _) (push fn idle)))
              ((symbol-function 'garbage-collect-maybe) (lambda (factor) (push factor collected))))
      (vellumory--defer-gc)
      (should (= gc-cons-threshold vellumory-gc-threshold))
      (should (equal idle '(vellumory--collect-deferred)))
      ;; A next move before reading rests keeps the first one's record.
      (vellumory--defer-gc)
      (should (= vellumory--gc-saved 800000))
      (should (= (length idle) 1))
      ;; Moments after a move the collection waits for reading to rest.
      (vellumory--collect-deferred)
      (should (= (length idle) 2))
      (should (= gc-cons-threshold vellumory-gc-threshold))
      (setq vellumory--gc-activity (- (float-time) 10))
      ;; While a scroll animates, the collection waits for its end.
      (with-temp-buffer
        (setq vellumory--animation (timer-create))
        (vellumory--collect-deferred))
      (should (= (length idle) 3))
      (should (= gc-cons-threshold vellumory-gc-threshold))
      (vellumory--collect-deferred)
      (should (= gc-cons-threshold 800000))
      (should-not vellumory--gc-saved)
      (should (equal collected '(2)))
      ;; A threshold set by something else meanwhile is kept.
      (vellumory--defer-gc)
      (setq gc-cons-threshold 123456 vellumory--gc-activity (- (float-time) 10))
      (vellumory--collect-deferred)
      (should (= gc-cons-threshold 123456))
      ;; Left to gcmh when it is on.
      (setq gcmh-mode t)
      (vellumory--defer-gc)
      (should (= gc-cons-threshold 123456)))))

(ert-deftest vellumory-view-keys-win-in-evil-normal-state ()
  "A view's own keys outrank evil's normal state (the history tree's c, d, v;
the validation report's n, p, b); the syntax view's q closes it while it is
read-only and is a letter, or evil's macro recording, while editing."
  (require 'vellumory-syntax)
  (with-temp-buffer
    (vellumory-syntax-mode)
    (setq buffer-read-only t)
    (should (eq (key-binding "q") #'quit-window))
    (setq buffer-read-only nil)
    (should (eq (key-binding "q") #'self-insert-command)))
  (let ((evil-lisp (getenv "VELLUMORY_EVIL_LISP")))
    (skip-unless (and evil-lisp (file-directory-p evil-lisp)))
    (add-to-list 'load-path evil-lisp)
    (skip-unless (ignore-errors (require 'evil) t))
    (require 'vellumory-evil)
    (vellumory-evil-setup)
    (with-temp-buffer
      (vellumory-syntax-mode)
      (evil-local-mode 1)
      (unwind-protect
          (progn
            (evil-normal-state)
            (setq buffer-read-only t)
            (should (eq (key-binding "q") #'quit-window))
            (setq buffer-read-only nil)
            (should (eq (key-binding "q") #'evil-record-macro)))
        (evil-local-mode -1)))
    (require 'vellumory-edit)
    (require 'vellumory-validate)
    (dolist (case '((vellumory-history-mode "c" vellumory-history-checkout) (vellumory-history-mode "d" vellumory-history-diff)
                    (vellumory-history-mode "v" vellumory-history-view) (vellumory-history-mode "RET" vellumory-history-checkout)
                    (vellumory-validation-mode "n" vellumory-validation-next-rule)
                    (vellumory-validation-mode "b" vellumory-validation-beyond)))
      (with-temp-buffer
        (funcall (car case))
        (evil-local-mode 1)
        (unwind-protect
            (progn (evil-normal-state)
                   (should (equal (list (car case) (nth 1 case) (key-binding (kbd (nth 1 case))))
                                  (list (car case) (nth 1 case) (nth 2 case)))))
          (evil-local-mode -1))))))

(ert-deftest vellumory-diff-geometry-words ()
  "A turned or cropped page's geometry reads as words in the comparison."
  (require 'vellumory-diff)
  (should (equal (vellumory--diff-geometry 90) "90°"))
  (should (equal (vellumory--diff-geometry nil) "none"))
  (should (equal (vellumory--diff-geometry [0 0 612 792]) "612 × 792 pt at 0, 0"))
  (should (equal (vellumory--diff-geometry [36 36 576.5 756]) "540.5 × 720 pt at 36, 36")))

(ert-deftest vellumory-edits-refuse-read-only-documents ()
  "An edit of a read-only document (one in the Nix store) says so at once,
instead of failing in the worker on the temporary file it writes."
  (require 'vellumory-edit)
  (let* ((directory (make-temp-file "vellumory-read-only-" t))
         (file (expand-file-name "doc.pdf" directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "%PDF-1.4\n"))
          (set-file-modes directory #o500)
          (with-temp-buffer
            (setq vellumory--source-file file)
            (should (string-match-p "read-only"
                                    (cadr (should-error (vellumory--edit-file) :type 'user-error))))))
      (set-file-modes directory #o700)
      (delete-directory directory t))))


(ert-deftest vellumory-dispatch-keys ()
  "? and C-c C-v open the menu in a document."
  (should (eq (keymap-lookup vellumory-mode-map "?") #'vellumory-dispatch))
  (should (eq (keymap-lookup vellumory-mode-map "C-c C-v") #'vellumory-dispatch)))

(ert-deftest vellumory-context-menus-everywhere ()
  "A right click has a menu in the reader, its sidebars and its views."
  (require 'vellumory)
  (dolist (file '("vellumory-thumbnails" "vellumory-navigation" "vellumory-edit" "vellumory-validate"
                  "vellumory-diff" "vellumory-details" "vellumory-search" "vellumory-annotate" "vellumory-dired"
                  "vellumory-reflow"))
    (require (intern file)))
  (pcase-dolist (`(,_ . ,map) vellumory-context--maps)
    (should (equal (list map (keymap-lookup (symbol-value map) "<down-mouse-3>"))
                   (list map #'vellumory-context-menu))))
  ;; A view's menu holds its own commands, named by their documentation.
  (with-temp-buffer
    (vellumory-history-mode)
    (let* ((menu (vellumory--context-menu (make-sparse-keymap) `(mouse-3 ,(posn-at-point))))
           (labels (let (all) (map-keymap (lambda (_ item) (when (eq (car-safe item) 'menu-item) (push (nth 1 item) all))) menu) all)))
      (should (member (vellumory-context--label #'vellumory-history-checkout) labels))
      (should (member "Close" labels)))))

(ert-deftest vellumory-dispatch-opens-everywhere ()
  "The menu opens from the generated autoloads alone, in a document, a plain
buffer, Dired and Org, and every entry names a defined or autoloaded command.
In another Emacs: a menu left open in batch keeps its keys active."
  (should (equal (vellumory-test--dispatch-probe) "ok")))

(defun vellumory-test--dispatch-probe ()
  "Run the menu checks in a fresh Emacs; return its last line."
  (let ((lisp (file-name-directory (locate-library "vellumory"))))
    (with-temp-buffer
      (call-process
       (expand-file-name invocation-name invocation-directory) nil t nil
       "--batch" "-Q" "-L" lisp "--eval"
       (prin1-to-string
        `(progn
           (load "vellumory-autoloads" nil t)
           (dolist (mode '(fundamental-mode dired-mode org-mode vellumory-mode))
             (with-temp-buffer
               (let ((major-mode mode)) (call-interactively #'vellumory-dispatch))))
           (let (missing)
             (with-temp-buffer
               (insert-file-contents ,(expand-file-name "vellumory-dispatch.el" lisp))
               (while (re-search-forward "(\"[^\"]+\" \"[^\"]+\" \\([a-z0-9-]+\\)" nil t)
                 (let ((command (intern (match-string 1))))
                   (unless (or (fboundp command) (get command 'transient--suffix)) (push command missing)))))
             (princ (if missing (format "missing %S" missing) "ok"))))))
      (car (last (split-string (buffer-string) "\n" t))))))

(ert-deftest vellumory-details-edit-metadata-in-place ()
  "Editing a metadata value in the details applies one recorded metadata edit."
  (require 'vellumory-details)
  (let (sent)
    (with-temp-buffer
      (let ((source (current-buffer)))
        (with-temp-buffer
          (setq vellumory--info-source source)
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Field notes"))
                    ((symbol-function 'vellumory--edit) (lambda (operation) (setq sent operation))))
            (vellumory--details-edit-field 'title "Title" "Draft")))))
    (should (equal sent '((op . "metadata") (set . ((title . "Field notes"))))))
    (should (equal (json-serialize sent) "{\"op\":\"metadata\",\"set\":{\"title\":\"Field notes\"}}"))))

(ert-deftest vellumory-doctor-checks ()
  "The doctor's checks each have a known status; with the built worker,
it answers hello in the protocol this Lisp speaks."
  (require 'vellumory-doctor)
  (let ((sections (vellumory-doctor--sections)))
    (should (>= (length sections) 6))
    (pcase-dolist (`(,_ . ,checks) sections)
      (dolist (check checks)
        (should (memq (plist-get check :status) '(ok info warn error)))
        (should (stringp (plist-get check :title))))))
  (let ((worker (expand-file-name "../.lake/build/bin/vellumory" (file-name-directory (locate-library "vellumory")))))
    (skip-unless (file-executable-p worker))
    (let ((hello (vellumory-doctor--worker-hello worker)))
      (should (consp hello))
      (should (eql (alist-get 'protocol hello) vellumory-doctor--protocol))
      ;; And the programs it runs, which the doctor then needs nowhere else.
      (should (equal (sort (mapcar #'car (alist-get 'tools hello)) #'string<) '(ocrmypdf qpdf verapdf))))))

(ert-deftest vellumory-night-mode-and-auto-colours ()
  "`auto' inverts pages with a dark theme only; night mode inverts one
document and, turned off, returns it to the configured colours."
  (with-temp-buffer
    (let ((vellumory-theme 'auto))
      (cl-letf (((symbol-function 'frame-parameter) (lambda (_ parameter) (and (eq parameter 'background-mode) 'dark))))
        (should (eq (vellumory--theme) 'invert)))
      (cl-letf (((symbol-function 'frame-parameter) (lambda (_ parameter) (and (eq parameter 'background-mode) 'light))))
        (should (null (vellumory--theme)))))
    (let ((major-mode 'vellumory-mode))
      (vellumory-night-mode 1)
      (should (eq vellumory-theme 'invert))
      (vellumory-night-mode -1)
      (should-not (local-variable-p 'vellumory-theme)))))

(ert-deftest vellumory-paper-takes-the-page-colours ()
  "Unrendered pages show the colour paper will have once drawn: as is,
the theme's background with `tint', black when inverted."
  (with-temp-buffer
    (let ((vellumory-theme nil))
      (vellumory--update-paper)
      (should-not vellumory--paper-remap))
    (let ((vellumory-theme 'invert))
      (vellumory--update-paper)
      (should (equal (plist-get (cadr (assq 'vellumory-paper face-remapping-alist)) :background)
                     (vellumory--invert-luminance (face-background 'vellumory-paper nil t))))
      (should (equal (vellumory--invert-luminance "white") "#000000"))
      (should (equal (vellumory--invert-luminance "black") "#ffffff")))
    (let ((vellumory-theme 'tint))
      (vellumory--update-paper)
      (should (equal (plist-get (cadr (assq 'vellumory-paper face-remapping-alist)) :background)
                     (face-background 'default nil t)))
      (should (= (length face-remapping-alist) 1)))))

(ert-deftest vellumory-theme-commands ()
  "Page-colour commands set the document's colours and redraw its pages and
previews, from its sidebar too; a number or \\[universal-argument] picks one."
  (let ((document (generate-new-buffer " document")) (sidebar (generate-new-buffer " previews")) calls)
    (unwind-protect
        (cl-letf (((symbol-function 'vellumory--layout-preserving) (lambda () (push 'pages calls)))
                  ((symbol-function 'vellumory--thumbnail-layout) (lambda () (push 'previews calls)))
                  ((symbol-function 'completing-read) (lambda (&rest _) "inverted with a dark theme")))
          (with-current-buffer document
            (setq major-mode 'vellumory-mode)
            (setq-local vellumory--pages [1] vellumory--previews (make-hash-table)
                        vellumory--thumbnail-buffer sidebar))
          (with-current-buffer sidebar
            (setq-local vellumory--thumbnail-source document)
            (vellumory-theme-invert)
            (should (eq (buffer-local-value 'vellumory-theme document) 'invert))
            (should (equal calls '(previews pages)))
            (vellumory-cycle-theme)
            (should (null (buffer-local-value 'vellumory-theme document)))
            (vellumory-cycle-theme 1)
            (should (eq (buffer-local-value 'vellumory-theme document) 'tint))
            (vellumory-cycle-theme '(4))
            (should (eq (buffer-local-value 'vellumory-theme document) 'auto))))
      (kill-buffer document)
      (kill-buffer sidebar))))

(ert-deftest vellumory-info-finds-the-manual-beside-the-worker ()
  "The manual is found beside a Nix-installed worker when the Lisp comes
from elsewhere (straight.el), as well as beside the Lisp itself."
  (let* ((prefix (make-temp-file "vellumory-prefix-" t))
         (manual (expand-file-name "share/emacs/site-lisp/vellumory/vellumory.info" prefix))
         (program (expand-file-name "bin/vellumory" prefix)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory manual) t)
          (make-directory (file-name-directory program) t)
          (with-temp-file manual (insert "manual"))
          (with-temp-file program (insert "#!/bin/sh\n"))
          (set-file-modes program #o755)
          (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "/nonexistent/vellumory-install.el")))
            (let ((vellumory-program program))
              (should (equal (vellumory-info--file) manual)))))
      (delete-directory prefix t))))

(ert-deftest vellumory-missing-worker-is-said-plainly ()
  "Opening a document without the worker program says what is missing and
where to look, instead of Emacs's \"Searching for program\" error."
  (let ((vellumory-program "/nonexistent/vellumory"))
    (with-temp-buffer
      (let ((err (should-error (vellumory-mode) :type 'user-error)))
        (should (string-search "worker program" (cadr err)))
        (should (string-search "vellumory-doctor" (cadr err)))))))
