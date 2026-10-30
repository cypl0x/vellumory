;;; bench.el --- Comparative reader measurements in a real GUI Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Measures Vellumory, pdf-tools (single page and roll mode) and DocView in
;; the same window on the same document.  "Blocking" is wall time spent on
;; Emacs' main thread in the command plus a forced redisplay: the latency a
;; keypress experiences.  "Sharp" is wall time until every visible part of
;; the viewport shows a final-resolution raster.  Measurements are only
;; meaningful relative to each other on one machine and window size.

;;; Code:
(require 'cl-lib)
(require 'doc-view)

(defvar vellumory-bench-timeout 60.0)

(defvar vellumory-bench-jump 300
  "Page the jump goes to: 300, or the last page of a shorter document.")

(defun vellumory-bench--scroll-start ()
  "Scrolling starts where there is room for 40 fast steps: after the jump in a
long document, from the first page in a shorter one (its last page leaves none)."
  (< vellumory-bench-jump 300))

(defun vellumory-bench--ms (start) (* 1000.0 (- (float-time) start)))

(defun vellumory-bench--block (fn)
  "Milliseconds FN plus a forced redisplay keep the main thread busy."
  (let ((start (float-time)))
    (funcall fn)
    (redisplay t)
    (vellumory-bench--ms start)))

(defun vellumory-bench--wait (done &optional process)
  "Pump events until DONE returns non-nil; return elapsed ms or nil on timeout."
  (let ((start (float-time)) (deadline (+ (float-time) vellumory-bench-timeout)))
    (while (and (not (funcall done)) (< (float-time) deadline))
      (if process (accept-process-output process 0.002) (sit-for 0.002))
      (redisplay t))
    (and (funcall done) (vellumory-bench--ms start))))

(defun vellumory-bench--stats (samples)
  (let* ((sorted (sort (copy-sequence samples) #'<)) (n (length sorted)))
    (list :mean (/ (apply #'+ sorted) n) :p50 (nth (/ n 2) sorted)
          :p95 (nth (min (1- n) (floor (* 0.95 n))) sorted) :max (car (last sorted)))))

(defun vellumory-bench--round (plist)
  (cl-loop for (k v) on plist by #'cddr
           append (list k (cond ((floatp v) (/ (round (* 10 v)) 10.0))
                                ((and (consp v) (keywordp (car v))) (vellumory-bench--round v))
                                (t v)))))

(defun vellumory-bench--scroll (step-fn steps done process)
  (let ((blocking (cl-loop repeat steps collect (vellumory-bench--block step-fn))))
    (list :blocking (vellumory-bench--stats blocking)
          :total-blocking (apply #'+ blocking)
          :sharp-after (vellumory-bench--wait done process))))

;;;; Readers

(defun vellumory-bench-vellumory (file)
  ;; A fresh state directory: a saved reading position would turn the open
  ;; and the jump below into measurements of something else.
  (let* ((vellumory-state-directory (make-temp-file "vellumory-bench-state-" t))
         (start (float-time)) (buffer (vellumory-open file)) result)
    (with-current-buffer buffer
      (let* ((sharp (lambda ()
                     ;; Every visible tile final; idle prefetch may continue.
                     (and vellumory--pages vellumory--visible
                          (not (cl-some #'vellumory--needs-sharp vellumory--visible))))))
        (setq vellumory--last-motion 0.0)
        (vellumory-bench--wait (lambda () vellumory--pages) vellumory--process)
        ;; The other readers jump to the same page.
        (setq vellumory-bench-jump (min 300 (length vellumory--pages)))
        (vellumory--refresh t)
        (vellumory-bench--wait sharp vellumory--process)
        (push (list :open-sharp (vellumory-bench--ms start)) result)
        (let ((blocking (vellumory-bench--block (lambda () (vellumory-goto-page vellumory-bench-jump)))))
          (push (list :jump (list :blocking blocking
                                  :sharp (vellumory-bench--wait sharp vellumory--process)))
                result))
        (when (vellumory-bench--scroll-start)
          (vellumory-first-page)
          (vellumory-bench--wait sharp vellumory--process))
        (push (list :read-scroll (vellumory-bench--scroll (lambda () (vellumory-scroll 96)) 40
                                                          sharp vellumory--process))
              result)
        (push (list :fast-scroll (vellumory-bench--scroll (lambda () (vellumory-scroll 300)) 40
                                                          sharp vellumory--process))
              result)
        (push (list :worker-threads vellumory--threads
                    :rendered-megapixels (/ vellumory--rendered-pixels 1e6)) result)))
    (kill-buffer buffer)
    (apply #'append (nreverse result))))

(defun vellumory-bench--pdf-tools-load (lisp tablist)
  (add-to-list 'load-path lisp)
  (add-to-list 'load-path tablist)
  (load (expand-file-name "tablist-autoloads" tablist) nil t)
  (load (expand-file-name "pdf-tools-autoloads" lisp) nil t)
  (require 'pdf-tools)
  (require 'pdf-roll)
  (setq pdf-info-epdfinfo-program (expand-file-name "epdfinfo" lisp)
        pdf-view-display-size 'fit-width)
  (pdf-tools-install-noverify))

(defun vellumory-bench-pdf-tools (file roll)
  (let ((start (float-time)) buffer result)
    (setq buffer (let ((auto-mode-alist (cons '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist))
                       (magic-mode-alist nil))
                   (find-file file)))
    (with-current-buffer buffer
      (when roll (pdf-view-roll-minor-mode 1))
      (pdf-view-fit-width-to-window)
      (redisplay t)
      (push (list :open-sharp (vellumory-bench--ms start)) result)
      (push (list :jump (list :blocking (vellumory-bench--block (lambda () (pdf-view-goto-page vellumory-bench-jump)))
                              :sharp 0.0))
            result)
      (when (vellumory-bench--scroll-start) (pdf-view-goto-page 1) (redisplay t))
      (let ((step (if roll (lambda () (pdf-roll-scroll-forward 96 nil t))
                    (lambda () (pdf-view-next-line-or-next-page
                                (max 1 (round 96 (frame-char-height))))))))
        (push (list :read-scroll (vellumory-bench--scroll step 40 #'always nil)) result))
      (let ((step (if roll (lambda () (pdf-roll-scroll-forward 300 nil t))
                    (lambda () (pdf-view-next-line-or-next-page
                                (max 1 (round 300 (frame-char-height))))))))
        (push (list :fast-scroll (vellumory-bench--scroll step 40 #'always nil)) result)))
    (kill-buffer buffer)
    (apply #'append (nreverse result))))

(defun vellumory-bench-doc-view (file)
  (let ((start (float-time)) buffer result)
    (setq buffer (let ((auto-mode-alist (cons '("\\.pdf\\'" . doc-view-mode) auto-mode-alist))
                       (magic-mode-alist nil) (doc-view-continuous t))
                   (find-file file)))
    (with-current-buffer buffer
      (let ((shown (lambda (page)
                     (lambda () (and (eq major-mode 'doc-view-mode)
                                     (= (doc-view-current-page) page)
                                     (doc-view-current-image))))))
        (push (list :open-sharp (let ((w (vellumory-bench--wait (funcall shown 1))))
                                  (and w (vellumory-bench--ms start))))
              result)
        (push (list :jump (list :blocking (vellumory-bench--block (lambda () (doc-view-goto-page vellumory-bench-jump)))
                                :sharp (vellumory-bench--wait (funcall shown vellumory-bench-jump))))
              result)))
    (doc-view-kill-proc)
    (kill-buffer buffer)
    (apply #'append (nreverse result))))

(defun vellumory-bench-reader (file lisp)
  "The Emacs Reader (a MuPDF dynamic module, LISP its directory), fit width.
Its commands queue their work; the synchronous ones are timed instead, so that
blocking includes the work, as for the other readers.  It shows one page at a
time: scrolling continues onto the next page as its C-n and SPC do."
  (add-to-list 'load-path lisp)
  (require 'reader)
  (let ((start (float-time)) buffer result
        (lines (lambda (pixels) (max 1 (round pixels (frame-char-height))))))
    (setq buffer (let ((auto-mode-alist (cons '("\\.pdf\\'" . reader-mode) auto-mode-alist))
                       (magic-mode-alist nil))
                   (find-file file)))
    (with-current-buffer buffer
      (reader-fit-to-width)
      (redisplay t)
      (push (list :open-sharp (vellumory-bench--ms start)) result)
      (push (list :jump (list :blocking (vellumory-bench--block (lambda () (reader-goto-page vellumory-bench-jump)))
                              :sharp 0.0))
            result)
      (when (vellumory-bench--scroll-start) (reader-goto-page 1) (redisplay t))
      (push (list :read-scroll
                  (vellumory-bench--scroll
                   (lambda () (reader--non-queue-scroll-down-or-next-page (funcall lines 96))) 40 #'always nil))
            result)
      (push (list :fast-scroll
                  (vellumory-bench--scroll
                   (lambda () (reader--non-queue-scroll-down-or-next-page (funcall lines 300))) 40 #'always nil))
            result))
    (kill-buffer buffer)
    (apply #'append (nreverse result))))

(defun vellumory-bench-all (file pdf-tools-lisp tablist-lisp &optional reader-lisp)
  "Measure every reader on FILE; return a plist of plists."
  (with-temp-buffer (insert-file-contents-literally file)) ; warm the OS cache
  (vellumory-bench--pdf-tools-load pdf-tools-lisp tablist-lisp)
  (delete-other-windows)
  (let ((results
         (list :file (file-name-nondirectory file)
               :window (list (window-body-width nil t) (window-body-height nil t))
               :emacs emacs-version
               :vellumory (vellumory-bench-vellumory file)
               :pdf-tools (vellumory-bench-pdf-tools file nil)
               :pdf-tools-roll (vellumory-bench-pdf-tools file t)
               :doc-view (vellumory-bench-doc-view file)
               :reader (and reader-lisp (vellumory-bench-reader file reader-lisp)))))
    (vellumory-bench--round results)))

(provide 'vellumory-bench)
;;; bench.el ends here
