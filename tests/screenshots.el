;;; screenshots.el --- Scenes for the README's screenshots -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded into a review Emacs by `Review.lean screenshots', which calls
;; `vellumory-shot' with each scene's name and captures the frame once it
;; returns.  Every scene starts from one window and builds its state with the
;; commands a reader would use, so the pictures show what those commands do.

;;; Code:
(require 'vellumory)
(require 'vellumory-edit)
(load (expand-file-name "tests/gui-tests.el" (getenv "VELLUMORY_REVIEW_ROOT")) nil t)

(defvar vellumory-shot-files nil
  "Documents the scenes read, as (NAME . FILE), set by the caller.")

(defun vellumory-shot--file (name)
  "The file the caller gave for NAME."
  (or (alist-get name vellumory-shot-files nil nil #'equal) (error "No document %s" name)))

(defun vellumory-shot--wait (predicate &optional seconds)
  "Process input and output until PREDICATE holds or SECONDS (20) pass."
  (let ((deadline (+ (float-time) (or seconds 20))))
    (while (and (< (float-time) deadline) (not (funcall predicate)))
      (accept-process-output nil 0.05)
      (redisplay t))))

(defun vellumory-shot--open (name)
  "Show document NAME alone in the selected window, rendered sharp."
  (delete-other-windows)
  (find-file (vellumory-shot--file name))
  (vellumory-shot--wait (lambda () vellumory--pages))
  (vellumory-gui--settle))

(defun vellumory-shot--settle-on (buffer)
  "Let BUFFER's document finish rendering, then the frame redisplay."
  (with-current-buffer buffer (vellumory-gui--settle))
  (sit-for 0.5)
  (redisplay t))

(defun vellumory-shot--report (name &optional text)
  "Wait until the buffer named NAME exists and holds text (TEXT, when given)."
  (vellumory-shot--wait (lambda () (let ((b (get-buffer name)))
                                     (and b (> (buffer-size b) 40)
                                          (or (null text) (with-current-buffer b
                                                            (save-excursion (goto-char (point-min))
                                                                            (search-forward text nil t)))))))
                        120))

(defun vellumory-shot (scene)
  "Set up SCENE, a symbol, in the selected frame, with a clear echo area."
  (vellumory-shot-1 scene)
  (message nil)
  (redisplay t))

(defun vellumory-shot-1 (scene)
  "Set up SCENE, a symbol, in the selected frame."
  (pcase scene
    ('reading
     ;; The book's front: a whole page, the previews beside it.
     (vellumory-shot--open "book")
     (vellumory-goto-page 2)
     (vellumory-fit-page)
     (vellumory--show-thumbnails)
     (vellumory-shot--settle-on (current-buffer)))
    ('search
     ;; Continuous pages, previews beside them, a search's matches on the page.
     (vellumory-shot--open "tracemonkey")
     (vellumory-goto-page 2)
     (vellumory--show-thumbnails)
     (select-window (get-buffer-window (vellumory-shot--file-buffer "tracemonkey")))
     (vellumory--search-begin "trace" nil nil)
     (vellumory-shot--wait (lambda () (not vellumory--search-job)) 10)
     (vellumory-shot--settle-on (current-buffer)))
    ('contents
     (vellumory-shot--open "book")
     (vellumory-goto-page 12)
     (vellumory--show-outline)
     (vellumory-shot--settle-on (vellumory-shot--file-buffer "book")))
    ('spread
     (vellumory-shot--open "book")
     (vellumory-goto-page 2)
     (vellumory-cycle-spread)
     (vellumory-shot--settle-on (current-buffer)))
    ('info
     (vellumory-shot--open "book")
     (vellumory-details)
     (vellumory-shot--report (format "*Details: %s*" (file-name-nondirectory (vellumory-shot--file "book"))))
     (vellumory-shot--settle-on (vellumory-shot--file-buffer "book")))
    ('validation
     (vellumory-shot--open "tracemonkey")
     (vellumory-validate "1b")
     ;; veraPDF's verdict, not only the notes found beyond it.
     (vellumory-shot--report (format "*Validation: %s*" (file-name-nondirectory (vellumory-shot--file "tracemonkey")))
                             "validation profile")
     (vellumory-shot--settle-on (vellumory-shot--file-buffer "tracemonkey")))
    ('history
     ;; Rotate, compress, undo the compression, rotate back: a branch.
     (vellumory-shot--open "editable")
     (let ((buffer (current-buffer)))
       (dolist (step (list (lambda () (vellumory-rotate-pages)) #'vellumory-compress #'vellumory-undo
                           (lambda () (vellumory-rotate-pages-counterclockwise))))
         (with-current-buffer buffer
           (let ((before (vellumory-shot--head buffer)))
             (funcall step)
             (vellumory-shot--wait (lambda () (not (equal before (vellumory-shot--head buffer)))) 60))))
       (with-current-buffer buffer (vellumory-history))
       (vellumory-shot--report (format "*History: %s*" (buffer-name buffer)))
       (vellumory-shot--settle-on buffer)))
    ('diff
     (vellumory-shot--open "revised")
     (vellumory-diff (vellumory-shot--file "draft"))
     (vellumory-shot--report (format "*Diff: %s*" (file-name-nondirectory (vellumory-shot--file "revised"))))
     (vellumory-shot--settle-on (vellumory-shot--file-buffer "revised")))
    ('syntax
     (vellumory-shot--open "draft")
     (vellumory-syntax)
     (vellumory-shot--report (format "*Syntax: %s*" (file-name-nondirectory (vellumory-shot--file "draft"))))
     (vellumory-shot--settle-on (vellumory-shot--file-buffer "draft")))
    ('menu
     ;; Every command in one menu, opened with ?.
     (vellumory-shot--open "book")
     (vellumory-goto-page 2)
     (vellumory-fit-page)
     (vellumory-dispatch)
     (sit-for 0.5))
    ('reflow
     ;; Reading mode: a page with a heading, links, a figure and its caption.
     (vellumory-shot--open "book")
     (vellumory-goto-page 101)
     (vellumory-reflow)
     (let ((reflow (window-buffer)))
       (vellumory-shot--wait (lambda () (with-current-buffer reflow
                                          (and vellumory-reflow--first (null vellumory-reflow--target)))))
       (sit-for 0.5)))
    ('preview
     ;; The mouse resting on a link within the book: where it leads.
     (vellumory-shot--open "book")
     (vellumory-goto-page 101)
     (vellumory-gui--settle)
     (let ((window (selected-window)) (link nil) (x 0) (y 0))
       (vellumory-shot--wait (lambda () (vectorp (gethash 100 vellumory--links))))
       ;; Find a link within the book in the window, as the pointer would.
       (pcase-let ((`(,left ,top ,right ,bottom) (window-body-pixel-edges window)))
         (cl-loop for py from (+ top 20) below (- bottom 20) by 4 until link do
                  (cl-loop for px from left below right by 4 until link do
                           (let* ((posn (posn-at-x-y px py (selected-frame)))
                                  (location (vellumory--event-location posn))
                                  (found (and location (apply #'vellumory--link-at location))))
                             (when (and found (natnump (alist-get 'page found)))
                               (setq link found x (+ px 3) y (+ py 2)))))))
       (unless link (error "No link within the book on this page"))
       (set-mouse-pixel-position (selected-frame) x y)
       (vellumory--link-preview-hover window link)
       ;; The scene, not the pointer, decides when it closes.
       (cancel-timer (plist-get vellumory--link-preview :watch))
       (cancel-timer (plist-get vellumory--link-preview :timer))
       (vellumory--link-preview-show window link)
       (vellumory-shot--wait (lambda () (and (frame-live-p vellumory--link-preview-frame)
                                             (frame-visible-p vellumory--link-preview-frame))))
       (sit-for 0.5)))
    (_ (error "Unknown scene %s" scene))))

(defun vellumory-shot-reset ()
  "Close every document and its views, leaving one window."
  (when (bound-and-true-p transient--prefix) (ignore-errors (transient-quit-all)))
  (when (fboundp (quote vellumory-link-preview-hide)) (vellumory-link-preview-hide))
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (string-prefix-p "vellumory-" (symbol-name (buffer-local-value 'major-mode buffer))))
      (vellumory-gui--kill buffer)))
  (delete-other-windows))

(defun vellumory-shot--file-buffer (name)
  "The buffer visiting document NAME."
  (find-buffer-visiting (vellumory-shot--file name)))

(defun vellumory-shot--head (buffer)
  "BUFFER's position in its document's history, as the reader last saw it."
  (with-current-buffer buffer (list vellumory--hash (buffer-modified-tick))))

(provide 'screenshots)
;;; screenshots.el ends here
