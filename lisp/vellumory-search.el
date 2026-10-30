;;; vellumory-search.el --- Asynchronous graphical PDF search -*- lexical-binding: t; -*-

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
;; `vellumory-search' (`/', C-s) finds text with incremental previews: the
;; worker searches one page at a time from the reading position, matches
;; are highlighted on the page, and C-s and C-r in the minibuffer step
;; through them as in isearch.  `vellumory-search-regexp' (C-M-s, or M-r in
;; the minibuffer) matches an Emacs regexp against the page text in Lisp.
;; M-n and M-p visit further matches; M-s c clears the highlights.
;;
;; `vellumory-occur' (M-s o) lists every matching line of the document by
;; page, and `vellumory-occur-documents' does the same across several
;; documents (the marked ones in Dired); RET or `next-error' shows a match.

;;; Code:
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'cl-lib)
(defvar vellumory--pages)
(defvar vellumory--hash)
(defvar vellumory--process)
(defvar vellumory--last-motion)
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--pump "vellumory" ())
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--reading-position "vellumory" (&optional window))
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--note-motion "vellumory" (pixels))
(declare-function vellumory--page-description "vellumory-navigation" (page))
(declare-function vellumory--lines "vellumory-caret" (page))

(defvar vellumory-search-history nil)
(defvar-local vellumory--search-query "")
(defvar-local vellumory--search-case-sensitive nil)
(defvar-local vellumory--search-regexp nil "The query is an Emacs regexp, matched in Lisp.")
(defvar-local vellumory--search-generation 0)
(defvar-local vellumory--search-cache nil)
(defvar-local vellumory--search-cache-order nil)
(defvar-local vellumory--search-current nil)
(defvar-local vellumory--search-job nil)
(defvar-local vellumory--search-status "")
(defvar-local vellumory--search-input-timer nil)

(defun vellumory--quad-points (quad)
  "Return the four corners of QUAD as (X . Y) pairs, in its order."
  (list (cons (alist-get 'x0 quad) (alist-get 'y0 quad))
        (cons (alist-get 'x1 quad) (alist-get 'y1 quad))
        (cons (alist-get 'x2 quad) (alist-get 'y2 quad))
        (cons (alist-get 'x3 quad) (alist-get 'y3 quad))))

(defun vellumory--hit-top (hit)
  "Return the top of HIT, the smallest y of the corners of its quads."
  (apply #'min (cl-loop for quad across hit append (mapcar #'cdr (vellumory--quad-points quad)))))

(defun vellumory--search-page-quads (page)
  "Highlight polygons of PAGE as (QUAD-VECTOR . CURRENT-P)."
  (let ((result (and vellumory--search-cache (not (string-empty-p vellumory--search-query))
                     (gethash page vellumory--search-cache))))
    (when result
      (cl-loop for hit across (alist-get 'hits result) for index from 0
               nconc (let ((current (equal (cons page index) vellumory--search-current)))
                       (cl-loop for quad across hit
                                collect (cons (vector (alist-get 'x0 quad) (alist-get 'y0 quad)
                                                      (alist-get 'x1 quad) (alist-get 'y1 quad)
                                                      (alist-get 'x2 quad) (alist-get 'y2 quad)
                                                      (alist-get 'x3 quad) (alist-get 'y3 quad))
                                              current)))))))

(defvar-local vellumory--search-busy nil "A search request is in flight.")

(defun vellumory--search-redraw ()
  "Sharp tiles depend on highlights; refreshing re-renders those that changed."
  (vellumory--refresh t)
  (force-mode-line-update))

(defun vellumory-search-clear ()
  "Cancel pending search and remove its highlights."
  (interactive)
  (cl-incf vellumory--search-generation)
  (setq vellumory--search-query "" vellumory--search-current nil vellumory--search-regexp nil
        vellumory--search-job nil vellumory--search-cache nil
        vellumory--search-cache-order nil vellumory--search-status "")
  (when (timerp vellumory--search-input-timer) (cancel-timer vellumory--search-input-timer))
  (setq vellumory--search-input-timer nil)
  (vellumory--search-redraw))

(defun vellumory--search-cache-put (page result)
  "Cache the search RESULT of PAGE.
Keep geometry for at most sixteen pages of the current query, dropping the
page stored longest ago."
  (puthash page result vellumory--search-cache)
  (setq vellumory--search-cache-order (cons page (delq page vellumory--search-cache-order)))
  (when (> (length vellumory--search-cache-order) 16)
    (remhash (car (last vellumory--search-cache-order)) vellumory--search-cache)
    (setq vellumory--search-cache-order (butlast vellumory--search-cache-order))))

(defun vellumory--search-found (page index hits wrapped truncated)
  "Make match INDEX of HITS on PAGE the current one and show it.
End the search job, scroll to a little above the match and report it in
the status and the echo area.  WRAPPED notes that the search went around
the document, TRUNCATED that PAGE has more matches than HITS.  The jump
is not continuous motion: the match's page renders sharp at once."
  (setq vellumory--last-motion 0.0)
  (setq vellumory--search-current (cons page index) vellumory--search-job nil
        vellumory--search-status
        (format "Match %d/%d%s · page %s%s" (1+ index) (length hits)
                (if truncated "+" "") (vellumory--page-description page)
                (if wrapped " · wrapped" "")))
  (vellumory--restore-position
   `((page . ,page) (y . ,(max 0 (- (vellumory--hit-top (aref hits index)) 36)))))
  (vellumory--search-redraw)
  (vellumory--refresh t)
  (message "%s" vellumory--search-status))

(defun vellumory--search-step (result)
  "Consume one page RESULT and either select a hit or advance the search."
  (let* ((job vellumory--search-job) (page (plist-get job :page))
         (direction (plist-get job :direction)) (first (plist-get job :first))
         (hits (alist-get 'hits result)) (count (length hits))
         (after (plist-get job :after)) (threshold (plist-get job :y))
         (indices (number-sequence 0 (1- count))) index)
    (when (< direction 0) (setq indices (nreverse indices)))
    (setq index
          (cl-find-if
           (lambda (i)
             (or (not first)
                 (if (integerp after) (if (> direction 0) (> i after) (< i after))
                   (if (> direction 0) (>= (vellumory--hit-top (aref hits i)) threshold)
                     (<= (vellumory--hit-top (aref hits i)) threshold))))) indices))
    (cond
     ;; We cannot silently jump over omitted matches, particularly in reverse.
     ((and (alist-get 'truncated result) (or (null index) (< direction 0)))
      (setq vellumory--search-job nil
            vellumory--search-status (format "Too many matches on page %s; refine the query"
                                             (vellumory--page-description page)))
      (message "%s" vellumory--search-status))
     (index (vellumory--search-found page index hits (plist-get job :wrapped) (alist-get 'truncated result)))
     (t
      (let* ((next (+ page direction)) (total (length vellumory--pages))
             (wrapped (or (plist-get job :wrapped) (< next 0) (>= next total)))
             (remaining (1- (plist-get job :remaining))))
        (if (zerop remaining)
            (progn (setq vellumory--search-job nil vellumory--search-status "No match")
                   (message "No match for %s" vellumory--search-query))
          (setq vellumory--search-job (list :page (mod next total) :direction direction
                                          :first nil :wrapped wrapped :remaining remaining)))))))
  (force-mode-line-update))

(defun vellumory--regexp-page-result (page regexp sensitive)
  "Return the matches of REGEXP on PAGE as the worker reports literal ones.
Each hit has one quad per line it covers.  The page's lines are joined by
newlines, so a regexp may span them; an incomplete one (typed so far)
matches nothing.  SENSITIVE respects case.  After 1000 hits the result is
marked truncated."
  (let* ((lines (vellumory--lines page)) (case-fold-search (not sensitive))
         (places nil) (texts nil) (hits nil) (truncated nil))
    ;; The page text, and for each of its characters (line . index); newlines (line . nil).
    (dotimes (l (length lines))
      (let ((chars (alist-get 'chars (aref lines l))))
        (dotimes (c (length chars))
          (push (aref (aref chars c) 0) texts)
          (dotimes (_ (length (aref (aref chars c) 0))) (push (cons l c) places)))
        (push "\n" texts) (push (cons l nil) places)))
    (let ((text (apply #'concat (nreverse texts))) (places (vconcat (nreverse places))) (start 0))
      (condition-case nil
          (while (and (< start (length text)) (string-match regexp text start) (not truncated))
            (let ((from (match-beginning 0)) (to (match-end 0)) spans)
              (if (= from to) (setq start (1+ to))
                (setq start to)
                ;; The characters matched, by line: each run one quad.
                (cl-loop for i from from below to
                         for (l . c) = (aref places i)
                         when c do (let ((span (assq l spans)))
                                     (if span (setcdr span (list (cadr span) c))
                                       (push (list l c c) spans))))
                (when spans
                  (push (vconcat
                         (mapcar (pcase-lambda (`(,l ,first ,last))
                                   (let* ((line (aref lines l)) (box (alist-get 'box line))
                                          (chars (alist-get 'chars line))
                                          (x0 (aref (aref chars first) 1)) (x1 (aref (aref chars last) 3)))
                                     ;; Around the polygon, as the worker's quads: top left, top
                                     ;; right, bottom right, bottom left.
                                     `((x0 . ,x0) (y0 . ,(aref box 1)) (x1 . ,x1) (y1 . ,(aref box 1))
                                       (x2 . ,x1) (y2 . ,(aref box 3)) (x3 . ,x0) (y3 . ,(aref box 3)))))
                                 (nreverse spans)))
                        hits)
                  (when (> (length hits) 1000) (setq truncated t))))))
        (invalid-regexp nil)))
    `((hits . ,(vconcat (nreverse hits))) (truncated . ,truncated) (page . ,page))))

(defun vellumory--search-pump ()
  "Search at most one uncached page at a time on the worker's document thread."
  (when (and (not vellumory--search-busy) vellumory--search-job (process-live-p vellumory--process))
    (while (and vellumory--search-job (not vellumory--search-busy))
      (let* ((page (plist-get vellumory--search-job :page))
             (cached (gethash page vellumory--search-cache)))
        (cond
         (cached (vellumory--search-step cached))
         ;; A regexp is matched here, in the page's characters from the worker.
         (vellumory--search-regexp
          ;; Fetching the page's characters waits for the worker, and timers
          ;; run meanwhile: busy, so none of them pumps this search again.
          (let ((generation vellumory--search-generation) (job vellumory--search-job) result)
            (setq vellumory--search-busy t)
            (unwind-protect
                (setq result (vellumory--regexp-page-result page vellumory--search-query
                                                            vellumory--search-case-sensitive))
              (setq vellumory--search-busy nil))
            (when (and (= generation vellumory--search-generation) (eq job vellumory--search-job))
              (vellumory--search-cache-put page result)
              (vellumory--search-step result)
              (vellumory--search-redraw))))
         (t
          (let ((generation vellumory--search-generation) (hash vellumory--hash)
                (query vellumory--search-query))
            (setq vellumory--search-busy t
                  vellumory--search-status (format "Searching page %s…" (vellumory--page-description page)))
            (force-mode-line-update)
            (vellumory--rpc
             "search" `((page . ,page) (query . ,query) (case_sensitive . ,(if vellumory--search-case-sensitive t :false)))
             (lambda (reply)
               (setq vellumory--search-busy nil)
               (when (and (= generation vellumory--search-generation) (equal hash vellumory--hash))
                 (let ((result (alist-get 'result reply)))
                   (if-let* ((error (alist-get 'error reply)))
                       (progn (setq vellumory--search-job nil vellumory--search-status error)
                              (message "Vellumory search: %s" error))
                     (when (and (equal hash (alist-get 'hash result))
                                (equal query (alist-get 'query result)) (= page (alist-get 'page result)))
                       (vellumory--search-cache-put page result)
                       (vellumory--search-step result)
                       (vellumory--search-redraw)))))
               (force-mode-line-update)
               (vellumory--pump))))))))))

(defun vellumory--search-move (direction &optional origin)
  "Look for the next match in DIRECTION, 1 forward or -1 backward.
Search from ORIGIN (a reading position) when given, else after the current
match, else from the reading position.  `vellumory--pump' then searches
page by page, wrapping around the document."
  (unless vellumory--pages (user-error "PDF is still opening"))
  (when (string-empty-p vellumory--search-query) (user-error "Start a search with / first"))
  (cl-incf vellumory--search-generation)
  (let* ((position (or origin (vellumory--reading-position) '((page . 0) (y . 0))))
         (current (and (not origin) vellumory--search-current))
         (page (max 0 (min (1- (length vellumory--pages))
                           (if current (car current) (alist-get 'page position))))))
    (setq vellumory--search-job
          (list :page page :direction direction :first t :remaining (1+ (length vellumory--pages))
                :after (cdr current) :y (or (alist-get 'y position) 0)))
    (vellumory--pump)))

(defun vellumory-search-next ()
  "Visit the next text match, wrapping at the end of the document."
  (interactive) (vellumory--search-move 1))

(defun vellumory-search-previous ()
  "Visit the previous text match, wrapping at the start of the document."
  (interactive) (vellumory--search-move -1))

(defun vellumory-search-quit ()
  "Stop pending PDF search work before quitting the current command."
  (interactive)
  (vellumory-search-clear)
  (keyboard-quit))

(defun vellumory--search-begin (query sensitive origin &optional regexp)
  "Search for QUERY from ORIGIN, unless that search is already shown.
SENSITIVE respects case; REGEXP matches QUERY as an Emacs regexp.  An empty
QUERY clears the search and returns to ORIGIN."
  (unless (and (equal query vellumory--search-query) (eq sensitive vellumory--search-case-sensitive)
               (eq regexp vellumory--search-regexp))
    (vellumory-search-clear)
    (if (string-empty-p query)
        (progn (vellumory--restore-position origin) (vellumory--refresh t))
      (setq vellumory--search-query query vellumory--search-case-sensitive sensitive
            vellumory--search-regexp regexp
            vellumory--search-cache (make-hash-table :test #'eql))
      (vellumory--search-move 1 origin))))

(defvar-local vellumory--search-session nil
  "The search this minibuffer reads, as (READER SENSITIVE ORIGIN REGEXP).
The reader searched, whether case matters, the starting reading position
and whether the input is a regexp.")

(defun vellumory--search-minibuffer-step (direction)
  "Step through matches of the minibuffer input in DIRECTION (1 or -1).
Like isearch's \\<isearch-mode-map>\\[isearch-repeat-forward] and \\[isearch-repeat-backward]: the first match of new input, then the
next ones.  With empty input, bring back the previous search."
  (pcase-let ((`(,source ,sensitive ,origin ,regexp) vellumory--search-session)
              (input (minibuffer-contents-no-properties)))
    (cond ((string-empty-p input)
           (when-let* ((last (car vellumory-search-history))) (insert last)))
          ((buffer-live-p source)
           (with-current-buffer source
             (if (and (equal input vellumory--search-query) (eq sensitive vellumory--search-case-sensitive)
                      (eq regexp vellumory--search-regexp))
                 (vellumory--search-move direction)
               (vellumory--search-begin input sensitive origin regexp)))))))

(defun vellumory-search-minibuffer-toggle-regexp ()
  "Match the input as an Emacs regexp, or as text again.
This is \\<vellumory-search-minibuffer-map>\\[vellumory-search-minibuffer-toggle-regexp] in the search minibuffer, as in isearch."
  (interactive)
  (setf (nth 3 vellumory--search-session) (not (nth 3 vellumory--search-session)))
  (pcase-let ((`(,source ,sensitive ,origin ,regexp) vellumory--search-session))
    (message (if regexp "Matching a regexp" "Matching text"))
    (when (buffer-live-p source)
      (with-current-buffer source
        (vellumory--search-begin (with-current-buffer (window-buffer (minibuffer-window))
                                   (minibuffer-contents-no-properties))
                                 sensitive origin regexp)))))

(defun vellumory-search-minibuffer-next () "Next match." (interactive) (vellumory--search-minibuffer-step 1))
(defun vellumory-search-minibuffer-previous () "Previous match." (interactive)
  (vellumory--search-minibuffer-step -1))

(defvar vellumory-search-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-s" #'vellumory-search-minibuffer-next)
    (keymap-set map "C-r" #'vellumory-search-minibuffer-previous)
    (keymap-set map "M-r" #'vellumory-search-minibuffer-toggle-regexp)
    map)
  "Keys added to the minibuffer while finding text.")

(defun vellumory-search (&optional sensitive regexp)
  "Find text with incremental previews.  With prefix SENSITIVE, respect case.
RET keeps the result; \\<global-map>\\[keyboard-quit] restores the original reading position.  In the
minibuffer \\<vellumory-search-minibuffer-map>\\[vellumory-search-minibuffer-next] and \\[vellumory-search-minibuffer-previous] step through matches, as in isearch, and \\[vellumory-search-minibuffer-toggle-regexp] switches
between text and an Emacs REGEXP (\\<vellumory-mode-map>\\[vellumory-search-regexp] starts with one).  Use \\[vellumory-search-next] and \\[vellumory-search-previous]
for subsequent matches, and \\[vellumory-search-clear] to clear highlights.
This searches the extracted text (OCR it first for scans)."
  (interactive "P")
  (unless vellumory--pages (user-error "PDF is still opening"))
  (let* ((source (current-buffer)) (origin (vellumory--reading-position))
         (sensitive (and sensitive t)) (session (list source sensitive origin regexp)) committed query)
    (vellumory-search-clear)
    (unwind-protect
        (progn
          (setq query
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq vellumory--search-session session)
                      (use-local-map (make-composed-keymap vellumory-search-minibuffer-map (current-local-map)))
                      (add-hook
                       'post-command-hook
                       (lambda ()
                         (let ((input (minibuffer-contents-no-properties)))
                           (when (buffer-live-p source)
                             (with-current-buffer source
                               (when (timerp vellumory--search-input-timer)
                                 (cancel-timer vellumory--search-input-timer))
                               (setq vellumory--search-input-timer
                                     (run-with-timer
                                      .15 nil
                                      (lambda ()
                                        (when (buffer-live-p source)
                                          (with-current-buffer source
                                            (vellumory--search-begin input sensitive origin (nth 3 session)))))))))))
                       nil t))
                  (read-string (format "Find %s%s: " (if regexp "regexp" "text") (if sensitive " (case sensitive)" ""))
                               nil 'vellumory-search-history)))
          (setq committed t)
          (with-current-buffer source (vellumory--search-begin query sensitive origin (nth 3 session))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (when (timerp vellumory--search-input-timer) (cancel-timer vellumory--search-input-timer))
          (setq vellumory--search-input-timer nil)
          (unless committed
            (vellumory-search-clear)
            (vellumory--restore-position origin)
            (vellumory--refresh t)))))))

(defun vellumory-search-regexp (&optional sensitive)
  "Find matches of an Emacs regexp as \\[vellumory-search] finds text.
With prefix SENSITIVE, respect case."
  (interactive "P")
  (vellumory-search sensitive t))

;;;; Occur: every matching line of the document, by page

(defvar vellumory-program)
(defvar vellumory--source-file)
(defvar vellumory--file)
(defvar-local vellumory--document-text nil "(HASH . vector of page texts), once fetched.")
(defvar-local vellumory--occur-source nil)
(defcustom vellumory-occur-context 40
  "Characters of text shown before a match in `vellumory-occur'.
Twice as many are shown after it."
  :type 'natnum :group 'vellumory)

(defun vellumory--with-document-text (callback)
  "Call CALLBACK with a vector of every page's text, fetched once per content."
  (if (equal (car vellumory--document-text) vellumory--hash)
      (funcall callback (cdr vellumory--document-text))
    (let ((source (current-buffer)) (hash vellumory--hash)
          (output (generate-new-buffer " *vellumory-text*")))
      (let ((default-directory (vellumory--existing-directory)))
        (make-process
         :name "vellumory-text" :buffer output :noquery t :connection-type 'pipe
         :command (list vellumory-program "text" (or vellumory--source-file vellumory--file))
         :stderr (get-buffer-create " *vellumory-text-errors*")
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((pages (with-current-buffer output
                            (vconcat (split-string (decode-coding-string (buffer-string) 'utf-8) "\f")))))
               (kill-buffer output)
               (if (not (zerop (process-exit-status process)))
                   (message "Vellumory: the text could not be extracted")
                 (when (buffer-live-p source)
                   (with-current-buffer source
                     (setq vellumory--document-text (cons hash pages))
                     (funcall callback pages))))))))))))

(defun vellumory--occur-matches (pages regexp)
  "Lines of PAGES matching REGEXP: (PAGE LINE START END) each, in order.
Case folds unless REGEXP has upper case, as `occur' decides."
  (let ((case-fold-search (and case-fold-search (isearch-no-upper-case-p regexp t))) matches)
    (dotimes (page (length pages))
      (dolist (line (split-string (aref pages page) "\n" t "[ \t]+"))
        (let ((start 0))
          (while (and (< start (length line)) (string-match regexp line start))
            (push (list page line (match-beginning 0) (match-end 0)) matches)
            (setq start (max (1+ (match-beginning 0)) (match-end 0)))))))
    (nreverse matches)))

(defun vellumory--occur-insert (page label line start end file)
  "Insert a match of LINE from START to END on PAGE (shown as LABEL), in context.
FILE names its document when the list spans several."
  ;; Extracted text has a paragraph per line: show the match in context.
  (let* ((from (max 0 (- start vellumory-occur-context)))
         (to (min (length line) (+ end (* 2 vellumory-occur-context))))
         (text (concat (if (> from 0) "…" "") (substring line from to) (if (< to (length line)) "…" "")))
         (offset (- (if (> from 0) 1 0) from)))
    (add-face-text-property (+ start offset) (+ end offset) 'match nil text)
    (insert (propertize (format "%7s: " (concat "p. " label)) 'face 'shadow)
            (propertize text 'vellumory-occur (list page (substring line start end) file))
            "\n")))

(defun vellumory--occur-render (source regexp matches)
  "Fill this buffer with MATCHES of REGEXP in the reader SOURCE.
A header counts the matches and their pages; point goes to the first."
  (let ((inhibit-read-only t) (pages (length (delete-dups (mapcar #'car matches)))))
    (erase-buffer)
    (insert (propertize (format "%d matches for “%s” on %d pages\n" (length matches) regexp pages)
                        'face 'bold))
    (pcase-dolist (`(,page ,line ,start ,end) matches)
      (vellumory--occur-insert page (with-current-buffer source (vellumory--page-description page))
                               line start end nil))
    (goto-char (point-min))
    (forward-line 1)))

(defun vellumory--file-pages (file callback)
  "Call CALLBACK with FILE's page texts (a vector), or nil if it has none:
from a reader showing it, else from the command line."
  (let* ((truename (file-truename file))
         (open (seq-find (lambda (buffer)
                           (with-current-buffer buffer
                             (and (derived-mode-p 'vellumory-mode) vellumory--hash
                                  (equal vellumory--source-file truename))))
                         (buffer-list))))
    (if open
        (with-current-buffer open (vellumory--with-document-text callback))
      (let ((output (generate-new-buffer " *vellumory-text*")))
        (let ((default-directory (vellumory--existing-directory)))
          (make-process
           :name "vellumory-text" :buffer output :noquery t :connection-type 'pipe
           :command (list vellumory-program "text" (expand-file-name file))
           :stderr (get-buffer-create " *vellumory-text-errors*")
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let ((pages (and (zerop (process-exit-status process))
                                 (with-current-buffer output
                                   (vconcat (split-string (decode-coding-string (buffer-string) 'utf-8) "\f"))))))
                 (kill-buffer output)
                 (funcall callback pages))))))))))

(defun vellumory--occur-files ()
  "Documents to search: Dired's marked files there, else every open document."
  (or (if (derived-mode-p 'dired-mode)
          (seq-filter #'file-regular-p (dired-get-marked-files))
        (delete-dups (delq nil (mapcar (lambda (buffer)
                                         (with-current-buffer buffer
                                           (and (derived-mode-p 'vellumory-mode) vellumory--source-file)))
                                       (buffer-list)))))
      (user-error "No documents to search")))

;;;###autoload
(defun vellumory-occur-documents (regexp files)
  "List every line matching REGEXP in each of FILES, by document and page.
In Dired the marked files (`; o'), elsewhere every open document
\(\\[universal-argument] \\<vellumory-mode-map>\\[vellumory-occur] in a reader), as pdf-tools\\=' `pdf-occur' over several documents.
Text comes from an open reader when there is one; four documents are read
at a time.  RET or \\<global-map>\\[next-error] opens a match\\='s document there, highlighted."
  (interactive (let ((files (vellumory--occur-files)))
                 (list (read-regexp (format "List lines matching in %d documents" (length files))) files)))
  (let* ((files (vconcat files)) (results (make-vector (length files) nil))
         (remaining (length files)) (queue (number-sequence 0 (1- (length files)))) (running 0))
    (cl-labels ((start ()
                  (while (and queue (< running 4))
                    (let ((index (pop queue)))
                      (cl-incf running)
                      (vellumory--file-pages
                       (aref files index)
                       (lambda (pages)
                         (cl-decf running)
                         (aset results index (and pages (vellumory--occur-matches pages regexp)))
                         (if (zerop (cl-decf remaining)) (show) (start)))))))
                (show ()
                  (let ((total (apply #'+ (mapcar #'length results)))
                        (found (seq-count #'identity results)))
                    (if (zerop total) (message "No lines match %s in %d documents" regexp (length files))
                      (with-current-buffer (get-buffer-create "*Occur: documents*")
                        (vellumory-occur-mode)
                        (setq vellumory--occur-source nil)
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (insert (propertize (format "%d matches for “%s” in %d of %d documents\n"
                                                      total regexp found (length files))
                                              'face 'bold))
                          (dotimes (index (length files))
                            (when-let* ((matches (aref results index)))
                              (insert "\n" (propertize (abbreviate-file-name (aref files index)) 'face 'bold)
                                      (propertize (format "  %d" (length matches)) 'face 'shadow) "\n")
                              (pcase-dolist (`(,page ,line ,start ,end) matches)
                                (vellumory--occur-insert page (number-to-string (1+ page)) line start end
                                                         (aref files index)))))
                          (goto-char (point-min))
                          (forward-line 1))
                        (setq next-error-last-buffer (current-buffer))
                        (display-buffer (current-buffer)))
                      (message "%d matches in %d documents" total found)))))
      (if (zerop remaining) (user-error "No documents to search") (start)))))

(defun vellumory-occur-goto (&optional pos)
  "Show the match at POS in the reader, highlighted."
  (interactive)
  (let ((match (save-excursion
                 (goto-char (or pos (point)))
                 ;; The match text follows the page column, anywhere on the line.
                 (let ((start (next-single-property-change (line-beginning-position) 'vellumory-occur nil
                                                           (line-end-position))))
                   (or (get-text-property (line-beginning-position) 'vellumory-occur)
                       (and start (get-text-property start 'vellumory-occur))))))
        (source vellumory--occur-source))
    (unless match (user-error "No match on this line"))
    ;; A list over several documents opens the match's own.
    (when-let* ((file (nth 2 match)))
      (setq source (vellumory-open file t))
      (with-timeout (10 (user-error "%s did not open" (file-name-nondirectory file)))
        (while (not (buffer-local-value 'vellumory--pages source)) (accept-process-output nil 0.02))))
    (unless (buffer-live-p source) (user-error "The document has been closed"))
    (pop-to-buffer source)
    ;; The exact matched text, from the top of its page.
    (with-current-buffer source
      (vellumory-search-clear)
      (vellumory--search-begin (cadr match) t `((page . ,(car match)) (y . 0))))))

(defun vellumory--occur-next-error (count reset)
  "Show the COUNTth next match in the reader, as `next-error-function'.
A negative COUNT moves back; RESET counts from the start of the buffer.
Lines without a match are skipped."
  (when reset (goto-char (point-min)))
  (forward-line (if (and reset (> count 0)) (1- count) count))
  (while (and (not (eobp)) (not (bobp))
              (not (next-single-property-change (line-beginning-position) 'vellumory-occur nil
                                                (line-end-position))))
    (forward-line (if (< count 0) -1 1)))
  (vellumory-occur-goto))

(defvar vellumory-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'vellumory-occur-goto)
    (keymap-set map "n" #'next-error-no-select)
    (keymap-set map "p" #'previous-error-no-select)
    map))

(define-derived-mode vellumory-occur-mode special-mode "PDF Occur"
  "Lines of a document matching a regexp.
\\{vellumory-occur-mode-map}"
  (setq-local next-error-function #'vellumory--occur-next-error truncate-lines t))

;;;###autoload
(defun vellumory-occur (regexp &optional all)
  "List every line of the document matching REGEXP, by page.
RET or \\[next-error] shows a match in the reader, highlighted.  With a prefix
argument (ALL), every open document: `vellumory-occur-documents'."
  (interactive (if current-prefix-arg (list nil t) (list (read-regexp "List lines matching") nil)))
  (if all (call-interactively #'vellumory-occur-documents) (vellumory--occur-document regexp)))

(defun vellumory--occur-document (regexp)
  "List the lines of this document matching REGEXP."
  (unless vellumory--pages (user-error "PDF is still opening"))
  (let ((source (current-buffer)) (name (format "*Occur: %s*" (buffer-name))))
    (vellumory--with-document-text
     (lambda (pages)
       (let ((matches (vellumory--occur-matches pages regexp)) (buffer nil))
         (if (null matches)
             (message "No lines match %s" regexp)
           (setq buffer (get-buffer-create name))
           (with-current-buffer buffer
             (vellumory-occur-mode)
             (setq vellumory--occur-source source)
             (vellumory--occur-render source regexp matches))
           (setq next-error-last-buffer buffer)
           (display-buffer buffer)
           (message "%d matches" (length matches))))))))

(provide 'vellumory-search)
;;; vellumory-search.el ends here
