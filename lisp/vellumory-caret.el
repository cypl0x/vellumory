;;; vellumory-caret.el --- A keyboard caret in the page text -*- lexical-binding: t; -*-

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
;; `vellumory-caret-mode' (v, or the menu on ?; caret browsing as in Evince and Firefox)
;; puts a caret into the text of the page in view.  It moves by character
;; (h l, arrows, C-b C-f), line (j k, C-n C-p, keeping its column), word (w b e,
;; M-f M-b), to line ends (0 $, C-a C-e) and blocks ({ }, M-{ M-}), across
;; pages, and the view follows it.  v (or C-SPC) starts a selection at the
;; caret, V selects whole lines, o swaps the ends, shifted arrows extend; y or
;; M-w copies it.  q, Escape or C-g leave.  In the reader, v starts selecting
;; at once, as evil's visual state does.  Characters, their order and their
;; boundaries come from MuPDF's structured text, as for mouse selection.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar vellumory--hash)
(defvar vellumory--pages)
(defvar vellumory--process)
(defvar vellumory--scale)
(defvar vellumory-caret-mode)
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--refresh "vellumory" (&optional force))
(declare-function vellumory--restore-position "vellumory" (position &optional window))
(declare-function vellumory--reading-position "vellumory" (&optional window))
(declare-function vellumory--offset-of "vellumory" (page y))
(declare-function vellumory--absolute-position "vellumory" (&optional window))
(declare-function vellumory--points-per-pixel "vellumory" (page))
(declare-function vellumory--rect-quad "vellumory-select" (x0 y0 x1 y1))
(declare-function vellumory--select-range "vellumory-select" (start end mode callback))
(declare-function vellumory--set-selection "vellumory-select" (selections))
(declare-function vellumory-clear-selection "vellumory-select" ())
(declare-function vellumory-copy-selection "vellumory-select" ())

(defvar-local vellumory--chars nil "(HASH . table of page → vector of lines, or `pending').")
(defvar-local vellumory-caret--pos nil "The caret: (PAGE LINE INDEX); INDEX may be the line's length.")
(defvar-local vellumory-caret--anchor nil "The other end of the selection, or nil.")
(defvar-local vellumory-caret--unit 'chars "Selection by `chars' or whole `lines'.")
(defvar-local vellumory-caret--goal nil "Column (x in points) kept by vertical moves.")
(defvar-local vellumory-caret--selecting nil "Generation of a selection request still out.")

;;;; Characters

(defun vellumory--page-lines (page)
  "PAGE's lines once known, else nil; the first call asks the worker."
  (unless (equal (car vellumory--chars) vellumory--hash)
    (setq vellumory--chars (cons vellumory--hash (make-hash-table :test #'eql))))
  (let ((known (gethash page (cdr vellumory--chars))))
    (cond ((vectorp known) known)
          ((eq known 'pending) nil)
          (t (puthash page 'pending (cdr vellumory--chars))
             (let ((buffer (current-buffer)) (hash vellumory--hash))
               (vellumory--rpc "chars" `((page . ,page))
                               (lambda (reply)
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (when (equal hash (car vellumory--chars))
                                       (puthash page (or (alist-get 'lines (alist-get 'result reply)) [])
                                                (cdr vellumory--chars))))))))
             nil))))

(defun vellumory--lines (page)
  "PAGE's lines, waiting for the worker the first time (it answers in ms)."
  (or (vellumory--page-lines page)
      (let ((deadline (+ (float-time) 3)))
        (while (and (not (vellumory--page-lines page)) (< (float-time) deadline))
          (accept-process-output vellumory--process 0.01))
        (or (vellumory--page-lines page) []))))

(defun vellumory-caret--line (pos) "Return the line POS is on." (aref (vellumory--lines (nth 0 pos)) (nth 1 pos)))
(defun vellumory-caret--chars (pos) "Return the characters of POS's line." (alist-get 'chars (vellumory-caret--line pos)))

(defun vellumory-caret--char (pos)
  "The character at POS; a line's end reads as a newline."
  (let ((chars (vellumory-caret--chars pos)))
    (if (< (nth 2 pos) (length chars)) (aref (aref chars (nth 2 pos)) 0) "\n")))

(defun vellumory-caret--class (char)
  "Return the class of the string CHAR: `space', `word' or `punct'."
  (cond ((string-match-p "\\`[[:space:]]+\\'" char) 'space)
        ((string-match-p "\\`[[:alnum:]_]+\\'" char) 'word)
        (t 'punct)))

(defun vellumory-caret--before-p (a b)
  "Whether position A comes before B in reading order."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
           (or (< (nth 1 a) (nth 1 b)) (and (= (nth 1 a) (nth 1 b)) (< (nth 2 a) (nth 2 b)))))))

;;;; Positions

(defun vellumory-caret--next-line (pos)
  "The start of the line after POS's, crossing pages; nil at the end."
  (pcase-let ((`(,page ,line ,_) pos))
    (if (< (1+ line) (length (vellumory--lines page))) (list page (1+ line) 0)
      (cl-loop for next from (1+ page) below (length vellumory--pages)
               when (> (length (vellumory--lines next)) 0) return (list next 0 0)))))

(defun vellumory-caret--previous-line (pos)
  "The start of the line before POS's, crossing pages; nil at the start."
  (pcase-let ((`(,page ,line ,_) pos))
    (if (> line 0) (list page (1- line) 0)
      (cl-loop for previous downfrom (1- page) to 0
               for lines = (vellumory--lines previous)
               when (> (length lines) 0) return (list previous (1- (length lines)) 0)))))

(defun vellumory-caret--line-end (pos) "Return the end of POS's line." (list (nth 0 pos) (nth 1 pos) (length (vellumory-caret--chars pos))))
(defun vellumory-caret--line-start (pos) "Return the start of POS's line." (list (nth 0 pos) (nth 1 pos) 0))

(defun vellumory-caret--forward (pos)
  "The position after POS, or nil at the end of the document."
  (if (< (nth 2 pos) (length (vellumory-caret--chars pos)))
      (list (nth 0 pos) (nth 1 pos) (1+ (nth 2 pos)))
    (vellumory-caret--next-line pos)))

(defun vellumory-caret--backward (pos)
  "The position before POS, or nil at the start of the document."
  (if (> (nth 2 pos) 0)
      (list (nth 0 pos) (nth 1 pos) (1- (nth 2 pos)))
    (when-let* ((previous (vellumory-caret--previous-line pos)))
      (vellumory-caret--line-end previous))))

(defun vellumory-caret--x (pos)
  "The x of POS: its character's start, or the end of its line."
  (let ((chars (vellumory-caret--chars pos)) (i (nth 2 pos)))
    (cond ((< i (length chars)) (aref (aref chars i) 1))
          ((> (length chars) 0) (aref (aref chars (1- (length chars))) 3))
          (t (aref (alist-get 'box (vellumory-caret--line pos)) 0)))))

(defun vellumory-caret--at-column (pos x)
  "The position on POS's line nearest to column X."
  (let ((best 0) (distance nil) (n (length (vellumory-caret--chars pos))))
    (dotimes (i (1+ n))
      (let ((d (abs (- x (vellumory-caret--x (list (nth 0 pos) (nth 1 pos) i))))))
        (when (or (null distance) (< d distance)) (setq best i distance d))))
    (list (nth 0 pos) (nth 1 pos) best)))

(defun vellumory-caret--forward-word (pos)
  "The start of the next word after POS, as evil's w moves."
  (let ((class (vellumory-caret--class (vellumory-caret--char pos))) (last pos))
    (unless (eq class 'space)
      (while (and pos (eq (vellumory-caret--class (vellumory-caret--char pos)) class))
        (setq last pos pos (vellumory-caret--forward pos))))
    (while (and pos (eq (vellumory-caret--class (vellumory-caret--char pos)) 'space))
      (setq last pos pos (vellumory-caret--forward pos)))
    (or pos last)))

(defun vellumory-caret--backward-word (pos)
  "The start of the word before POS, as evil's b moves."
  (let ((at (vellumory-caret--backward pos)))
    (while (and at (eq (vellumory-caret--class (vellumory-caret--char at)) 'space))
      (setq at (vellumory-caret--backward at)))
    (if (null at) (list (nth 0 pos) (nth 1 pos) 0)
      (let ((class (vellumory-caret--class (vellumory-caret--char at))))
        (while (let ((before (vellumory-caret--backward at)))
                 (and before (eq (vellumory-caret--class (vellumory-caret--char before)) class)))
          (setq at (vellumory-caret--backward at)))
        at))))

(defun vellumory-caret--end-of-word (pos)
  "The last character of the word at or after POS, as evil's e moves."
  (let ((at (or (vellumory-caret--forward pos) pos)))
    (while (and at (eq (vellumory-caret--class (vellumory-caret--char at)) 'space)
                (vellumory-caret--forward at))
      (setq at (vellumory-caret--forward at)))
    (let ((class (vellumory-caret--class (vellumory-caret--char at))))
      (while (let ((next (vellumory-caret--forward at)))
               (and next (eq (vellumory-caret--class (vellumory-caret--char next)) class)))
        (setq at (vellumory-caret--forward at)))
      at)))

(defun vellumory-caret--block (pos) "Return the block of text POS's line belongs to." (alist-get 'block (vellumory-caret--line pos)))

(defun vellumory-caret--next-block (pos)
  "The start of the next block (paragraph, column piece) after POS's."
  (let ((block (vellumory-caret--block pos)) (page (nth 0 pos)) (at (vellumory-caret--next-line pos)))
    (while (and at (= (nth 0 at) page) (eql (vellumory-caret--block at) block))
      (setq at (vellumory-caret--next-line at)))
    (or at (vellumory-caret--line-end pos))))

(defun vellumory-caret--previous-block (pos)
  "The start of POS's block, or of the block before when already there."
  (cl-flet ((block-start (at)
              (let ((block (vellumory-caret--block at)) (page (nth 0 at)))
                (while (when-let* ((before (and (> (nth 1 at) 0) (vellumory-caret--previous-line at))))
                         (and (= (nth 0 before) page) (eql (vellumory-caret--block before) block)
                              (setq at before))))
                (vellumory-caret--line-start at))))
    (let ((start (block-start pos)))
      (if (and (equal start pos) (vellumory-caret--previous-line pos))
          (block-start (vellumory-caret--previous-line pos))
        start))))

;;;; Selection and display

(defun vellumory-caret--start-point (pos)
  "(PAGE X Y) where POS's character starts, or the end of its line."
  (let ((chars (vellumory-caret--chars pos)) (i (nth 2 pos)))
    (cond ((< i (length chars)) (let ((c (aref chars i))) (list (nth 0 pos) (aref c 1) (aref c 2))))
          ((> (length chars) 0) (let ((c (aref chars (1- (length chars))))) (list (nth 0 pos) (aref c 3) (aref c 4))))
          (t (let ((box (alist-get 'box (vellumory-caret--line pos))))
               (list (nth 0 pos) (aref box 0) (/ (+ (aref box 1) (aref box 3)) 2)))))))

(defun vellumory-caret--end-point (pos)
  "(PAGE X Y) where POS's character ends: selections include the caret's."
  (let ((chars (vellumory-caret--chars pos)) (i (nth 2 pos)))
    (if (< i (length chars))
        (let ((c (aref chars i))) (list (nth 0 pos) (aref c 3) (aref c 4)))
      (vellumory-caret--start-point pos))))

(defun vellumory-caret--range ()
  "The selected range as (FIRST LAST) positions, both included, or nil."
  (when (and vellumory-caret--anchor vellumory-caret--pos)
    (let* ((a vellumory-caret--anchor) (b vellumory-caret--pos)
           (first (if (vellumory-caret--before-p b a) b a)) (last (if (eq first a) b a)))
      (if (eq vellumory-caret--unit 'lines)
          (list (vellumory-caret--line-start first)
                (let ((end (vellumory-caret--line-end last)))
                  (if (> (nth 2 end) 0) (list (nth 0 end) (nth 1 end) (1- (nth 2 end))) end)))
        (list first last)))))

(defun vellumory-caret--select ()
  "Ask for the selection between anchor and caret; clear it without an anchor."
  ;; The header and mode lines name the caret's state; no text changed to
  ;; make Emacs redraw them.
  (force-mode-line-update)
  (if-let* ((range (vellumory-caret--range)))
      (let ((generation (setq vellumory-caret--selecting (1+ (or vellumory-caret--selecting 0))))
            (buffer (current-buffer)))
        (vellumory--select-range (vellumory-caret--start-point (car range))
                                 (vellumory-caret--end-point (cadr range)) 'chars
                                 (lambda (selections)
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (when (eql generation vellumory-caret--selecting)
                                         (setq vellumory-caret--selecting nil)
                                         (vellumory--set-selection selections)))))))
    (setq vellumory-caret--selecting nil)
    (vellumory-clear-selection)))

(defun vellumory--caret-quads (page)
  "The caret as a thin bar on PAGE, or nil."
  (when-let* ((pos vellumory-caret--pos) ((= (car pos) page))
              (lines (vellumory--page-lines page)) ((< (nth 1 pos) (length lines))))
    (let* ((box (alist-get 'box (aref lines (nth 1 pos))))
           (x (vellumory-caret--x pos)) (y0 (aref box 1)) (y1 (aref box 3))
           ;; At least 1.5 device pixels at any zoom, so it never blurs away.
           (width (max (* 1.5 (/ (vellumory--points-per-pixel page) (max 1.0 vellumory--scale)))
                       (* 0.07 (- y1 y0)))))
      (list (vellumory--rect-quad (- x (/ width 2)) y0 (+ x (/ width 2)) y1)))))

(defun vellumory-caret--indicator ()
  "The caret's state and keys for the header line; Doom hides mode lighters."
  (cond ((not vellumory-caret-mode) "")
        ((not vellumory-caret--anchor) " | CARET  v select  y copy  q leave")
        ((eq vellumory-caret--unit 'lines) " | VISUAL LINE  y copy  o swap  q leave")
        (t " | VISUAL  y copy  o swap  V lines  q leave")))

(defun vellumory-caret--reveal ()
  "Scroll so the caret's line is in view, a third from the top when moved."
  (when-let* ((pos vellumory-caret--pos) (window (get-buffer-window (current-buffer))))
    (let* ((page (nth 0 pos)) (box (alist-get 'box (vellumory-caret--line pos)))
           (top (vellumory--absolute-position window)) (height (window-body-height window t))
           (y0 (vellumory--offset-of page (aref box 1))) (y1 (vellumory--offset-of page (aref box 3))))
      (when (or (< y0 top) (> y1 (+ top height)))
        (vellumory--restore-position
         `((page . ,page) (y . ,(max 0 (- (aref box 1) (* (vellumory--points-per-pixel page) (/ height 3))))))
         window)))))

(defun vellumory-caret--move (pos &optional keep-goal)
  "Put the caret at POS, keeping the column for vertical moves when KEEP-GOAL."
  (unless pos (user-error "No more text"))
  (unless keep-goal (setq vellumory-caret--goal nil))
  (setq vellumory-caret--pos pos)
  (vellumory-caret--reveal)
  (vellumory-caret--select)
  (vellumory--refresh t))

;;;; Commands

(defmacro vellumory-caret--define (name doc form &optional keep-goal)
  "Define caret motion NAME (DOC) moving to FORM, where `pos' is the caret.
With KEEP-GOAL, the motion keeps the column of vertical moves."
  (declare (indent 1))
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((pos vellumory-caret--pos)) (vellumory-caret--move ,form ,keep-goal))))

(vellumory-caret--define vellumory-caret-forward-char "Caret to the next character."
  (vellumory-caret--forward pos))
(vellumory-caret--define vellumory-caret-backward-char "Caret to the previous character."
  (vellumory-caret--backward pos))
(vellumory-caret--define vellumory-caret-forward-word "Caret to the start of the next word."
  (vellumory-caret--forward-word pos))
(vellumory-caret--define vellumory-caret-backward-word "Caret to the start of this or the previous word."
  (vellumory-caret--backward-word pos))
(vellumory-caret--define vellumory-caret-end-of-word "Caret to the end of this or the next word."
  (vellumory-caret--end-of-word pos))
(vellumory-caret--define vellumory-caret-line-start "Caret to the start of the line."
  (vellumory-caret--line-start pos))
(vellumory-caret--define vellumory-caret-line-end "Caret to the end of the line."
  (vellumory-caret--line-end pos))
(vellumory-caret--define vellumory-caret-next-block "Caret to the next block of text."
  (vellumory-caret--next-block pos))
(vellumory-caret--define vellumory-caret-previous-block "Caret to the start of this or the previous block."
  (vellumory-caret--previous-block pos))

(defun vellumory-caret--vertical (step)
  "Move the caret to the line STEP finds, keeping its column.
STEP takes the caret's position and returns the start of a line, or nil."
  (let* ((pos vellumory-caret--pos)
         (goal (or vellumory-caret--goal (vellumory-caret--x pos)))
         (line (funcall step pos)))
    (setq vellumory-caret--goal goal)
    (vellumory-caret--move (and line (vellumory-caret--at-column line goal)) t)))

(defun vellumory-caret-next-line () "Caret down a line, keeping its column." (interactive)
  (vellumory-caret--vertical #'vellumory-caret--next-line))
(defun vellumory-caret-previous-line () "Caret up a line, keeping its column." (interactive)
  (vellumory-caret--vertical #'vellumory-caret--previous-line))

(defun vellumory-caret-toggle-selection ()
  "Start selecting characters at the caret, or stop."
  (interactive)
  ;; As in vim: v ends a character selection, turns a line selection into one.
  (if (and vellumory-caret--anchor (eq vellumory-caret--unit 'chars))
      (setq vellumory-caret--anchor nil)
    (setq vellumory-caret--anchor (or vellumory-caret--anchor vellumory-caret--pos)))
  (setq vellumory-caret--unit 'chars)
  (vellumory-caret--select)
  (vellumory--refresh t))

(defun vellumory-caret-toggle-line-selection ()
  "Select whole lines from the caret, or stop."
  (interactive)
  (if (and vellumory-caret--anchor (eq vellumory-caret--unit 'lines))
      (setq vellumory-caret--anchor nil)
    (setq vellumory-caret--anchor (or vellumory-caret--anchor vellumory-caret--pos)))
  (setq vellumory-caret--unit 'lines)
  (vellumory-caret--select)
  (vellumory--refresh t))

(defun vellumory-caret-exchange ()
  "Swap the caret and the other end of the selection."
  (interactive)
  (unless vellumory-caret--anchor (user-error "No selection"))
  (cl-rotatef vellumory-caret--anchor vellumory-caret--pos)
  (vellumory-caret--move vellumory-caret--pos))

(defun vellumory-caret--extending (motion)
  "Return a command that runs MOTION, extending the selection.
Without a selection, it starts selecting characters at the caret."
  (lambda ()
    (interactive)
    (unless vellumory-caret--anchor (setq vellumory-caret--anchor vellumory-caret--pos vellumory-caret--unit 'chars))
    (call-interactively motion)))

(defun vellumory-caret-copy ()
  "Copy the selection, then leave the caret."
  (interactive)
  (let ((deadline (+ (float-time) 3)))
    (while (and vellumory-caret--selecting (< (float-time) deadline))
      (accept-process-output vellumory--process 0.01)))
  (vellumory-copy-selection)
  (vellumory-caret-mode -1))

(defun vellumory-caret-quit ()
  "Leave the caret and its selection."
  (interactive)
  (vellumory-caret-mode -1))

(defun vellumory-caret--place-at (page x y)
  "Put the caret at the character nearest to X, Y on PAGE."
  (let* ((lines (vellumory--lines page)) (best nil) (distance nil))
    (dotimes (line (length lines))
      (let* ((box (alist-get 'box (aref lines line)))
             (d (cond ((< y (aref box 1)) (- (aref box 1) y)) ((> y (aref box 3)) (- y (aref box 3))) (t 0))))
        (when (or (null distance) (< d distance)) (setq best line distance d))))
    (when best (vellumory-caret--move (vellumory-caret--at-column (list page best 0) x)))))

(defun vellumory-caret--first-visible ()
  "The start of the first line in view, or of the first line after it."
  (let* ((position (vellumory--reading-position))
         (page (or (alist-get 'page position) 0)) (y (or (alist-get 'y position) 0))
         (lines (vellumory--lines page)))
    (or (cl-loop for line below (length lines)
                 when (> (aref (alist-get 'box (aref lines line)) 3) y) return (list page line 0))
        (vellumory-caret--next-line (list page (max 0 (1- (length lines))) 0)))))

(defvar vellumory-caret-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding '((vellumory-caret-backward-char "h" "<left>" "C-b")
                       (vellumory-caret-forward-char "l" "<right>" "C-f")
                       (vellumory-caret-next-line "j" "<down>" "C-n")
                       (vellumory-caret-previous-line "k" "<up>" "C-p")
                       (vellumory-caret-forward-word "w" "M-f" "C-<right>")
                       (vellumory-caret-backward-word "b" "M-b" "C-<left>")
                       (vellumory-caret-end-of-word "e")
                       (vellumory-caret-line-start "0" "^" "C-a" "<home>")
                       (vellumory-caret-line-end "$" "C-e" "<end>")
                       (vellumory-caret-next-block "}" "M-}")
                       (vellumory-caret-previous-block "{" "M-{")
                       (vellumory-caret-toggle-selection "v" "C-SPC")
                       (vellumory-caret-toggle-line-selection "V")
                       (vellumory-caret-exchange "o" "C-x C-x")
                       (vellumory-caret-copy "y" "M-w")
                       (vellumory-caret-quit "q" "<escape>" "C-g")))
      (dolist (key (cdr binding)) (keymap-set map key (car binding))))
    (dolist (pair '(("S-<left>" . vellumory-caret-backward-char) ("S-<right>" . vellumory-caret-forward-char)
                    ("S-<down>" . vellumory-caret-next-line) ("S-<up>" . vellumory-caret-previous-line)
                    ("C-S-<left>" . vellumory-caret-backward-word) ("C-S-<right>" . vellumory-caret-forward-word)
                    ("S-<home>" . vellumory-caret-line-start) ("S-<end>" . vellumory-caret-line-end)))
      (keymap-set map (car pair) (vellumory-caret--extending (cdr pair))))
    map)
  "Caret keys; the reader's other keys (paging, zoom, search) still work.")

;;;###autoload
(define-minor-mode vellumory-caret-mode
  "Move a caret through the page text with the keyboard and select from it.
\\{vellumory-caret-mode-map}"
  :lighter " Caret" :keymap vellumory-caret-mode-map
  (if (not vellumory-caret-mode)
      (progn (setq vellumory-caret--pos nil vellumory-caret--anchor nil vellumory-caret--goal nil)
             (vellumory-clear-selection)
             (force-mode-line-update)
             (vellumory--refresh t))
    (unless (derived-mode-p 'vellumory-mode)
      (setq vellumory-caret-mode nil)
      (user-error "Not a document"))
    (let ((start (vellumory-caret--first-visible)))
      (unless start
        (setq vellumory-caret-mode nil)
        (user-error "No text from here on (OCR adds a text layer: e o)"))
      (setq vellumory-caret--anchor nil vellumory-caret--unit 'chars)
      (vellumory-caret--move start))))

(defun vellumory-visual ()
  "Select from the caret at the top of the view, as evil's v does."
  (interactive)
  (unless vellumory-caret-mode (vellumory-caret-mode 1))
  (vellumory-caret-toggle-selection))

(provide 'vellumory-caret)
;;; vellumory-caret.el ends here
