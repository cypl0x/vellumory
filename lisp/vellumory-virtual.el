;;; vellumory-virtual.el --- Several documents read as one -*- lexical-binding: t; -*-

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
;; A virtual document lists pages, or parts of pages, of other documents in
;; the order to read them, in pdf-tools' .vpdf format, so files written for
;; pdf-tools open unchanged:
;;
;;   ;; %VPDF 1.0
;;   (("paper.pdf" (1 . 4) 7)                ; pages 1–4 and 7
;;    ("slides.pdf")                          ; every page
;;    ("scan.pdf" (2 0 0 1 0.5)))             ; the top half of page 2
;;
;; A page spec is a page, (FIRST . LAST) (LAST 0: the last page), or either
;; with a region (LEFT TOP RIGHT BOTTOM, fractions of the page as shown);
;; (nil . REGION) is every page.  Visiting such a file composes it with MuPDF
;; into one PDF, cached under a key of the list and every source's content, and
;; reads that like any document: search, occur, selection, links and the
;; reading position all work.  The header names each page's source.  C-c C-c
;; edits the list and C-c C-c again shows it; M-{ and M-} go to the previous
;; and next source.  `vellumory-virtual-create' (`; V' in Dired) starts one.

;;; Code:
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'cl-lib)
(require 'subr-x)

(defvar vellumory-program)
(defvar vellumory--materialized)
(defvar vellumory--source-file)
(defvar vellumory--pages)
(defvar vellumory--current-page)
(declare-function dired-get-marked-files "dired" (&optional localp arg filter distinguish-one-marked error))
(declare-function vellumory-mode "vellumory" ())
(declare-function vellumory--header-line "vellumory" ())
(declare-function vellumory-goto-page "vellumory" (page))

(defconst vellumory-virtual-magic "\\`[ \t]*;+[ \t]*%VPDF\\_>"
  "The first line of a virtual document, as pdf-tools recognizes it.")

(defvar-local vellumory-virtual--parts nil
  "Where the composed pages come from: alists of file, first, last, start.")
(defvar-local vellumory-virtual--text nil "The list being shown, to edit again.")
(put 'vellumory-virtual--text 'permanent-local t)

(defun vellumory-virtual--page-spec (spec)
  "SPEC as (FIRST LAST REGION), the forms pdf-tools accepts."
  (let ((parsed (pcase spec
                  ((pred natnump) (list spec spec nil))
                  (`(,(and first (pred natnump)) . ,(and last (pred natnump))) (list first last nil))
                  (`(nil . ,region) (list 1 0 region))
                  (`(,(and page (pred natnump)) . ,region) (list page page region))
                  (`((,(and first (pred natnump)) . ,last) . ,region)
                   (list first (if (natnump last) last 0) region))
                  (_ (user-error "Not a page spec: %S" spec)))))
    (pcase-let ((`(,first ,last ,region) parsed))
      (when (equal region '(0 0 1 1)) (setq region nil))
      (unless (or (null region) (and (= (length region) 4) (cl-every #'numberp region)))
        (user-error "A region is four fractions, left top right bottom: %S" region))
      (list first last region))))

(defun vellumory-virtual--parts-of (list directory)
  "The parts LIST asks for, its files relative to DIRECTORY, for the composer."
  (unless (and (listp list) (cl-every #'consp list) (cl-every (lambda (e) (stringp (car e))) list))
    (user-error "A virtual document is a list of (FILE . PAGE-SPECS)"))
  (cl-loop for (file . specs) in list
           for path = (expand-file-name (substitute-in-file-name file) directory)
           do (unless (file-readable-p path) (user-error "Cannot read %s" path))
           append (cl-loop for spec in (or specs '((nil 0 0 1 1)))
                           collect (pcase-let ((`(,first ,last ,region) (vellumory-virtual--page-spec spec)))
                                     `((file . ,path) (first . ,first) (last . ,last)
                                       ,@(and region `((region . ,(vconcat region)))))))))

(defun vellumory-virtual--compose (parts)
  "Compose PARTS with the worker's composer; its reply (path, pages, parts)."
  (with-temp-buffer
    (let ((input (json-serialize (vconcat parts))) (errors (make-temp-file "vellumory-compose-")))
      (unwind-protect
          (progn
            (insert input)
            (let ((status (let ((default-directory (vellumory--existing-directory)))
                            (call-process-region (point-min) (point-max) vellumory-program t
                                                 (list t errors) nil "compose"))))
              (unless (zerop status)
                (user-error "Composing failed: %s"
                            (let ((text (with-temp-buffer (insert-file-contents errors) (buffer-string))))
                              (or (ignore-errors (alist-get 'error (json-parse-string text :object-type 'alist)))
                                  (string-trim text)))))
              (json-parse-string (buffer-string) :object-type 'alist :array-type 'list)))
        (delete-file errors)))))

;;;###autoload
(defun vellumory-virtual-mode ()
  "Read this virtual document (pdf-tools' .vpdf list) as one document.
\\<vellumory-virtual-view-mode-map>\\[vellumory-virtual-edit] edits the list."
  (interactive)
  (require 'vellumory)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (list (with-temp-buffer
                 (insert text)
                 (goto-char (point-min))
                 (condition-case err (read (current-buffer))
                   (error (user-error "Not a virtual document: %s" (error-message-string err))))))
         (reply (condition-case err (vellumory-virtual--compose
                                     (vellumory-virtual--parts-of list default-directory))
                  (user-error (vellumory-virtual-edit-mode)
                              (signal (car err) (cdr err))))))
    (let ((vellumory--materialized (alist-get 'path reply)))
      (vellumory-mode))
    (setq vellumory-virtual--text text
          vellumory-virtual--parts (alist-get 'parts reply))
    (setq-local header-line-format '((:eval (vellumory--header-line)) (:eval (vellumory-virtual--header))))
    (vellumory-virtual-view-mode 1)
    (message "%d pages from %d documents%s" (alist-get 'pages reply)
             (length (delete-dups (mapcar (lambda (p) (alist-get 'file p)) vellumory-virtual--parts)))
             (if (eq (alist-get 'cached reply) t) "" " (composed)"))))

(defun vellumory-virtual--part (&optional page)
  "The part showing zero-based PAGE (the current page by default)."
  (let ((page (or page vellumory--current-page 0)))
    (seq-find (lambda (part)
                (let ((start (alist-get 'start part)))
                  (and (<= start page) (< page (+ start (- (alist-get 'last part) (alist-get 'first part)) 1)))))
              vellumory-virtual--parts)))

(defun vellumory-virtual--header ()
  "The current page's source document and page there."
  (when-let* ((part (vellumory-virtual--part)))
    (propertize (format "   %s p. %d" (file-name-nondirectory (alist-get 'file part))
                        (+ (alist-get 'first part) (- (or vellumory--current-page 0) (alist-get 'start part))))
                'face 'shadow)))

(defun vellumory-virtual-next-document (&optional count)
  "Go to the first page of the COUNTth next source document (previous if negative)."
  (interactive "p")
  (let* ((count (or count 1))
         (starts (let (result last)
                   (dolist (part vellumory-virtual--parts (nreverse result))
                     (unless (equal (alist-get 'file part) last)
                       (push (alist-get 'start part) result))
                     (setq last (alist-get 'file part)))))
         (current (or (cl-position-if (lambda (start) (<= start (or vellumory--current-page 0))) starts
                                      :from-end t)
                      0))
         (target (+ current count)))
    (unless (< -1 target (length starts)) (user-error "No %s document" (if (> count 0) "next" "previous")))
    (vellumory-goto-page (1+ (nth target starts)))))

(defun vellumory-virtual-previous-document (&optional count)
  "Go to the first page of the COUNTth previous source document."
  (interactive "p")
  (vellumory-virtual-next-document (- (or count 1))))

(defun vellumory-virtual-edit ()
  "Edit the list this virtual document is composed from."
  (interactive)
  ;; The buffer holds the list, not the composition: bring the list back.
  (setq vellumory--source-file nil)
  (vellumory-virtual-edit-mode))

(defvar-keymap vellumory-virtual-view-mode-map
  "C-c C-c" #'vellumory-virtual-edit
  "<remap> <forward-paragraph>" #'vellumory-virtual-next-document
  "<remap> <backward-paragraph>" #'vellumory-virtual-previous-document
  "M-}" #'vellumory-virtual-next-document
  "M-{" #'vellumory-virtual-previous-document)

(define-minor-mode vellumory-virtual-view-mode
  "Keys of a virtual document.
\\<vellumory-virtual-view-mode-map>\\[vellumory-virtual-edit] edits its list; \\[vellumory-virtual-previous-document] and \\[vellumory-virtual-next-document] move between its sources."
  :lighter " VPDF")

(defvar-keymap vellumory-virtual-edit-mode-map
  "C-c C-c" #'vellumory-virtual-mode)

(define-derived-mode vellumory-virtual-edit-mode emacs-lisp-mode "VPDF-Edit"
  "Edit a virtual document's list of pages; \\[vellumory-virtual-mode] shows it."
  (when vellumory-virtual--text
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert vellumory-virtual--text)
      (set-buffer-modified-p nil)))
  (setq buffer-read-only nil)
  (buffer-enable-undo))

;;;###autoload
(defun vellumory-virtual-create (files &optional name)
  "Start a virtual document reading FILES in order, in a buffer NAME.
In Dired, the marked files (`; V'); otherwise the PDFs of this directory."
  (interactive (list (if (derived-mode-p 'dired-mode) (dired-get-marked-files)
                       (directory-files default-directory t "\\.pdf\\'"))
                     (read-string "Virtual document (default all.vpdf): " nil nil "all.vpdf")))
  (let ((directory default-directory))
    (with-current-buffer (generate-new-buffer (or name "all.vpdf"))
      (setq default-directory directory)
      (insert ";; %VPDF 1.0\n\n"
              ";; (FILE PAGE-SPEC…): a page, (FIRST . LAST), either followed by a region\n"
              ";; LEFT TOP RIGHT BOTTOM in fractions of the page, or (nil . REGION) for all.\n"
              ";; C-c C-c reads it; save it as a .vpdf file to keep it.\n\n"
              "(" (mapconcat (lambda (file) (format "(%S)" (file-relative-name file directory))) files "\n ")
              ")\n")
      (vellumory-virtual-edit-mode)
      (pop-to-buffer (current-buffer)))))

;; Copied into the autoloads, where `vellumory-virtual-magic' is not yet defined.
;;;###autoload
(add-to-list 'magic-mode-alist (cons "\\`[ \t]*;+[ \t]*%VPDF\\_>" #'vellumory-virtual-mode))

(provide 'vellumory-virtual)
;;; vellumory-virtual.el ends here
