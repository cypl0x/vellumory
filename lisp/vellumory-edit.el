;;; vellumory-edit.el --- Reversible document edits and history -*- lexical-binding: t; -*-

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
;; Every edit goes through `vellumory edit': qpdf works on an immutable
;; snapshot, the result is checked and stored by content hash, the history
;; records it, and only then is the file replaced atomically.  Undo, redo and
;; checkout move between stored snapshots; nothing is ever deleted.
;; `vellumory-history' shows the branching history as a tree.

;;; Code:
(declare-function vellumory--existing-directory "vellumory-install" ())
(require 'cl-lib)

(defvar vellumory-program)
(defvar vellumory--source-file)
(defvar vellumory--pages)
(defvar vellumory--current-page)
(defvar vellumory--password)
(defvar vellumory--signed)
(declare-function vellumory-reload "vellumory" ())
(declare-function vellumory-open "vellumory" (file &optional no-select))
(declare-function vellumory-diff "vellumory-diff" (old))

(defgroup vellumory-edit nil "Reversible document edits." :group 'vellumory)

(defvar vellumory-virtual--parts)
(defvar vellumory--original-file)

(defun vellumory--edit-file ()
  "Return the local file that edits of this document change.
Signal a user error for virtual and converted documents, and for
documents not visited from a local file."
  (when (bound-and-true-p vellumory-virtual--parts)
    (user-error "A virtual document is composed from others: edit those (C-c C-c shows which)"))
  (when (bound-and-true-p vellumory--original-file)
    (user-error "%s is shown converted to PDF; its edits would not reach it"
                (file-name-nondirectory vellumory--original-file)))
  (let ((file (or vellumory--source-file
                  (user-error "Only documents visited from a local file can be edited"))))
    ;; An edit replaces the file by a new one written beside it.
    (unless (and (file-writable-p file) (file-writable-p (file-name-directory file)))
      (user-error "%s or its folder is read-only: write a copy (%s) and edit that"
                  (file-name-nondirectory file)
                  (substitute-command-keys "\\<vellumory-mode-map>\\[vellumory-write-copy]")))
    file))

(defun vellumory-write-copy (file)
  "Write a copy of this document to FILE and visit it, to edit that one.
For a document that cannot be changed where it is: its file or its folder
is read-only.  The copy is writable."
  (interactive
   (list (read-file-name "Write a copy to: " nil nil nil
                         (concat (file-name-base (or vellumory--source-file vellumory--file)) "-copy.pdf"))))
  (let ((source (or vellumory--source-file vellumory--file)))
    (when (file-directory-p file) (setq file (expand-file-name (file-name-nondirectory source) file)))
    (copy-file source file 1)
    (set-file-modes file (logior (file-modes file) #o200))
    (find-file file)))

(defun vellumory--read-file ()
  "Return the local PDF that commands only reading this document read.
Unlike `vellumory--edit-file', a read-only file is fine: extracting or
exporting never changes it."
  (or vellumory--source-file vellumory--file
      (user-error "Only documents read from a local file can be exported")))

;;;; Marked pages, for export

(defvar-local vellumory--marked-pages nil "Pages (one-based) marked for export, in order.")

(declare-function vellumory--thumbnail-show-marks "vellumory-thumbnails" ())

(defun vellumory--marks-changed ()
  "Show the marks in the page previews and say how many there are."
  (when (fboundp 'vellumory--thumbnail-show-marks) (vellumory--thumbnail-show-marks))
  (message (if vellumory--marked-pages
               (format "Marked for export: %s (%s exports them)"
                       (mapconcat #'number-to-string vellumory--marked-pages ", ")
                       (substitute-command-keys "\\[vellumory-export-marked-pages]"))
             "No pages marked")))

;;;###autoload
(defun vellumory-mark-page (&optional page)
  "Mark PAGE (one-based; the current page by default) for export, or unmark it."
  (interactive)
  (let ((page (or page (1+ (or vellumory--current-page 0)))))
    (setq vellumory--marked-pages
          (if (memq page vellumory--marked-pages) (delq page vellumory--marked-pages)
            (sort (cons page vellumory--marked-pages) #'<)))
    (vellumory--marks-changed)))

;;;###autoload
(defun vellumory-unmark-all ()
  "Clear every page mark."
  (interactive)
  (setq vellumory--marked-pages nil)
  (vellumory--marks-changed))

(defun vellumory--export-default (pages)
  "A file name beside the document for an export of PAGES."
  (let ((file (vellumory--read-file)))
    (expand-file-name (format "%s-p%s.pdf" (file-name-base file)
                              (mapconcat #'number-to-string pages "_"))
                      (file-name-directory file))))

(defun vellumory--export (pages output)
  "Write PAGES (one-based) of this document to a new document OUTPUT."
  (let ((output (expand-file-name output)))
    (vellumory--cli (list "extract" (vellumory--read-file) (mapconcat #'number-to-string pages ",") output)
                    (lambda (_)
                      (when (y-or-n-p (format "Wrote %s.  Open it? " (abbreviate-file-name output)))
                        (vellumory-open output))))))

;;;###autoload
(defun vellumory-export-page (page output)
  "Write PAGE (one-based; the current page) to a new document OUTPUT."
  (interactive
   (let ((page (1+ (or vellumory--current-page 0))))
     (list page (read-file-name (format "Export page %d to: " page) nil nil nil
                                (file-name-nondirectory (vellumory--export-default (list page)))))))
  (vellumory--export (list page) output))

;;;###autoload
(defun vellumory-export-marked-pages (output)
  "Write the pages marked for export to a new document OUTPUT, in page order."
  (interactive
   (progn (unless vellumory--marked-pages
            (user-error "No pages marked (%s marks one)" (substitute-command-keys "\\[vellumory-mark-page]")))
          (list (read-file-name (format "Export pages %s to: "
                                        (mapconcat #'number-to-string vellumory--marked-pages ", "))
                                nil nil nil (file-name-nondirectory (vellumory--export-default vellumory--marked-pages))))))
  (vellumory--export vellumory--marked-pages output))

(defun vellumory--cli (args callback &optional buffer input)
  "Run the worker executable with ARGS; call CALLBACK with its parsed JSON.
Errors are reported as messages.  BUFFER receives the reload afterwards.
INPUT, when given, is written to the process's standard input."
  (let* ((output (generate-new-buffer " *vellumory-cli*"))
         ;; Each run its own: concurrent commands must not mix their errors.
         (errors-buffer (generate-new-buffer " *vellumory-cli-errors*"))
         (source (or buffer (current-buffer)))
         (process
          (let ((default-directory (vellumory--existing-directory)))
            (make-process
             :name "vellumory-cli" :buffer output :command (cons vellumory-program args)
             :connection-type 'pipe :noquery t :stderr errors-buffer
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (let* ((ok (zerop (process-exit-status process)))
                        (text (with-current-buffer output (string-trim (buffer-string))))
                        (errors (with-current-buffer errors-buffer (string-trim (buffer-string))))
                        ;; The JSON is the last line: MuPDF may warn before it.
                        (json (ignore-errors
                                (json-parse-string (car (last (split-string (if ok text errors) "\n" t)))
                                                   :object-type 'alist :array-type 'array
                                                   :null-object nil :false-object nil))))
                   (kill-buffer output)
                   (when (buffer-live-p errors-buffer) (kill-buffer errors-buffer))
                   (if ok
                       (when (buffer-live-p source)
                         (with-current-buffer source (funcall callback json)))
                     (message "Vellumory: %s" (or (alist-get 'error json) errors text))))))))))
    (when input
      (process-send-string process input)
      (process-send-eof process))
    process))

(defun vellumory--after-edit (result)
  "Reload the document and its history buffers, then report RESULT.
RESULT says whether the edit changed the document and names the state
it led to."
  (vellumory-reload)
  (vellumory--history-refresh-buffers)
  (when (fboundp (quote vellumory--details-refresh-buffers)) (vellumory--details-refresh-buffers))
  (let ((node (alist-get 'node result)))
    (message (if (alist-get 'changed result)
                 (format "%s · state %d (undo with %s)" (alist-get 'label node) (alist-get 'index node)
                         (substitute-command-keys "\\[vellumory-undo]"))
               "Already in that state"))))

(defun vellumory--edit (operation)
  "Apply OPERATION (an alist) to the visited document.
Operations carrying passwords go through standard input, never the command
line, which other users can read."
  (let ((file (vellumory--edit-file))
        (json (json-serialize operation)))
    ;; Every edit rewrites the file, which invalidates digital signatures.
    (when (and (> vellumory--signed 0)
               (not (yes-or-no-p
                     (format "%s is digitally signed; this edit invalidates %s (the signed version stays in the history).  Edit? "
                             (file-name-nondirectory file)
                             (if (= vellumory--signed 1) "its signature" (format "its %d signatures" vellumory--signed))))))
      (user-error "Not edited"))
    (if (seq-some (lambda (key) (assq key operation)) '(user owner password))
        (vellumory--cli (list "edit" file "-") #'vellumory--after-edit nil json)
      (vellumory--cli (list "edit" file json) #'vellumory--after-edit))))

;;;###autoload
(defun vellumory-stamp (file pages &optional under)
  "Draw the first page of FILE over PAGES, a qpdf page range such as 1-z.
With a prefix argument UNDER, draw it beneath them (a letterhead or
watermark behind the text)."
  (interactive (list (read-file-name "Stamp with the first page of: " nil nil t)
                     (read-string "On pages (1-z: all, 2-z, 1,3,5): " "1-z")
                     current-prefix-arg))
  (vellumory--edit `((op . "stamp") (file . ,(expand-file-name file)) (under . ,(if under t :false))
                     (pages . ,pages))))

;;;###autoload
(defun vellumory-encrypt (user owner print modify extract)
  "Encrypt with AES-256.  USER opens the document (empty: no password to open);
OWNER is needed to change the permissions PRINT, MODIFY and EXTRACT.
Neither password is recorded in the history."
  (interactive (list (read-passwd "Password to open (empty for none): " t)
                     (read-passwd "Password to change permissions: " t)
                     (completing-read "Printing: " '("full" "low" "none") nil t nil nil "full")
                     (completing-read "Changes allowed: " '("all" "annotate" "form" "assembly" "none")
                                      nil t nil nil "all")
                     (y-or-n-p "Allow copying text and images? ")))
  ;; The reader reopens the result with it.
  (setq vellumory--password (and (not (string-empty-p user)) user))
  (vellumory--edit `((op . "encrypt") (user . ,user) (owner . ,owner) (print . ,print)
                     (modify . ,modify) (extract . ,(if extract t :false)))))

;;;###autoload
(defun vellumory-decrypt (password)
  "Remove encryption, given PASSWORD (to open, or the owner's)."
  (interactive (list (or vellumory--password (read-passwd "Password: "))))
  (vellumory--edit `((op . "decrypt") (password . ,password))))

(defun vellumory--parse-pages (spec)
  "Pages in SPEC such as \"1-3,7,9-8\", in order, one-based."
  (let ((count (length vellumory--pages)) pages)
    (dolist (part (split-string spec "[, ]+" t) (nreverse pages))
      (if (string-match "\\`\\([0-9]+\\|z\\)\\(?:-\\([0-9]+\\|z\\)\\)?\\'" part)
          (let* ((value (lambda (s) (if (equal s "z") count (string-to-number s))))
                 (from (funcall value (match-string 1 part)))
                 (to (if (match-string 2 part) (funcall value (match-string 2 part)) from)))
            (dolist (page (if (<= from to) (number-sequence from to) (number-sequence from to -1)))
              (unless (<= 1 page count) (user-error "Page %d is outside 1–%d" page count))
              (push page pages)))
        (user-error "Not a page range: %s" part)))))

(defun vellumory--read-pages (prompt)
  "Read one-based pages with PROMPT; the default is the current page."
  (let ((current (number-to-string (1+ (or vellumory--current-page 0)))))
    (vellumory--parse-pages (read-string (format "%s (e.g. 1-3,7, default %s): " prompt current)
                                         nil nil current))))

(defun vellumory--pages-arg (arg prompt)
  "Return one-based pages: read with PROMPT when ARG, else the current page."
  (if arg (vellumory--read-pages prompt) (list (1+ (or vellumory--current-page 0)))))

;;;###autoload
(defun vellumory-rotate-pages (&optional arg)
  "Rotate the current page clockwise; with ARG, pages read from the minibuffer."
  (interactive "P")
  (vellumory--edit `((op . "rotate") (pages . ,(vconcat (vellumory--pages-arg arg "Rotate pages")))
                     (degrees . 90))))

;;;###autoload
(defun vellumory-rotate-pages-counterclockwise (&optional arg)
  "Rotate the current page counterclockwise; with ARG, read pages."
  (interactive "P")
  (vellumory--edit `((op . "rotate") (pages . ,(vconcat (vellumory--pages-arg arg "Rotate pages")))
                     (degrees . -90))))

;;;###autoload
(defun vellumory-delete-pages (&optional arg)
  "Delete the current page; with ARG, pages read from the minibuffer.
The deleted pages remain in the history and come back with undo."
  (interactive "P")
  (let* ((doomed (vellumory--pages-arg arg "Delete pages"))
         (kept (cl-loop for page from 1 to (length vellumory--pages)
                        unless (memq page doomed) collect page)))
    (unless kept (user-error "A document needs at least one page"))
    (vellumory--edit `((op . "select") (pages . ,(vconcat kept))))))

;;;###autoload
(defun vellumory-keep-pages (spec)
  "Keep only the pages in SPEC, in that order: reorder, delete or duplicate."
  (interactive (list (read-string "Keep pages in this order (e.g. 3,1-2): ")))
  (vellumory--edit `((op . "select") (pages . ,(vconcat (vellumory--parse-pages spec))))))

;;;###autoload
(defun vellumory-move-page (to)
  "Move the current page so that it becomes page TO."
  (interactive (list (read-number "Move this page to position: " (1+ (or vellumory--current-page 0)))))
  (let* ((count (length vellumory--pages)) (from (1+ (or vellumory--current-page 0)))
         (others (delq from (number-sequence 1 count))))
    (unless (<= 1 to count) (user-error "Position outside 1–%d" count))
    (vellumory--edit `((op . "select") (pages . ,(vconcat (append (seq-take others (1- to)) (list from)
                                                                 (seq-drop others (1- to)))))))))

;;;###autoload
(defun vellumory-append-documents (files)
  "Append the pages of FILES to this document."
  (interactive (list (list (read-file-name "Append document: " nil nil t))))
  (vellumory--edit `((op . "append") (files . ,(vconcat (mapcar #'expand-file-name files))))))

;;;###autoload
(defun vellumory-compress ()
  "Compress losslessly: object streams and maximum Flate recompression."
  (interactive) (vellumory--edit '((op . "compress"))))

;;;###autoload
(defun vellumory-linearize ()
  "Linearize the document for fast incremental loading over a network."
  (interactive) (vellumory--edit '((op . "linearize"))))

;;;###autoload
(defun vellumory-set-page-labels (specs)
  "Set page labels from qpdf SPECS, e.g. \"1:r 5:D\" (roman from 1, decimal from 5)."
  (interactive (list (read-string "Page labels (first-page:[D|A|a|R|r][/start[/prefix]] …): ")))
  (vellumory--edit `((op . "labels") (specs . ,(vconcat (split-string specs nil t))))))

(defconst vellumory--relationships
  '("Source" "Data" "Alternative" "Supplement" "EncryptedPayload" "FormData" "Schema" "Unspecified")
  "How an associated file relates to the document (ISO 32000-2 14.13.2).")

;;;###autoload
(defun vellumory-attach-file (file key description &optional relationship)
  "Embed FILE in the document under KEY with DESCRIPTION.
With a prefix argument, also name its RELATIONSHIP to the document (Source,
Data, Supplement, ...), which makes it an associated file; PDF/A-3 and -4
documents get Unspecified without one."
  (interactive (let ((file (read-file-name "Attach file: " nil nil t)))
                 (list file (read-string "Attachment name: " (file-name-nondirectory file))
                       (read-string "Description: ")
                       (and current-prefix-arg
                            (completing-read "Relationship to the document: " vellumory--relationships nil t)))))
  (vellumory--edit `((op . "attach") (file . ,(expand-file-name file)) (key . ,key)
                     (description . ,description) (relationship . ,(or relationship "")))))

;;;###autoload
(defun vellumory-remove-attachment (key)
  "Remove the embedded file KEY."
  (interactive (list (read-string "Remove attachment: ")))
  (vellumory--edit `((op . "detach") (key . ,key))))

;;;###autoload
(defun vellumory-ocr (languages mode)
  "Add a searchable text layer with ocrmypdf in LANGUAGES (tesseract codes).
MODE skip leaves pages that already have text alone; redo replaces an
earlier OCR layer; force rasterizes every page first.  Recorded and undoable."
  (interactive (list (read-string "OCR languages (e.g. eng or deu+eng): " "eng")
                     (completing-read "Pages with text: " '("skip" "redo" "force") nil t nil nil "skip")))
  (message "Recognizing text; this can take a while…")
  (vellumory--edit `((op . "ocr") (languages . ,languages) (mode . ,mode))))

;;;###autoload
(defun vellumory-undo ()
  "Restore the state before the last edit."
  (interactive)
  (vellumory--cli (list "undo" (vellumory--edit-file)) #'vellumory--after-edit))

;;;###autoload
(defun vellumory-redo ()
  "Return to the most recent state after the current one."
  (interactive)
  (vellumory--cli (list "redo" (vellumory--edit-file)) #'vellumory--after-edit))

;;;###autoload
(defun vellumory-extract-pages (spec output)
  "Write the pages in SPEC to a new document OUTPUT; this one is unchanged."
  (interactive (list (read-string "Extract pages (qpdf range, e.g. 1-3,7): ")
                     (read-file-name "New document: ")))
  (vellumory--cli (list "extract" (vellumory--read-file) spec (expand-file-name output))
                  (lambda (_) (when (y-or-n-p (format "Open %s? " output)) (vellumory-open output)))))

;;;###autoload
(defun vellumory-split (every directory)
  "Split into documents of EVERY pages inside DIRECTORY."
  (interactive (list (read-number "Pages per document: " 1)
                     (read-directory-name "Into directory: ")))
  (let* ((file (vellumory--read-file))
         (pattern (expand-file-name (concat (file-name-base file) "-%d.pdf") directory)))
    (make-directory directory t)
    (vellumory--cli (list "split" file (number-to-string every) pattern)
                    (lambda (_) (message "Split into %s" directory)))))

;;;###autoload
(defun vellumory-export-text (output)
  "Write the text of every page to OUTPUT (form feeds between pages)."
  (interactive (list (read-file-name "Export text to: " nil nil nil
                                     (concat (file-name-base (vellumory--read-file)) ".txt"))))
  (let ((output (expand-file-name output)) (file (vellumory--read-file)))
    (let ((default-directory (vellumory--existing-directory)))
      (make-process
       :name "vellumory-text" :command (list vellumory-program "text" file) :noquery t
       :buffer (generate-new-buffer " *vellumory-text*")
       :sentinel (lambda (process _)
                   (unless (process-live-p process)
                     (with-current-buffer (process-buffer process)
                       (let ((coding-system-for-write 'utf-8))
                         (write-region nil nil output nil 'silent)))
                     (kill-buffer (process-buffer process))
                     (message "Wrote %s" output)))))))

;;;; History tree

(defvar-local vellumory--history-file nil)
(defvar-local vellumory--history nil)

(defun vellumory--history-tree (nodes)
  "Lines (INDEX . PREFIX) of NODES in depth-first order, children oldest first."
  (let ((children (make-hash-table :test #'eql)) lines)
    (cl-loop for node across nodes for i from 0
             when (alist-get 'parent node)
             do (push i (gethash (alist-get 'parent node) children)))
    (cl-labels ((walk (i prefix last root)
                  (push (cons i (if root "" (concat prefix (if last "└─ " "├─ ")))) lines)
                  (let ((kids (reverse (gethash i children))))
                    (cl-loop for (kid . rest) on kids
                             do (walk kid (if root "" (concat prefix (if last "   " "│  ")))
                                      (null rest) nil)))))
      (walk 0 "" t t))
    (nreverse lines)))

(defun vellumory--history-render ()
  "Redraw the history as a tree, with point on the current state."
  (let* ((inhibit-read-only t) (nodes (alist-get 'nodes vellumory--history))
         (head (alist-get 'head vellumory--history)) (target nil))
    (erase-buffer)
    (insert (propertize (abbreviate-file-name vellumory--history-file) 'face 'bold) "\n\n")
    (dolist (line (vellumory--history-tree nodes))
      (let* ((i (car line)) (node (aref nodes i)) (current (= i head)) (start (point)))
        (when current (setq target start))
        (insert (cdr line)
                (propertize (if current "● " "○ ") 'face (if current 'success 'shadow))
                (propertize (format-time-string "%Y-%m-%d %H:%M:%S"
                                                (/ (alist-get 'time node) 1000.0))
                            'face 'font-lock-comment-face)
                "  " (propertize (alist-get 'label node) 'face (if current 'bold 'default))
                (propertize (format "  %s" (substring (alist-get 'hash node) 0 10)) 'face 'shadow)
                "\n")
        (put-text-property start (point) 'vellumory-state i)))
    (insert "\n" (propertize (substitute-command-keys
                              "\\<vellumory-history-mode-map>\\[vellumory-history-checkout] make current  \
\\[vellumory-history-view] view without changing the file  \\[vellumory-history-diff] compare with now  \\[vellumory-history-refresh] refresh  \\[quit-window] quit")
                             'face 'shadow))
    (goto-char (or target (point-min)))))

(defun vellumory-history-refresh ()
  "Reload the history from the store."
  (interactive)
  (let ((buffer (current-buffer)))
    (vellumory--cli (list "history" vellumory--history-file)
                    (lambda (history)
                      (with-current-buffer buffer
                        (setq vellumory--history history)
                        (vellumory--history-render)))
                    buffer)))

(defun vellumory--history-refresh-buffers ()
  "Refresh every history buffer of this document's file."
  (let ((file vellumory--source-file))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'vellumory-history-mode) (equal vellumory--history-file file))
          (vellumory-history-refresh))))))

(defun vellumory--history-state ()
  "Return the index of the state on this line, or signal a user error."
  (or (get-text-property (point) 'vellumory-state) (user-error "No state on this line")))

(defun vellumory-history-checkout ()
  "Make the state at point current; the file is rewritten from its snapshot."
  (interactive)
  (let ((file vellumory--history-file) (state (vellumory--history-state)))
    (vellumory--cli (list "checkout" file (number-to-string state))
                    (lambda (_)
                      (vellumory-history-refresh)
                      (when-let* ((reader (find-buffer-visiting file)))
                        (with-current-buffer reader (vellumory-reload)))
                      (message "State %d is current" state)))))

(defun vellumory-history-view ()
  "Open the state at point read-only, leaving the file as it is."
  (interactive)
  (let ((file vellumory--history-file) (state (vellumory--history-state)))
    (vellumory--cli (list "snapshot" file (number-to-string state))
                    (lambda (result)
                      (let ((buffer (vellumory-open (alist-get 'path result))))
                        (with-current-buffer buffer
                          (rename-buffer (format "%s@%d" (file-name-nondirectory file) state) t)
                          ;; A snapshot is immutable: never offer to edit it.
                          (setq-local vellumory--source-file nil)))))))

(defun vellumory-history-diff ()
  "Compare the state at point with the current file."
  (interactive)
  (let ((file vellumory--history-file) (state (vellumory--history-state)))
    (vellumory--cli (list "snapshot" file (number-to-string state))
                    (lambda (result)
                      (let ((reader (or (find-buffer-visiting file) (vellumory-open file t))))
                        (with-current-buffer reader (vellumory-diff (alist-get 'path result))))))))

(defun vellumory-history-next () "Next state." (interactive)
  (forward-line 1) (while (and (not (eobp)) (not (get-text-property (point) 'vellumory-state))) (forward-line 1)))
(defun vellumory-history-previous () "Previous state." (interactive)
  (forward-line -1) (while (and (not (bobp)) (not (get-text-property (point) 'vellumory-state))) (forward-line -1)))

(defvar vellumory-history-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'vellumory-history-checkout)
    (keymap-set map "c" #'vellumory-history-checkout)
    (keymap-set map "v" #'vellumory-history-view)
    (keymap-set map "d" #'vellumory-history-diff)
    (keymap-set map "g" #'vellumory-history-refresh)
    (keymap-set map "n" #'vellumory-history-next)
    (keymap-set map "p" #'vellumory-history-previous)
    (keymap-set map "j" #'vellumory-history-next)
    (keymap-set map "k" #'vellumory-history-previous)
    map))

(define-derived-mode vellumory-history-mode special-mode "PDF History"
  "Every state of a document as a tree.  Nothing here is ever deleted.
\\{vellumory-history-mode-map}"
  (setq-local truncate-lines t revert-buffer-function (lambda (&rest _) (vellumory-history-refresh))))

;;;###autoload
(defun vellumory-history ()
  "Show this document's branching edit history."
  (interactive)
  (let* ((file (vellumory--edit-file))
         (buffer (get-buffer-create (format "*History: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buffer
      (vellumory-history-mode)
      (setq vellumory--history-file file)
      (vellumory-history-refresh))
    (pop-to-buffer buffer)))

(provide 'vellumory-edit)
;;; vellumory-edit.el ends here
