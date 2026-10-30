;;; vellumory-form.el --- Fill in PDF forms -*- lexical-binding: t; -*-

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
;; `vellumory-fill-form' (`e f') shows the document's form fields as a form:
;; text fields take any text, checkboxes yes or no, choices one of the
;; values shown.  C-c C-c records the changed fields as one undoable edit.
;; Values are set with the document's JavaScript triggers ignored: filling a
;; form never runs code from the document.  Read-only fields are shown but
;; cannot be changed; signature fields are left out.

;;; Code:
(require 'text-property-search)
(require 'cl-lib)
(require 'subr-x)

(defvar vellumory--source-file)
(declare-function vellumory--rpc "vellumory" (method params callback &optional id))
(declare-function vellumory--edit "vellumory-edit" (operation))

(defvar-local vellumory-form--source nil "Reader buffer the form fills.")
(defvar-local vellumory-form--fields nil "The fields as the worker reported them.")

(defun vellumory-form--shown (field)
  "FIELD's value as the form shows it."
  (let ((value (or (alist-get 'value field) "")))
    (pcase (alist-get 'type field)
      ("checkbox" (if (member value '("" "Off")) "no" "yes"))
      ("radio" (if (member value '("" "Off")) "" value))
      (_ value))))

(defun vellumory-form--hint (field)
  "The values FIELD accepts as a hint, or nil for free text."
  (pcase (alist-get 'type field)
    ("checkbox" "yes or no")
    ((or "combobox" "listbox") (string-join (append (alist-get 'options field) nil) ", "))
    ("radio" (string-join (append (alist-get 'states field) nil) ", "))
    (_ nil)))

(defun vellumory-form--fillable-p (field)
  "Non-nil if FIELD is of a type the form can fill."
  (member (alist-get 'type field) '("text" "checkbox" "combobox" "listbox" "radio")))

(defun vellumory-form--render (fields)
  "Show the fillable FIELDS in this buffer, one per line, as label and value.
Read-only fields are shown but cannot be changed."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Form of %s\n" (file-name-nondirectory
                                               (buffer-local-value 'vellumory--source-file vellumory-form--source)))
                        'face 'bold 'read-only t 'rear-nonsticky t)
            (propertize (substitute-command-keys
                         (concat "\\<vellumory-form-mode-map>\\[vellumory-form-commit] records one undoable edit; "
                                 "the document's scripts never run.\n\n"))
                        'face 'shadow 'read-only t 'rear-nonsticky t))
    (setq vellumory-form--fields nil)
    (seq-doseq (field fields)
      (when (vellumory-form--fillable-p field)
        (push field vellumory-form--fields)
        (let* ((name (alist-get 'name field))
               (label (let ((l (alist-get 'label field))) (if (and l (not (string-empty-p l))) l name)))
               (hint (vellumory-form--hint field))
               (locked (eq (alist-get 'read_only field) t)))
          (insert (propertize (format "%s%s%s: " label (if hint (format " (%s)" hint) "")
                                      (if locked " [read-only]" ""))
                              'read-only t 'rear-nonsticky (not locked) 'front-sticky t
                              'face (if locked 'shadow 'font-lock-keyword-face) 'vellumory-form-field name))
          (insert (if locked
                      (propertize (vellumory-form--shown field) 'read-only t 'face 'shadow)
                    (vellumory-form--shown field))
                  "\n"))))
    (setq vellumory-form--fields (nreverse vellumory-form--fields))
    (goto-char (point-min))
    (text-property-search-forward 'vellumory-form-field)
    (set-buffer-modified-p nil)))

(defun vellumory-form--values ()
  "Field name → value as the form now reads."
  (save-excursion
    (goto-char (point-min))
    (let (values)
      (while (not (eobp))
        (when-let* ((name (get-text-property (point) 'vellumory-form-field))
                    (end (next-single-property-change (point) 'vellumory-form-field nil (line-end-position))))
          (push (cons name (string-trim (buffer-substring-no-properties end (line-end-position)))) values))
        (forward-line 1))
      (nreverse values))))

(defun vellumory-form--value (field shown)
  "The PDF value for FIELD from what the form SHOWN reads, or a user error."
  (let ((name (alist-get 'name field)))
    (pcase (alist-get 'type field)
      ("checkbox"
       (cond ((member (downcase shown) '("yes" "y" "on" "true" "x" "t"))
              (or (seq-first (alist-get 'states field)) "Yes"))
             ((member (downcase shown) '("no" "n" "off" "false" "" "nil")) "Off")
             (t (user-error "%s is yes or no" name))))
      ((or "combobox" "listbox" "radio")
       (let ((choices (append (or (alist-get 'options field) (alist-get 'states field)) nil)))
         (or (and (string-empty-p shown) "")
             (seq-find (lambda (choice) (string-equal-ignore-case choice shown)) choices)
             (user-error "%s is one of %s" name (string-join choices ", ")))))
      (_ shown))))

(defun vellumory-form--operation (fields values)
  "The fill-form operation for the VALUES that differ from FIELDS."
  (let ((changed (make-hash-table :test #'equal)))
    (dolist (field fields)
      (let* ((name (alist-get 'name field))
             (shown (cdr (assoc name values))))
        (when (and shown (not (eq (alist-get 'read_only field) t))
                   (not (equal shown (vellumory-form--shown field))))
          (puthash name (vellumory-form--value field shown) changed))))
    (when (zerop (hash-table-count changed)) (user-error "Nothing changed"))
    `((op . "fill-form") (values . ,changed))))

(defun vellumory-form-commit ()
  "Record the form's changes as one undoable edit."
  (interactive)
  (let ((source vellumory-form--source)
        (operation (vellumory-form--operation vellumory-form--fields (vellumory-form--values))))
    (unless (buffer-live-p source) (user-error "The document has been closed"))
    (quit-window t)
    (with-current-buffer source (vellumory--edit operation))))

(defun vellumory-form-cancel () "Discard the form." (interactive) (quit-window t))

(defvar vellumory-form-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'vellumory-form-commit)
    (keymap-set map "C-c C-k" #'vellumory-form-cancel)
    map))

(define-derived-mode vellumory-form-mode text-mode "PDF Form"
  "Fill a document's form; \\[vellumory-form-commit] records it.
\\{vellumory-form-mode-map}"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<vellumory-form-mode-map>\\[vellumory-form-commit] record  \\[vellumory-form-cancel] cancel")))

;;;###autoload
(defun vellumory-fill-form ()
  "Fill in this document's form fields."
  (interactive)
  (unless vellumory--source-file (user-error "Only documents visited from a local file can be edited"))
  (let ((source (current-buffer))
        (name (format "*Form: %s*" (file-name-nondirectory vellumory--source-file))))
    (vellumory--rpc "form-fields" nil
                    (lambda (reply)
                      (let ((fields (alist-get 'result reply)))
                        (cond ((alist-get 'error reply) (message "Vellumory: %s" (alist-get 'error reply)))
                              ((not (seq-some #'vellumory-form--fillable-p fields))
                               (message "This document has no form to fill"))
                              (t (let ((buffer (get-buffer-create name)))
                                   (with-current-buffer buffer
                                     (vellumory-form-mode)
                                     (setq vellumory-form--source source)
                                     (vellumory-form--render fields))
                                   (pop-to-buffer buffer)))))))))

;;;; On the page: hover help and click to fill

(defvar vellumory--hash)
(defvar vellumory--pages)
(declare-function vellumory--redecorate-page "vellumory-select" (page))
(declare-function vellumory--points-per-pixel "vellumory" (page))
(declare-function vellumory--tile-row "vellumory" (tile))
(declare-function vellumory--tile-page "vellumory" (tile))
(declare-function vellumory--tile-left "vellumory" (tile))
(declare-function vellumory--tile-right "vellumory" (tile))
(declare-function vellumory--row-top "vellumory" (row))
(declare-function vellumory--row-bottom "vellumory" (row))

(defvar-local vellumory--form-fields nil "(HASH . vector of fields, or `pending').")

(defun vellumory--ensure-form-fields ()
  "The document's form fields once known; ask the worker the first time."
  (if (equal (car vellumory--form-fields) vellumory--hash)
      (let ((fields (cdr vellumory--form-fields))) (and (vectorp fields) fields))
    (setq vellumory--form-fields (cons vellumory--hash 'pending))
    (let ((hash vellumory--hash))
      (vellumory--rpc "form-fields" nil
                      (lambda (reply)
                        (when (equal hash vellumory--hash)
                          (let ((fields (or (alist-get 'result reply) [])))
                            (setq vellumory--form-fields (cons hash fields))
                            (dolist (page (delete-dups (mapcar (lambda (f) (alist-get 'page f)) (append fields nil))))
                              (when (and (integerp page) (>= page 0)) (vellumory--redecorate-page page))))))))
    nil))

(defun vellumory--form-field-at (page x y)
  "The fillable form field drawn at X, Y on PAGE (points), or nil."
  (seq-find (lambda (field)
              (let ((rect (alist-get 'rect field)))
                (and (vellumory-form--fillable-p field) rect (eql (alist-get 'page field) page)
                     (<= (aref rect 0) x (aref rect 2)) (<= (aref rect 1) y (aref rect 3)))))
            (vellumory--ensure-form-fields)))

(defun vellumory--form-areas (tile)
  "Image-map areas: each form field on TILE invites a click."
  (when vellumory--pages
    (let* ((row (vellumory--tile-row tile)) (page (vellumory--tile-page tile))
           (k (vellumory--points-per-pixel page))
           (left (vellumory--tile-left tile)) (top (vellumory--row-top row))
           (width (- (vellumory--tile-right tile) left)) (height (- (vellumory--row-bottom row) top))
           areas)
      (seq-doseq (field (vellumory--ensure-form-fields))
        (when-let* (((eql (alist-get 'page field) page)) ((vellumory-form--fillable-p field))
                    (rect (alist-get 'rect field)))
          (let ((x0 (max 0 (- (round (/ (aref rect 0) k)) left))) (y0 (max 0 (- (round (/ (aref rect 1) k)) top)))
                (x1 (min width (- (round (/ (aref rect 2) k)) left)))
                (y1 (min height (- (round (/ (aref rect 3) k)) top))))
            (when (and (< x0 x1) (< y0 y1))
              (push `((rect . ((,x0 . ,y0) . (,x1 . ,y1))) vellumory-form-field
                      (pointer ,(if (eq (alist-get 'read_only field) t) 'arrow 'hand)
                               help-echo ,(format "%s (%s)%s" (alist-get 'name field) (alist-get 'type field)
                                                  (if (eq (alist-get 'read_only field) t) ", read-only"
                                                    ": click to fill"))))
                    areas)))))
      areas)))

(defun vellumory-fill-field (field)
  "Ask for FIELD's new value (a checkbox toggles) and record it as an edit."
  (when (eq (alist-get 'read_only field) t) (user-error "%s is read-only" (alist-get 'name field)))
  (let* ((name (alist-get 'name field))
         (label (let ((l (alist-get 'label field))) (if (and l (not (string-empty-p l))) l name)))
         (current (or (alist-get 'value field) ""))
         (value (pcase (alist-get 'type field)
                  ("checkbox" (if (member current '("" "Off")) (or (seq-first (alist-get 'states field)) "Yes") "Off"))
                  ((or "combobox" "listbox" "radio")
                   (completing-read (format "%s: " label)
                                    (append (or (alist-get 'options field) (alist-get 'states field)) nil)
                                    nil t nil nil current))
                  (_ (read-string (format "%s: " label) current)))))
    (let ((values (make-hash-table :test #'equal)))
      (puthash name value values)
      (vellumory--edit `((op . "fill-form") (values . ,values))))))

(provide 'vellumory-form)
;;; vellumory-form.el ends here
