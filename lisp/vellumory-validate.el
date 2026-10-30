;;; vellumory-validate.el --- PDF/A, PDF/UA and WTPDF validation -*- lexical-binding: t; -*-

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
;; `vellumory-validate' runs veraPDF through `vellumory validate' and shows
;; the failed rules with their checks.  Checks that name a page jump there;
;; `next-error' (M-g n) walks them.  The verdict is veraPDF's: PDF/UA also has
;; requirements only a person can judge (meaningful alt text, reading order).

;;; Code:
(require 'text-property-search)
(require 'cl-lib)
(require 'button)

(defvar vellumory-program)
(defvar vellumory--source-file)
(declare-function vellumory--cli "vellumory-edit" (args callback &optional buffer input))
(declare-function vellumory-goto-page "vellumory" (page))

(defconst vellumory-validation-profiles
  '(("auto (from the document's metadata)" . "0")
    ("PDF/A-1a" . "1a") ("PDF/A-1b" . "1b") ("PDF/A-2a" . "2a") ("PDF/A-2b" . "2b")
    ("PDF/A-2u" . "2u") ("PDF/A-3a" . "3a") ("PDF/A-3b" . "3b") ("PDF/A-3u" . "3u")
    ("PDF/A-4" . "4") ("PDF/A-4e" . "4e") ("PDF/A-4f" . "4f")
    ("PDF/UA-1" . "ua1") ("PDF/UA-2" . "ua2")
    ("WTPDF 1.0 reuse" . "wt1r") ("WTPDF 1.0 accessibility" . "wt1a"))
  "Validation profiles of veraPDF and their --flavour values.")

(defvar-local vellumory--validation-source nil)

(defun vellumory--validation-jump (page)
  "Show zero-based PAGE of the validated document."
  (let ((source vellumory--validation-source))
    (unless (buffer-live-p source) (user-error "The document has been closed"))
    (pop-to-buffer source)
    (vellumory-goto-page (1+ page))))

(defvar-local vellumory--validation-report nil "The report of veraPDF, once it has answered.")
(defvar-local vellumory--preflight nil "Preflight findings, once known (a vector).")

(defun vellumory--validation-redraw ()
  "Show veraPDF's report or that it still works, then the checks beyond it."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if vellumory--validation-report
        (vellumory--validation-render vellumory--validation-report)
      (insert (propertize "Validating with veraPDF…\n\n" 'face 'shadow)))
    (vellumory--preflight-insert vellumory--preflight)
    (goto-char (point-min))))

(defun vellumory--preflight-insert (findings)
  "The FINDINGS of checks beyond veraPDF, their pages as buttons."
  (insert (propertize "Beyond veraPDF" 'face 'bold) "\n"
          (propertize "What breaks, leaks or surprises in any PDF, conformance claimed or not.\n" 'face 'shadow))
  (cond ((null findings) (insert (propertize "Checking…\n" 'face 'shadow)))
        ((zerop (length findings)) (insert (propertize "✓ Nothing found\n" 'face 'success)))
        (t (seq-doseq (finding findings)
             (let ((severity (alist-get 'severity finding)) (start (point)))
               (insert (propertize (pcase severity ("error" "✗ ") ("warning" "⚠ ") (_ "· "))
                                   'face (pcase severity ("error" 'error) ("warning" 'warning) (_ 'shadow)))
                       (alist-get 'message finding))
               (seq-doseq (page (alist-get 'pages finding))
                 (insert " ")
                 (insert-text-button (format "p. %d" page) 'help-echo "Show this page"
                                     'action (let ((zero (1- page))) (lambda (_) (vellumory--validation-jump zero)))))
               (insert "\n")
               (put-text-property start (point) 'vellumory-finding finding))))))

(defun vellumory--validation-group (checks)
  "CHECKS as (CHECK . COUNT), identical page and message counted once, in order."
  (let (groups)
    (seq-doseq (check checks)
      (let ((same (seq-find (lambda (g) (and (equal (alist-get 'page (car g)) (alist-get 'page check))
                                              (equal (alist-get 'message (car g)) (alist-get 'message check))))
                            groups)))
        (if same (setcdr same (1+ (cdr same))) (push (cons check 1) groups))))
    (nreverse groups)))

(defun vellumory--preflight-summary (findings)
  "One line counting FINDINGS by severity, or nil while unknown."
  (when findings
    (let ((counts (mapcar (lambda (severity) (seq-count (lambda (f) (equal (alist-get 'severity f) severity)) findings))
                          '("error" "warning" "note"))))
      (if (zerop (length findings)) "Beyond veraPDF: nothing found"
        (format "Beyond veraPDF: %s (below, b)"
                (string-join (delq nil (cl-mapcar (lambda (n name) (and (> n 0) (format "%d %s%s" n name (if (= n 1) "" "s"))))
                                                  counts '("error" "warning" "note")))
                             ", "))))))

(defun vellumory--validation-render (report)
  "Insert veraPDF's REPORT: the verdict, the counts and each failed rule.
Under each rule, identical checks share one line."
  (let ((inhibit-read-only t) (compliant (alist-get 'compliant report)))
    (insert (propertize (or (alist-get 'profile report) "Validation") 'face 'bold) "  "
            (propertize (if compliant "COMPLIANT" "NOT COMPLIANT")
                        'face (if compliant 'success 'error))
            "\n"
            (format "%s rules failed (%s checks), %s passed (%s checks)\n"
                    (alist-get 'failed_rules report) (alist-get 'failed_checks report)
                    (alist-get 'passed_rules report) (alist-get 'passed_checks report))
            (propertize "Machine validation by veraPDF; some PDF/UA requirements need human review.\n"
                        'face 'shadow)
            (if-let* ((summary (vellumory--preflight-summary vellumory--preflight)))
                (concat (propertize summary 'face 'bold) "\n\n")
              "\n"))
    (cl-loop for rule across (alist-get 'rules report) do
             (let ((start (point)))
               (insert (propertize "✗ " 'face 'error)
                       (propertize (format "%s §%s-%s" (alist-get 'specification rule) (alist-get 'clause rule)
                                           (alist-get 'test_number rule))
                                   'face 'font-lock-keyword-face)
                       (format "  %s×\n" (alist-get 'failed_checks rule))
                       "  " (alist-get 'description rule) "\n")
               (put-text-property start (point) 'vellumory-rule rule))
             ;; Identical checks (one rule failing 225 times on a page) are one line.
             (pcase-dolist (`(,check . ,count) (vellumory--validation-group (alist-get 'checks rule)))
               (let ((page (alist-get 'page check)) (start (point)))
                 (insert "    ")
                 (if page
                     (insert-text-button (format "page %d" (1+ page))
                                         'action (lambda (_) (vellumory--validation-jump page))
                                         'help-echo "Show this page")
                   (insert (propertize "document" 'face 'shadow)))
                 (insert " · " (or (alist-get 'message check) "")
                         (if (> count 1) (propertize (format "  ×%d" count) 'face 'shadow) "") "\n")
                 (put-text-property start (point) 'vellumory-check check)))
             (insert "\n"))))

(defun vellumory--validation-next-error (n &optional reset)
  "`next-error-function': visit the Nth next check that names a page.
A negative N visits earlier checks; RESET starts from the top."
  (when reset (goto-char (point-min)))
  (let ((search (if (> n 0) #'text-property-search-forward #'text-property-search-backward))
        (count (abs n)) found)
    ;; Each check is one line: leave the current one before searching.
    (if (< n 0) (beginning-of-line) (unless reset (end-of-line)))
    (while (and (> count 0)
                (setq found (funcall search 'vellumory-check nil
                                     (lambda (_ v) (and v (alist-get 'page v))) t)))
      (cl-decf count))
    (unless found (user-error "No more checks with pages"))
    (goto-char (prop-match-beginning found))
    (vellumory--validation-jump (alist-get 'page (prop-match-value found)))))

(defun vellumory-validation-next-rule () "Next failed rule." (interactive)
  (forward-line 1)
  (while (and (not (eobp)) (not (and (get-text-property (point) 'vellumory-rule)
                                     (looking-at "✗"))))
    (forward-line 1)))

(defun vellumory-validation-previous-rule () "Previous failed rule." (interactive)
  (forward-line -1)
  (while (and (not (bobp)) (not (and (get-text-property (point) 'vellumory-rule) (looking-at "✗"))))
    (forward-line -1)))

(defun vellumory-validation-beyond ()
  "Go to the checks beyond veraPDF."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^Beyond veraPDF$")
  (beginning-of-line)
  (when (eq (window-buffer) (current-buffer)) (recenter 0)))

(defvar vellumory-validation-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "n" #'vellumory-validation-next-rule)
    (keymap-set map "p" #'vellumory-validation-previous-rule)
    (keymap-set map "b" #'vellumory-validation-beyond)
    (keymap-set map "TAB" #'forward-button)
    (keymap-set map "<backtab>" #'backward-button)
    map))

(define-derived-mode vellumory-validation-mode special-mode "PDF Validation"
  "Browse veraPDF results and the checks beyond them.
RET on a page shows it; \\[next-error] walks checks.
\\{vellumory-validation-mode-map}"
  (setq-local truncate-lines nil next-error-function #'vellumory--validation-next-error))

;;;###autoload
(defun vellumory-validate (flavour)
  "Validate against FLAVOUR (a veraPDF profile) and show the report."
  (interactive (list (cdr (assoc (completing-read "Validate against: " vellumory-validation-profiles nil t)
                                 vellumory-validation-profiles))))
  (let* ((file (or vellumory--source-file (user-error "Validation needs a local file")))
         (source (current-buffer))
         (buffer (get-buffer-create (format "*Validation: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buffer
      (vellumory-validation-mode)
      (setq vellumory--validation-source source vellumory--validation-report nil vellumory--preflight nil)
      (vellumory--validation-redraw))
    (display-buffer buffer)
    ;; The checks beyond veraPDF take milliseconds; show them while it works.
    (vellumory--cli (list "preflight" file)
                    (lambda (findings)
                      (with-current-buffer buffer
                        (setq vellumory--preflight (or findings []))
                        (vellumory--validation-redraw)))
                    buffer)
    (vellumory--cli (list "validate" "--flavour" flavour file)
                    (lambda (report)
                      (with-current-buffer buffer
                        (setq vellumory--validation-report report)
                        (vellumory--validation-redraw)
                        (setq next-error-last-buffer buffer)))
                    buffer)))

(provide 'vellumory-validate)
;;; vellumory-validate.el ends here
