;;; tools/vellumory/config.el -*- lexical-binding: t; -*-

(use-package! vellumory
  :defer t
  :init
  ;; File associations, the no-read visit handler and the large-file
  ;; exemption. Only vellumory-install.el loads now; the reader loads on the
  ;; first visit. Package builds without package.el (Nix, unstraightened)
  ;; ship but do not load the generated autoloads, so load them here.
  (require 'vellumory-autoloads)
  (vellumory-install)
  :config
  ;; doom-modeline shows the page; no duplicate header line.
  (setq vellumory-header-line nil)

  (set-popup-rule! "^\\*Text: " :side 'right :size 0.45 :select t :quit t :ttl nil)

  ;; Document buffers visit files, so they are real buffers, join workspaces
  ;; and are offered by `switch-to-buffer'. Sidebars are not real buffers.
  (setq-hook! 'vellumory-mode-hook doom-real-buffer-p t)

  (defun +vellumory-escape-h ()
    "Clear search highlights or the selection before other ESC handlers."
    (when (derived-mode-p 'vellumory-mode)
      (cond ((not (string-empty-p vellumory--search-query)) (vellumory-search-clear) t)
            (vellumory--selection (vellumory-clear-selection) t))))
  (add-hook 'doom-escape-hook #'+vellumory-escape-h -50)

  (when (modulep! :editor evil)
    (require 'vellumory-evil)
    (vellumory-evil-setup))

  (map! :map vellumory-mode-map
        :localleader
        :desc "Contents"           "o" #'vellumory-outline
        :desc "Page previews"      "t" #'vellumory-thumbnails
        :desc "Go to heading"      "h" #'vellumory-goto-heading
        :desc "Go to page"         "g" #'vellumory-goto-page
        :desc "Go to page label"   "l" #'vellumory-goto-page-label
        :desc "Find text"          "s" #'vellumory-search
        :desc "Clear search"       "S" #'vellumory-search-clear
        :desc "Follow link"        "f" #'vellumory-follow-link
        :desc "Copy selection"     "y" #'vellumory-copy-selection
        :desc "Page text"          "T" #'vellumory-page-text
        :desc "Document details"   "i" #'vellumory-details
        :desc "All commands"       "?" #'vellumory-dispatch
        :desc "Manual"             "M" #'vellumory-info
        :desc "Check the setup"    "!" #'vellumory-doctor
        :desc "Validate (PDF/A, UA)" "v" #'vellumory-validate
        :desc "PDF syntax (QDF)"   "d" #'vellumory-syntax
        :desc "Compare versions"   "=" #'vellumory-diff
        :desc "Cycle page colours" "c" #'vellumory-cycle-theme
        :desc "Fit width"          "w" #'vellumory-fit-width
        :desc "Fit page"           "p" #'vellumory-fit-page
        :desc "Two pages"          "2" #'vellumory-cycle-spread
        :desc "Occur (all matches)" "O" #'vellumory-occur
        :desc "Reload"             "r" #'vellumory-reload
        :desc "Back"               "[" #'vellumory-history-back
        :desc "Forward"            "]" #'vellumory-history-forward
        (:prefix ("e" . "edit (reversible)")
         :desc "Rotate clockwise"        "r" #'vellumory-rotate-pages
         :desc "Rotate counterclockwise" "R" #'vellumory-rotate-pages-counterclockwise
         :desc "Delete pages"            "d" #'vellumory-delete-pages
         :desc "Move page"               "m" #'vellumory-move-page
         :desc "Keep or reorder pages"   "k" #'vellumory-keep-pages
         :desc "Append documents"        "a" #'vellumory-append-documents
         :desc "Compress losslessly"     "c" #'vellumory-compress
         :desc "Page labels"             "l" #'vellumory-set-page-labels
         :desc "Attach file"             "A" #'vellumory-attach-file
         :desc "Recognize text (OCR)"    "o" #'vellumory-ocr
         :desc "Edit metadata"           "i" #'vellumory-edit-metadata
         :desc "Fill in form"            "f" #'vellumory-fill-form
         :desc "Stamp or watermark"      "w" #'vellumory-stamp
         :desc "Encrypt"                 "E" #'vellumory-encrypt
         :desc "Remove encryption"       "P" #'vellumory-decrypt
         :desc "Undo"                    "u" #'vellumory-undo
         :desc "Redo"                    "U" #'vellumory-redo
         :desc "History tree"            "h" #'vellumory-history)
        (:prefix ("a" . "annotate")
         :desc "Highlight selection"  "h" #'vellumory-highlight
         :desc "Underline selection"  "u" #'vellumory-underline
         :desc "Strike out selection" "s" #'vellumory-strike-out
         :desc "Squiggly underline"   "q" #'vellumory-squiggly
         :desc "Add note"             "n" #'vellumory-add-note
         :desc "List annotations"     "l" #'vellumory-annotations)
        (:prefix ("x" . "export")
         :desc "Extract pages" "x" #'vellumory-extract-pages
         :desc "Split"         "s" #'vellumory-split
         :desc "Text"          "t" #'vellumory-export-text)))

(map! :after dired
      :map dired-mode-map
      :localleader
      (:prefix ("v" . "vellumory (marked)")
       :desc "Validate"       "v" #'vellumory-dired-validate
       :desc "Edit each"      "e" #'vellumory-dired-edit
       :desc "Summarize"      "i" #'vellumory-dired-info
       :desc "Merge into new" "m" #'vellumory-dired-merge))

(after! doom-modeline
  (doom-modeline-def-segment vellumory-pages
    "Page label, physical page and count of the document in view."
    (when (derived-mode-p 'vellumory-mode)
      (concat (doom-modeline-spc)
              (propertize (concat "Page " (vellumory--page-string))
                          'face (doom-modeline-face 'doom-modeline-info))
              (doom-modeline-spc)
              (propertize (format "%d%%%%" (round (* 100 vellumory--zoom)))
                          'face (doom-modeline-face))
              (unless (string-empty-p vellumory--search-status)
                (concat (doom-modeline-spc) vellumory--search-status))
              ;; The keyboard caret, which the header line shows elsewhere.
              (let ((caret (vellumory-caret--indicator)))
                (unless (string-empty-p caret)
                  (concat (doom-modeline-spc)
                          (propertize (string-remove-prefix " | " caret)
                                      'face (doom-modeline-face 'doom-modeline-warning))))))))
  (doom-modeline-def-modeline 'vellumory
    '(bar window-number modals matches buffer-info vellumory-pages)
    '(compilation misc-info major-mode process vcs time))
  (add-to-list 'doom-modeline-mode-alist '(vellumory-mode . vellumory)))
