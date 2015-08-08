;; Comments are things that are missing
   
   `(default ((t (,@default-color ,@fmt-none))))
   ;;cursor
   `(error ((t (,@err-like ,@fmt-none))))
   `(escape-glyph-face ((t (,@err-like ,@fmt-none))))
   `(fringe ((t (,@standout-dull ,@fmt-none))))
   `(linum ((t (,@less-distraction ,@fmt-none))))
   ;; hl-line
   `(hl-line ((t (:underline t))))
   `(highlight ((t (,@fmt-bld))))
   `(region ((t (:inverse-video t))))
   ;; isearch
   ;; isearch-fail
   ;; lazy-highlight
   `(link ((t (,@link-like ,@fmt-und))))
   `(link-visited ((t (,@link-like ,@fmt-und))))
   ;;menu
   `(minibuffer-prompt ((t (,@standout-dull ,@fmt-bld))))
   `(mode-line ((t (,@standout-dull ,@fmt-rev))))
   `(mode-line-inactive ((t (,@standout-dull ,@fmt-rev))))
   ;;secondary-selection
   ;;shadow
   `(trailing-whitespace ((t (,@err-like ,@fmt-rev))))
   ;;vertical-border
   ;;comint-highlight-prompt
   ;;compilation-info
   `(compilation-info ((t (,@success-like ,@fmt-bld))))
   `(compilation-warning ((t (,@err-like ,@fmt-bld))))
   ;; custom-button
   ;; custom-button-mouse
   ;; custom-button-pressed
   ;; custom-changed
   ;; custom-comment-tag
   ;; custom-documentation
   ;; custom-group-tag
   ;; custom-group-tag-1
   ;; custom-invalid
   ;; custom-link
   ;; custom-state
   ;; custom-variable-tag
   ;; diff-file-header
   ;; diff-header
   `(ido-only-match ((t (,@success-like ,@fmt-none))))
   `(ido-subdir ((t (,@default-color ,@fmt-bld))))
   `(ido-first-match ((t (,@link-like ,@fmt-und))))
   ;; emacs-wiki-bad-link-face
   ;; emacs-wiki-link-face
   ;; emacs-wiki-verbatim-face
   ;; eshell-ls-archive
   ;; eshell-ls-backup
   ;; eshell-ls-clutter
   ;; eshell-ls-directory
   ;; eshell-ls-executable
   ;; eshell-ls-missing
   ;; eshell-ls-product
   ;; eshell-ls-readonly
   ;; eshell-ls-special
   ;; eshell-ls-symlink
   ;; eshell-ls-unreadable
   ;; eshell-prompt
   `(font-lock-builtin-face ((t (,@lessbright ,@fmt-bld))))
   `(font-lock-comment-face ((t (,@less-distraction ,@fmt-none))))
   `(font-lock-constant-face ((t (,@lessbright ,@fmt-none))))
   `(font-lock-function-name-face ((t (,@lessbright ,@fmt-none))))
   `(font-lock-keyword-face ((t (,@brighter ,@fmt-bld))))
   `(font-lock-string-face ((t (,@standout-dull ,@fmt-none))))
   `(font-lock-type-face ((t (,@brighter ,@fmt-bld))))
   `(font-lock-variable-name-face ((t (,@lessbright ,@fmt-none))))
   `(font-lock-warning-face ((t (,@err-like ,@fmt-none))))
   `(font-lock-doc-face ((t (,@less-distraction ,@fmt-bld))))
   ;; font-lock-doc-string-face
   ;; font-lock color-constant-face
   `(font-lock-comment-delimiter-face ((t (,@lessbright ,@fmt-none))))
   `(font-lock-preprocessor-face ((t (,@standout-dull ,@fmt-none))))
   ;; font-lock-reference-face
   ;; font-lock-negation-char-face
   ;; font-lock-other-type-face
   `(font-lock-special-keyword-face ((t (,@brighter ,@fmt-bldi))))
   ;; font-lock-exit-face
   `(font-lock-other-emphasized-face ((t (,@default-color ,@fmt-bld))))
   ;; font-lock-regexp-grouping-backslash
   ;; info-xref
   ;; info-xref-visited
   `(org-todo ((t (,@err-like ,@fmt-bld))))
   `(org-todo-keyword-face ((t (,@err-like ,@fmt-none))))
   `(org-done ((t (,@success-like ,@fmt-bld))))
   `(org-done-keyword-face ((t (,@success-like ,@fmt-none))))
   `(table-cell ((t (,@default-color ,@fmt-none))))

   ;; outline-1
   ;; outline-2
   ;; outline-3
   ;; outline-4
   ;; outline-5
   ;; outline-6
   ;; outline-7
   ;; outline-8

   ;; speedbar-button-face
   ;; speedbar-directory-face
   ;; speedbar-file-face
   ;; speedbar-highlight-face
   ;; speedbar-selected-face
   ;; speedbar-separator-face
   ;; speedbar-tag-face
   `(show-paren-match ((t (:background "gray10"
			   ,@fmt-rev))))
   `(show-paren-mismatch ((t (:background "red" :foreground "black" ,@fmt-rev))))
   ;; widget-field
   ;;widget-single-line
   `(flymake-errline ((t (,@err-like ,@fmt-bld))))
   `(flymake-warnline ((t (,@err-like ,@fmt-none))))
   ;; column-marker-1
   ;; column-marker-2
   ;; column-marker-3

   ;; jabber-stuff
   ;; gnus-stuff
   ;; message-stuff
   `(paren-face ((t (,@default-color))))
   ;; rainbow-delimiters-stuff
   ;; slime-stuff
   ;; whitespace-stuff
   ;; rcirc-stuff
   ;; erc-stuff
   `(font-latex-warning-face ((t (,@err-like ,@fmt-none))))
   ;; font-latex-sectioning-5-face

   `(persp-selected-face ((t (,@fmt-bld))))
   `(magit-item-highlight ((t (,@fmt-bld))))
   `(magit-log-sha1 ((t (,@link-like))))
   `(lazy-highlight ((t (:background ,bluish :foreground ,black))))
   `(isearch-fail ((t (:background ,reddish :foreground ,black))))
   `(isearch ((t (:background ,greenish :foreground ,black))))

   `(org-document-title ((t (:height 1.7 ,@link-like ,@fmt-bld))))
   `(org-block ((t (,@standout-dull ,@fmt-none))))
   `(org-block-begin-line ((t (,@standout-dull ,@fmt-bld))))
   `(org-block-end-line ((t (,@standout-dull ,@fmt-bld))))
   `(org-level-1 ((t (:height 1.5 :background ,bright-black :foreground ,white ,@fmt-bld))))
   `(org-level-2 ((t (:height 1.4 :background ,bright-black ,@fmt-bld))))
   `(org-level-3 ((t (:height 1.3 :background ,bright-black ,@fmt-bld))))
   `(org-level-4 ((t (:height 1.2 :background ,bright-black ,@fmt-bld))))
   `(org-level-5 ((t (:height 1.1 :background ,bright-black ,@fmt-bld))))
   `(org-level-6 ((t (:height 1.0 :background ,bright-black ,@fmt-bld))))
   `(org-level-7 ((t (:height 1.0 :background ,bright-black ,@fmt-none))))
   `(org-level-8 ((t (:height 1.0 :background ,bright-black :foreground ,dull-white ,@fmt-none))))
   )))


(provide-theme 'eltbus)


