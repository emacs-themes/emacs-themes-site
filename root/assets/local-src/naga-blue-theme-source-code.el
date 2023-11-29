(deftheme naga-blue
  "Created 2021-09-26.")

(require 'color)

(defgroup naga-blue-theme nil
  "Configuration options for the `naga-blue' theme."
  :group 'faces)

(defcustom naga-blue-theme-modeline-style 'green-box
  "The modeline style to use.
The default style is green text in a green box."
  :group 'naga-blue-theme
  :type '(choice
          (const :tag "Green box" green-box)
          (const :tag "Golden box" golden-box)
          (const :tag "Filled green" filled-green)))

(defcustom naga-blue-theme-use-lighter-org-block-background t
  "Whether to set a background for the `org-block' face.
The default is to use a slightly lighter color than the usual
background.  Setting this to `nil' means blocks have no special
background color."
  :group 'naga-blue-theme
  :type 'boolean)

(defmacro create-theme-colors ()
  "Expects the color variables to be bound."
  '(mapcar
    (lambda (entry)
      (list (car entry)
            `((t ,@(cdr entry)))))
    `((default (:foreground ,fg :background ,bg))
      (minibuffer-prompt (:foreground ,string))
      (highlight (:foreground ,fg :background ,region))
      (region (:background ,region))
      (secondary-selection (:foreground ,red :background "#1A0404"))
      (vertical-border (:foreground "gray30"))
      (help-key-binding (:foreground ,fg :background ,bg-blue :box ,fg))
      (link (:foreground ,fnct :underline t))
      (font-lock-builtin-face (:foreground ,keyword-blue))
      (font-lock-comment-face (:foreground ,comment))
      (font-lock-constant-face (:foreground ,purple))
      (font-lock-doc-face (:slant oblique :foreground ,docstring))
      (font-lock-function-name-face (:foreground ,fnct))
      (font-lock-keyword-face (:foreground ,kwd))
      (font-lock-preprocessor-face (:inherit (font-lock-constant-face)))
      (font-lock-string-face (:foreground ,string))
      (font-lock-type-face (:foreground ,sea-green))
      (font-lock-variable-name-face (:foreground ,green-yellow))
      (font-lock-warning-face (:slant italic :foreground ,keyword-blue-red))
      (fringe (:background ,bg))
      (warning (:foreground ,keyword-blue-red :weight regular))
      (header-line (:foreground ,gold ))
      (mode-line ,(cond
                   ((eq naga-blue-theme-modeline-style 'golden-box)
                    `(:background ,bg :foreground ,gold :box ,gold))
                   ((eq naga-blue-theme-modeline-style 'filled-green)
                    `(:background ,fg-dark :foreground ,bg :box ,bg))
                   ((eq naga-blue-theme-modeline-style 'green-box)
                    `(:background ,bg-blue :foreground ,fg :box ,fg))))
      (mode-line-buffer-id (:weight bold))
      (mode-line-emphasis (:weight bold))
      (mode-line-inactive (:box "#555555" :background ,bg :foreground ,comment))
      (isearch (:foreground ,bg :weight semi-bold :background ,gold))
      (lazy-highlight (:foreground ,fg :background "blue3"))
      (show-paren-match (:foreground ,bg :background ,red))
      (show-paren-mismatch (:foreground "red" :background ,dark-yellow))
      (completions-common-part (:inherit 'orderless-match-face-0))
      (error (:foreground ,red))
      (compilation-mode-line-run (:foreground ,kwd))
      (cursor (:background ,fg))
      (shadow (:foreground ,comment-light))
      (match (:foreground ,kwd :background ,bg-blue :slant oblique))

      ;; orderless
      (orderless-match-face-0 (:foreground ,purple))
      (orderless-match-face-1 (:foreground ,keyword-blue))
      (orderless-match-face-2 (:foreground ,string))
      (orderless-match-face-3 (:foreground ,comment-light))

      ;; outline-*, and by extension org-level-*
      (outline-1 (:weight bold :foreground "#00EBEB"))
      (outline-2 (:foreground ,gold))
      (outline-3 (:foreground ,fnct))
      (outline-4 (:foreground ,green-yellow))
      (outline-5 (:foreground ,purple))
      (outline-6 (:foreground ,string))
      (outline-7 (:foreground ,sea-green))
      (outline-8 (:foreground "dark khaki"))

      ;; company
      (company-tooltip (:background "gray10"))
      (company-tooltip-common (:foreground ,keyword-blue))
      (company-tooltip-selection (:background ,dark-yellow :weight bold))

      ;; corfu
      (corfu-current (:inherit 'highlight))
      (corfu-default (:background "#090909"))
      (corfu-border (:background ,fg-dark))
      (corfu-bar (:background ,comment-light))

      ;; which-key
      (which-key-key-face (:foreground ,kwd))
      (which-key-group-description-face (:foreground ,sea-green))
      (which-key-command-description-face (:foreground ,fg))

      ;; marginalia
      (marginalia-file-priv-dir (:inherit 'font-lock-keyword-face))
      (marginalia-file-priv-no (:inherit 'font-lock-comment-face))
      (marginalia-file-priv-exec (:inherit 'font-lock-function-name-face))
      (marginalia-file-priv-link (:inherit 'font-lock-keyword-face))
      (marginalia-file-priv-rare (:inherit 'font-lock-variable-name-face))
      (marginalia-file-priv-read (:inherit 'font-lock-type-face))
      (marginalia-file-priv-write (:inherit 'font-lock-builtin-face))
      (marginalia-file-priv-other (:inherit 'font-lock-constant-face))
      (marginalia-date (:foreground ,gold))
      (marginalia-number (:inherit 'font-lock-constant-face))

      ;; dired and related
      (diredfl-dir-name (:foreground ,string))
      (diredfl-dir-heading (:slant oblique :weight bold :foreground ,sea-green))
      (diredfl-file-name (:foreground ,fg))
      (diredfl-file-suffix (:foreground ,fg))
      (diredfl-ignored-file-name (:inherit (font-lock-comment-face)))
      (diredfl-dir-priv (:inherit 'marginalia-file-priv-dir))
      (diredfl-no-priv (:inherit 'marginalia-file-priv-no))
      (diredfl-exec-priv (:inherit 'marginalia-file-priv-exec))
      (diredfl-link-priv (:inherit 'marginalia-file-priv-link))
      (diredfl-rare-priv (:inherit 'marginalia-file-priv-rare))
      (diredfl-read-priv (:inherit 'marginalia-file-priv-read))
      (diredfl-other-priv (:inherit 'marginalia-file-priv-other))
      (diredfl-write-priv (:inherit 'marginalia-file-priv-write))
      (diredfl-compressed-file-suffix (:foreground ,fg-dark :slant italic))
      (diredfl-compressed-file-name (:foreground ,fg-dark :slant italic))
      (diredfl-symlink (:foreground ,fnct))
      (diredfl-deletion (:foreground ,keyword-blue-red))
      (diredfl-deletion-file-name (:foreground ,keyword-blue-red))
      (diredfl-flag-mark-line (:background "#033903"))
      (diredfl-flag-mark (:weight bold :foreground ,fnct))
      (diredfl-date-time (:inherit 'marginalia-date))
      (diredfl-number (:inherit 'marginalia-number))

      ;; line numbers
      (line-number (:foreground "#052E3D"))
      (line-number-current-line (:foreground "#07404c"))

      ;; org
      (org-tag (:foreground ,fg-dark :weight bold))
      (org-todo (:foreground ,green-yellow :weight bold))
      (org-done (:foreground ,fg :weight bold))
      (org-headline-todo (:foreground "green-yellow"))
      (org-headline-done (:foreground ,comment :strike-through t))
      (org-document-title (:foreground ,fnct :weight bold))
      (org-document-info (:foreground ,fnct))
      (org-document-info-keyword (:foreground ,fnct))
      (org-verbatim (:foreground ,purple))
      (org-code (:foreground ,string))
      (org-block (:background ,(if naga-blue-theme-use-lighter-org-block-background
                                   block
                                 bg)))
      (org-block-begin-line (:slant oblique :foreground ,comment-dark))
      (org-block-end-line (:slant oblique :foreground ,comment-dark))
      (org-special-keyword (:foreground ,comment))
      (org-agenda-date-today (:foreground ,green-yellow :weight bold))

      ;; magit
      (magit-section-heading (:foreground ,keyword-blue :weight semi-bold))
      (magit-section-highlight (:background ,dark-yellow))
      (magit-branch-local (:foreground ,kwd))
      (magit-branch-remote (:foreground ,fnct))
      (magit-tag (:foreground ,string))
      (magit-diff-file-heading-highlight (:background ,dark-yellow))
      (magit-diff-context-highlight (:background ,block-light :foreground ,grey))
      (magit-diff-context (:foreground ,comment))
      (magit-diff-hunk-heading (:background "#181818" :foreground ,comment-light :slant oblique))
      (magit-diff-hunk-heading-highlight (:slant oblique :weight bold :background "#3f3f3f" :foreground "#b5c5b5"))

      ;; manpages
      (Man-overstrike (:foreground ,fnct))

      ;; mu4e
      (mu4e-highlight-face (:weight semi-bold :foreground ,keyword-blue))

      ;; whitespace-mode
      (whitespace-space (:foreground ,whitespace-fg :background ,bg))
      (whitespace-tab (:foreground ,whitespace-fg :background ,bg))
      (whitespace-line (:foreground ,keyword-blue-red :background ,bg))
      (whitespace-newline (:foreground ,whitespace-fg :background ,bg))
      (whitespace-empty (:foreground ,red :background ,kwd))
      (whitespace-indentation (:foreground ,red :background ,kwd))
      (whitespace-space-before-tab (:foreground ,red :background ,keyword-blue))
      (whitespace-space-after-tab (:foreground ,red :background ,kwd))
      (whitespace-missing-newline-at-eof (:background ,string))
      (whitespace-trailing (:background ,red))
      (whitespace-big-indent (:background ,red))

      ;; shortdoc
      (shortdoc-section (:inherit 'default))
      (shortdoc-heading (:inherit 'default :weight bold :height 1.3))

      ;; gnus and message-mode
      (gnus-header (:inherit default))

      ;; helm
      (helm-match (:inherit 'orderless-match-face-0))
      (helm-source-header (:foreground ,bg :background ,fg))
      (helm-header (:foreground ,sea-green))
      (helm-selection (:foreground ,fg :background ,dark-yellow))
      (helm-M-x-key (:foreground ,gold :background ,bg :box ,gold))
      (helm-ff-directory (:foreground ,string :background ,bg))
      (helm-buffer-directory (:inherit helm-ff-directory))
      (helm-ff-dotted-directory (:foreground ,fg :background ,bg))
      (helm-ff-dotted-symlink-directory (:foreground ,dark-yellow :background ,bg))

      ;; ivy
      (ivy-minibuffer-match-face-1 (:foreground ,fg))
      (ivy-minibuffer-match-face-2 (:inherit 'orderless-match-face-0))
      (ivy-minibuffer-match-face-3 (:inherit 'orderless-match-face-1))
      (ivy-minibuffer-match-face-4 (:inherit 'orderless-match-face-2))

      ;; envrc
      (envrc-mode-line-none-face (:foreground ,fg))
      (envrc-mode-line-on-face (:foreground ,string))
      (envrc-mode-line-error-face (:inherit 'error))

      ;; vterm NOTE: vterm doesn't use the whole face description (or
      ;; these would not make sense at all), but rather seems to pick
      ;; either foreground or background color as actual foreground,
      ;; hence the duplicated color values.
      (vterm-color-red (:foreground ,red :background ,red))
      (vterm-color-blue (:foreground ,fnct :background ,fnct))
      (vterm-color-black (:foreground ,comment :background ,comment))
      (vterm-color-yellow (:foreground ,gold :background ,gold))
      (vterm-color-green (:foreground ,string :background ,string))
      (vterm-color-fnct (:foreground ,fnct :background ,fnct))
      (vterm-color-white (:foreground ,fg :background ,bg))
      (vterm-color-magenta (:foreground ,purple :background ,purple))

      (swiper-line-face (:inherit 'region))
      (swiper-match-face-1 (:background "#330000" :foreground "white"))
      (erc-notice-face (:foreground ,fg-dark))
      (erc-header-line (:background ,bg-blue :foreground ,fg :box ,fg))
      (erc-input-face (:foreground ,gold))
      (erc-my-nick-face (:foreground, gold))
      (erc-current-nick-face (:foreground ,gold))
      (erc-direct-msg-face (:foreground ,sea-green))
      (erc-nick-msg-face (:foreground ,sea-green))
      (erc-timestamp-face (:foreground ,fnct :weight bold))
      (ivy-current-match (:inherit nil :background ,bg-blue :box ,fg  ))
      (ivy-highlight-face (:background nil))
      (persp-selected-face (:foreground ,keyword-blue-red :weight bold))
      

      ;; eglot
      (eglot-highlight-symbol-face (:foreground ,fg :background ,bg-blue :weight bold)))))


;; Set all the colors to their actual values.
(let ((bg "#000000")
      (bg-blue "#04041a")
      (fg "#6afeff")
      (fg-dark "#077788")
      (kwd "#18b2fd")
      (gold "#ffce80")
      (fnct "#a2b7ff")
      (string "#b9ffc2")
      (purple "#b8b3ff")
      (keyword-blue "#19b3fe")
      (comment "#707370")
      (comment-light "#909590")
      (comment-dark "#353535")
      (docstring "#b9ffc2")
      (grey "#aabaaa")
      (dark-yellow "#222200")
      (region "#222222")
      (sea-green "#3cb371")
      (keyword-blue-red "#ff8426")
      (red "#ffce80")
      (whitespace-fg "#555f55")
      (block "#060606")
      (block-light "#252525")
      (green-yellow "#73f273"))
  (apply #'custom-theme-set-faces
         (cons 'naga-blue (create-theme-colors))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'naga-blue)

;;; naga-blue-theme.el ends here
