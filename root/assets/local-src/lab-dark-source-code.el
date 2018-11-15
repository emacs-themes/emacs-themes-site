;;; lab-dark-theme.el --- A custom theme carefully constructed in the LAB space

;; Copyright (C) 2010--2018 MetroWind.

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What the Fuck You Want
;; to Public License, Version 2, as published by Sam Hocevar. See
;; http://www.wtfpl.net/ for more details.

;; Author: MetroWind <chris.corsair@gmail.com>
;; URL: https://github.com/MetroWind/lab-theme
;; Keywords: lisp
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;;
;; Lab theme is a custom theme for Emacs. It Has both light and dark
;; variants. This file provides dark variant.

;;; Code:

;; Note: for every face that is customized here, a customization for
;; it should be also provided in the dark version. Otherwise it could
;; be ugly when switching bwteen styles

(deftheme lab-dark
  "A custom theme carefully constructed in the LAB space (dark variant)")

;; Colors
(let*
    (
     ;; Angle 2 Saturation 0.36 lightness 75
     (color-blue "#31C6FF")
     (color-cyan "#10CFC0")
     (color-green "#8DC85F")
     (color-yellow "#ECA964")
     (color-red "#FF96B3")
     (color-purple "#D1A5FF")

     ;; Angle 1.588 lightness 50 saturation 0.45
     (color-dark-blue "#007BD9")
     (color-dark-cyan "#008FAC")
     (color-dark-green "#008C36")
     (color-dark-yellow "#897500")
     (color-dark-red "#D04245")
     (color-dark-purple "#BA48B4")

     ;; Angle 1.8
     ;; Saturation 0.03
     (color-overexpo "#EAF1F8")         ; 95
     (color-ultralight "#DCE3EA")       ; 90
     (color-mist "#CED5DC")             ; 85
     (color-light "#C0C7CE")            ; 80
     (color-bad "#A5ACB2")              ; 70
     ;; Saturation 0.07
     (color-buddha "#8592A1")           ; 60
     (color-middle "#6B7887")           ; 50
     (color-dell "#535F6E")             ; 40
     ;; Saturation 0.08
     (color-darkness "#324150")         ; 27
     (color-void "#273644")             ; 22

     (color-fg color-light)
     (color-bg color-darkness))

  (custom-theme-set-faces
   'lab-dark
   `(default ((t (:background ,color-bg
                  :foreground ,color-fg))))
   `(cursor ((t (:background ,color-red
                 :foreground ,color-fg))))
   `(region ((t (:background ,color-green
                 :foreground ,color-bg))))
   `(mode-line ((t (:background ,color-void
                    :foreground ,color-fg
                    :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,color-fg))))
   `(mode-line-inactive ((t (:background ,color-dell
                             :foreground ,color-fg))))
   `(fringe ((t (:background ,color-bg))))
   `(minibuffer-prompt ((t (:slant italic :foreground ,color-bad))))
   `(font-lock-builtin-face ((t (:foreground ,color-mist))))
   `(font-lock-comment-face ((t (:slant italic :foreground ,color-middle))))
   `(font-lock-constant-face ((t (:slant italic :foreground ,color-bad))))
   `(font-lock-function-name-face ((t (:foreground ,color-yellow))))
   `(font-lock-keyword-face ((t (:foreground ,color-mist :slant italic))))
   `(font-lock-string-face ((t (:foreground ,color-cyan))))
   `(font-lock-type-face ((t (:foreground ,color-green))))
   `(font-lock-variable-name-face ((t (:foreground ,color-blue))))
   `(font-lock-warning-face ((t (:foreground ,color-red))))

   `(isearch ((t (:background ,color-buddha
                  :foreground ,color-bg))))
   `(lazy-highlight ((t (:background ,color-void))))
   `(link ((t (:foreground ,color-blue :underline t))))
   `(link-visited ((t (:foreground ,color-middle :underline t))))
   `(button ((t (:foreground ,color-yellow :underline t :background nil))))
   `(header-line ((t (:background ,color-void
                      :foreground ,color-fg))))
   `(shadow ((t (:foreground ,color-bad))))
   `(show-paren-match ((t (:background ,color-dark-green :foreground ,color-fg))))
   `(show-paren-mismatch ((t (:background ,color-dark-red
                              :foreground ,color-bg))))
   `(highlight ((t (:inverse-video nil :background ,color-void))))
   `(hl-line ((t (:inverse-video nil :background ,color-void))))

   ;; Faces for specific prog modes
   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))

   ;; Dired
   `(dired-directory ((t (:foreground ,color-blue))))
   `(dired-symlink ((t (:foreground ,color-cyan))))
   `(dired-perm-write ((t (:foreground ,color-red))))

   ;; Diff
   `(diff-added ((t (:foreground ,color-blue))))
   `(diff-removed ((t (:foreground ,color-red))))
   ;; `(diff-context ((t (:background nil))))
   `(diff-file-header ((t (:bold t :background ,color-dell :weight bold))))
   `(diff-header ((t (:background ,color-void :foreground ,color-fg))))

   ;; Whitespace
   `(whitespace-trailing ((t (:background ,color-dell))))
   `(whitespace-line ((t (:background ,color-dell :foreground unspecified))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,color-blue
                          :weight unspecified))))
   `(erc-header-line ((t (:foreground ,color-bg :background ,color-void))))
   `(erc-timestamp-face ((t (:foreground ,color-middle
                             :weight unspecified))))
   `(erc-current-nick-face ((t (:foreground ,color-green
                                :weight unspecified))))
   `(erc-input-face ((t (:foreground ,color-yellow))))
   `(erc-prompt-face ((t (:foreground ,color-bad
                          :background nil
                          :slant italic
                          :weight unspecified))))
   `(erc-my-nick-face ((t (:foreground ,color-yellow))))
   `(erc-pal-face ((t (:foreground ,color-cyan))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,color-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,color-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,color-dark-cyan))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,color-dark-red))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,color-green))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,color-dark-purple))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,color-bad))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,color-red))))

   ;; Magit
   `(magit-branch-local ((t (:foreground ,color-blue :background nil))))
   `(magit-branch-remote ((t (:foreground ,color-green :background nil))))
   `(magit-tag ((t (:foreground ,color-dark-blue :background ,color-bg))))
   `(magit-hash ((t (:foreground ,color-middle))))
   `(magit-section-title ((t (:foreground ,color-green :background ,color-bg))))
   `(magit-section-heading ((t (:background ,color-bg :foreground ,color-fg))))
   `(magit-section-highlight ((t (:background ,color-bg))))
   `(magit-item-highlight ((t (:foreground ,color-fg :background ,color-dell))))
   `(magit-log-author ((t (:foreground ,color-yellow))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
   `(magit-diff-context ((t (:inherit diff-context))))
   `(magit-diff-context-highlight ((t (:inherit magit-diff-context))))
   `(magit-popup-argument ((t (:inherit font-lock-function-name-face))))
   `(magit-popup-disabled-argument ((t (:inherit font-lock-comment-face))))

   ;; Git-gutter-fringe
   `(git-gutter-fr:modified ((t (:foreground ,color-purple))))
   `(git-gutter-fr:added ((t (:foreground ,color-green))))
   `(git-gutter-fr:deleted ((t (:foreground ,color-red))))

   ;; Company
   `(company-preview ((t (:foreground ,color-fg :background ,color-red))))
   `(company-preview-common ((t (:foreground ,color-fg :background ,color-yellow))))
   `(company-tooltip ((t (:foreground ,color-fg :background ,color-void))))
   `(company-tooltip-common ((t (:foreground ,color-yellow))))
   `(company-tooltip-selection ((t (:background ,color-dell))))
   `(company-tooltip-common-selection ((t (:foreground ,color-yellow))))
   `(company-tooltip-annotation ((t (:foreground ,color-blue))))
   `(company-scrollbar-bg ((t (:background ,color-void))))
   `(company-scrollbar-fg ((t (:background ,color-dell))))

   ;; Powerline
   `(powerline-active2 ((t (:foreground ,color-fg :background ,color-bg))))
   `(powerline-active1 ((t (:foreground ,color-bg :background ,color-green))))
   `(powerline-inactive2 ((t (:foreground ,color-bg :background ,color-dell))))
   `(powerline-inactive1 ((t (:foreground ,color-fg :background ,color-void))))

   ;; Smart mode line
   `(sml/global  ((t (:foreground ,color-fg))))
   `(sml/charging ((t (:foreground ,color-green))))
   `(sml/discharging ((t (:foreground ,color-red))))
   `(sml/read-only ((t (:foreground ,color-green))))
   `(sml/filename ((t (:foreground ,color-blue :weight normal))))
   `(sml/prefix ((t (:foreground ,color-purple :weight normal :slant italic))))
   `(sml/modes ((t (:foreground ,color-fg :weight normal))))
   `(sml/modified ((t (:foreground ,color-red))))
   `(sml/outside-modified ((t (:foreground ,color-bg :background ,color-red))))
   `(sml/position-percentage ((t (:foreground ,color-purple :slant normal))))

   ;; Helm
   `(helm-candidate-number ((t (:foreground ,color-fg :background nil))))
   `(helm-source-header ((t (:foreground ,color-bg :background ,color-blue
                                         :weight normal :slant italic))))
   `(helm-selection ((t (:inherit region :distant-foreground nil :background nil))))
   `(helm-prefarg ((t (:foreground ,color-red))))
   `(helm-ff-directory ((t (:foreground ,color-blue))))
   `(helm-ff-executable ((t (:foreground ,color-green))))
   `(helm-ff-invalid-symlink ((t (:foreground ,color-bg
                                  :background ,color-dark-yellow))))
   `(helm-ff-symlink ((t (:foreground ,color-purple))))
   `(helm-ff-prefix ((t (:background ,color-yellow))))
   `(helm-ff-dotted-directory ((t (:background nil :foreground ,color-dell))))
   `(helm-M-x-key ((t (:foreground ,color-green))))
   `(helm-buffer-file ((t (:foreground ,color-fg))))
   `(helm-buffer-archive ((t (:inherit helm-buffer-file))))
   `(helm-buffer-directory ((t (:foreground ,color-blue :background nil))))
   `(helm-buffer-not-saved ((t (:foreground ,color-red))))
   `(helm-buffer-modified ((t (:foreground ,color-red))))
   `(helm-buffer-process ((t (:foreground ,color-green))))
   `(helm-buffer-size ((t (:foreground ,color-dell))))
   `(helm-ff-file ((t (:inherit default))))

   ;; TeX
   `(font-latex-sedate-face ((t (:foreground ,color-blue))))
   `(font-latex-math-face ((t (:foreground ,color-cyan))))
   `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

   ;; adoc-mode
   `(markup-meta-hide-face ((t (:height 1.0 :foreground ,color-fg))))
   `(markup-meta-face ((t (:height 1.0 :foreground ,color-fg :family nil))))
   `(markup-reference-face ((t (:underline nil :foreground ,color-blue))))
   `(markup-gen-face ((t (:foreground ,color-blue))))
   `(markup-passthrough-face ((t (:inherit markup-gen-face))))
   `(markup-replacement-face ((t (:family nil :foreground ,color-green))))
   `(markup-list-face ((t (:weight bold))))
   `(markup-secondary-text-face ((t (:height 1.0 :foreground ,color-green))))
   `(markup-verbatim-face ((t (:foreground ,color-bad))))
   `(markup-typewriter-face ((t (:inherit nil))))
   `(markup-title-0-face ((t (:height 1.2 :inherit markup-gen-face))))
   `(markup-title-1-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-2-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-3-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-4-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-5-face ((t (:height 1.0 :inherit markup-gen-face))))

   ;; Outline
   `(outline-1 ((t (:foreground ,color-purple))))
   `(outline-2 ((t (:foreground ,color-blue))))
   `(outline-3 ((t (:foreground ,color-green))))
   `(outline-4 ((t (:foreground ,color-cyan))))
   `(outline-5 ((t (:foreground ,color-yellow))))
   `(outline-6 ((t (:foreground ,color-fg))))
   `(outline-7 ((t (:foreground ,color-fg :slant italic))))
   `(outline-8 ((t (:foreground ,color-bad))))

   ;; Org-mode
   `(org-hide ((t (:foreground ,color-bg))))
   `(org-table ((t (:foreground ,color-fg))))
   `(org-date ((t (:foreground ,color-dark-green))))
   `(org-done ((t (:weight normal :foreground ,color-bad))))
   `(org-todo ((t (:weight normal :foreground ,color-yellow))))
   `(org-latex-and-related ((t (:foreground ,color-middle :italic t))))
   `(org-checkbox ((t (:weight normal :foreground ,color-bad))))
   `(org-verbatim ((t (:foreground ,color-middle))))
   `(org-mode-line-clock ((t (:background nil))))
   `(org-document-title ((t (:weight normal :foreground nil))))

   ;; Message
   `(message-header-name ((t (:foreground ,color-bad))))
   `(message-header-other ((t (:foreground ,color-fg))))
   `(message-header-cc ((t (:inherit message-header-other))))
   `(message-header-newsgroups ((t (:inherit message-header-other))))
   `(message-header-xheader ((t (:inherit message-header-other))))
   `(message-header-subject ((t (:foreground ,color-green))))
   `(message-header-to ((t (:foreground ,color-blue))))
   `(message-cited-text ((t (:foreground ,color-yellow))))
   `(message-mml ((t (:foreground ,color-buddha))))

   ;; Notmuch
   `(notmuch-search-unread-face ((t (:foreground ,color-blue))))
   `(notmuch-tag-face ((t (:foreground ,color-green))))
   `(notmuch-tree-match-author-face ((t (:foreground ,color-blue))))
   `(notmuch-tree-no-match-face ((t (:foreground ,color-bad))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
   `(notmuch-tag-unread-face ((t (:foreground ,color-red))))
   `(notmuch-message-summary-face ((t (:foreground ,color-middle))))

   ;; Compilation
   `(compilation-error ((t (:foreground ,color-red))))
   `(compilation-info ((t (:foreground ,color-green))))
   `(compilation-warning ((t (:foreground ,color-yellow))))
   ))

(provide-theme 'lab-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lab-dark-theme.el ends here
