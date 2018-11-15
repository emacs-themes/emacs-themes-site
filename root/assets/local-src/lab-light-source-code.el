;;; lab-light-theme.el --- A custom theme carefully constructed in the LAB space

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
;; variants. This file provides light variant.

;;; Code:

;; Note: for every face that is customized here, a customization for
;; it should be also provided in the dark version. Otherwise it could
;; be ugly when switching bwteen styles

(deftheme lab-light
  "A custom theme carefully constructed in the LAB space (light variant)")

;; Colors
(let*
    (
     ;; Angle 1.588 Saturation 0.42 lightness 70

     (color-blue "#69AFFF")
     (color-cyan "#00C5E0")
     (color-green "#30C36E")
     (color-yellow "#C0A946")
     (color-red "#FF7F79")
     (color-purple "#F083E7")

     ;; lightness 50 saturation 0.45
     (color-dark-blue "#007BD9")
     (color-dark-cyan "#008FAC")
     (color-dark-green "#008C36")
     (color-dark-yellow "#897500")
     (color-dark-red "#D04245")
     (color-dark-purple "#BA48B4")

     ;; Saturation 0.01
     (color-overexpo "#EEF0F3")         ; 95
     ;; Saturation 0.03
     (color-ultralight "#DEE2EA")       ; 90
     (color-mist "#D0D4DC")             ; 85
     (color-light "#C2C6CE")            ; 80
     (color-bad "#A7ABB2")              ; 70
     (color-middle "#73777D")           ; 50
     (color-dell "#5A5E65")             ; 40

     (color-fg color-dell)
     (color-bg color-overexpo))

  (custom-theme-set-faces
   'lab-light
   `(default ((t (:background ,color-bg
                  :foreground ,color-fg))))
   `(cursor ((t (:background ,color-red
                 :foreground ,color-fg))))
   `(region ((t (:background ,color-green
                 :foreground ,color-bg))))
   `(mode-line ((t (:background ,color-mist
                    :foreground ,color-fg
                    :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,color-fg))))
   `(mode-line-inactive ((t (:background ,color-ultralight
                             :foreground ,color-fg))))
   `(fringe ((t (:background ,color-bg))))
   `(minibuffer-prompt ((t (:slant italic :foreground ,color-middle))))
   `(font-lock-builtin-face ((t (:foreground ,color-middle))))
   `(font-lock-comment-face ((t (:slant italic :foreground ,color-bad))))
   `(font-lock-constant-face ((t (:slant italic :foreground ,color-middle))))
   `(font-lock-function-name-face ((t (:foreground ,color-dark-purple))))
   `(font-lock-keyword-face ((t (:foreground ,color-middle :slant italic))))
   `(font-lock-string-face ((t (:foreground ,color-dark-cyan))))
   `(font-lock-type-face ((t (:foreground ,color-dark-green))))
   `(font-lock-variable-name-face ((t (:foreground ,color-dark-blue))))
   `(font-lock-warning-face ((t (:foreground ,color-dark-red))))

   `(isearch ((t (:background ,color-middle
                  :foreground ,color-bg))))
   `(lazy-highlight ((t (:background ,color-light))))
   `(link ((t (:foreground ,color-dark-blue :underline t))))
   `(link-visited ((t (:foreground ,color-middle :underline t))))
   `(button ((t (:background ,color-yellow :underline t :foreground nil))))
   `(header-line ((t (:background ,color-ultralight
                      :foreground ,color-fg))))
   `(shadow ((t (:foreground ,color-light))))
   `(show-paren-match ((t (:background ,color-green :foreground ,color-bg))))
   `(show-paren-mismatch ((t (:background ,color-red
                              :foreground ,color-bg))))
   `(highlight ((t (:inverse-video nil :background ,color-ultralight))))
   `(hl-line ((t (:inverse-video nil :background ,color-ultralight))))

   ;; Faces for specific prog modes
   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))

   ;; Dired
   `(dired-directory ((t (:foreground ,color-dark-blue))))
   `(dired-symlink ((t (:foreground ,color-dark-cyan))))
   `(dired-perm-write ((t (:foreground ,color-dark-red))))

   ;; Diff
   `(diff-added ((t (:foreground ,color-dark-blue))))
   `(diff-removed ((t (:foreground ,color-dark-red))))
   ;; `(diff-context ((t (:background nil))))
   `(diff-file-header ((t (:bold t :background ,color-light :weight bold))))
   `(diff-header ((t (:background ,color-ultralight :foreground ,color-fg))))

   ;; Whitespace
   `(whitespace-trailing ((t (:background ,color-light))))
   `(whitespace-line ((t (:background ,color-light :foreground unspecified))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,color-dark-blue
                          :weight unspecified))))
   `(erc-header-line ((t (:foreground ,color-bg :background ,color-mist))))
   `(erc-timestamp-face ((t (:foreground ,color-middle
                             :weight unspecified))))
   `(erc-current-nick-face ((t (:foreground ,color-dark-green
                                :weight unspecified))))
   `(erc-input-face ((t (:foreground ,color-dark-yellow))))
   `(erc-prompt-face ((t (:foreground ,color-middle
                          :background nil
                          :slant italic
                          :weight unspecified))))
   `(erc-my-nick-face ((t (:foreground ,color-dark-yellow))))
   `(erc-pal-face ((t (:foreground ,color-dark-cyan))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,color-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,color-dark-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,color-cyan))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,color-dark-red))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,color-dark-green))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,color-dark-purple))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,color-middle))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,color-red))))

   ;; Magit
   `(magit-branch-local ((t (:foreground ,color-dark-blue :background nil))))
   `(magit-branch-remote ((t (:foreground ,color-dark-green :background nil))))
   `(magit-tag ((t (:foreground ,color-blue :background ,color-bg))))
   `(magit-hash ((t (:foreground ,color-bad))))
   `(magit-section-title ((t (:foreground ,color-dark-green :background ,color-bg))))
   `(magit-section-heading ((t (:background ,color-bg :foreground ,color-fg))))
   `(magit-section-highlight ((t (:background ,color-bg))))
   `(magit-item-highlight ((t (:foreground ,color-fg :background ,color-ultralight))))
   `(magit-log-author ((t (:foreground ,color-dark-purple))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
   `(magit-diff-context ((t (:inherit diff-context))))
   `(magit-diff-context-highlight ((t (:inherit magit-diff-context))))
   `(magit-popup-argument ((t (:inherit font-lock-function-name-face))))
   `(magit-popup-disabled-argument ((t (:inherit font-lock-comment-face))))


   ;; Git-gutter-fringe
   `(git-gutter-fr:modified ((t (:foreground ,color-dark-purple))))
   `(git-gutter-fr:added ((t (:foreground ,color-dark-green))))
   `(git-gutter-fr:deleted ((t (:foreground ,color-dark-red))))

   ;; Company
   `(company-preview ((t (:foreground ,color-fg :background ,color-red))))
   `(company-preview-common ((t (:foreground ,color-fg :background ,color-dark-green))))
   `(company-tooltip ((t (:foreground ,color-fg :background ,color-ultralight))))
   `(company-tooltip-common ((t (:foreground ,color-dark-green))))
   `(company-tooltip-selection ((t (:background ,color-mist))))
   `(company-tooltip-common-selection ((t (:foreground ,color-dark-green))))
   `(company-tooltip-annotation ((t (:foreground ,color-dark-blue))))
   `(company-scrollbar-bg ((t (:background ,color-ultralight))))
   `(company-scrollbar-fg ((t (:background ,color-mist))))

   ;; Powerline
   `(powerline-active2 ((t (:foreground ,color-fg :background ,color-bg))))
   `(powerline-active1 ((t (:foreground ,color-bg :background ,color-dark-green))))
   `(powerline-inactive2 ((t (:foreground ,color-bg :background ,color-mist))))
   `(powerline-inactive1 ((t (:foreground ,color-fg :background ,color-middle))))

   ;; Smart mode line
   `(sml/global  ((t (:foreground ,color-fg))))
   `(sml/charging ((t (:foreground ,color-dark-green))))
   `(sml/discharging ((t (:foreground ,color-dark-red))))
   `(sml/read-only ((t (:foreground ,color-dark-green))))
   `(sml/filename ((t (:foreground ,color-dark-blue :weight bold))))
   `(sml/prefix ((t (:foreground ,color-dark-purple :weight normal :slant italic))))
   `(sml/modes ((t (:foreground ,color-fg :weight bold))))
   `(sml/modified ((t (:foreground ,color-dark-red))))
   `(sml/outside-modified ((t (:foreground ,color-bg :background ,color-dark-red))))
   `(sml/position-percentage ((t (:foreground ,color-dark-purple :slant normal))))

   ;; Helm
   `(helm-candidate-number ((t (:foreground ,color-fg :background nil))))
   `(helm-source-header ((t (:foreground ,color-bg :background ,color-blue
                                         :weight normal :slant italic))))
   `(helm-selection ((t (:inherit region :distant-foreground nil :background nil))))
   `(helm-prefarg ((t (:foreground ,color-dark-red))))
   `(helm-ff-directory ((t (:foreground ,color-dark-blue))))
   `(helm-ff-executable ((t (:foreground ,color-dark-green))))
   `(helm-ff-invalid-symlink ((t (:foreground ,color-bg
                                  :background ,color-dark-yellow))))
   `(helm-ff-symlink ((t (:foreground ,color-dark-purple))))
   `(helm-ff-prefix ((t (:background ,color-yellow))))
   `(helm-ff-dotted-directory ((t (:background nil :foreground ,color-light))))
   `(helm-M-x-key ((t (:foreground ,color-dark-green))))
   `(helm-buffer-file ((t (:foreground ,color-fg))))
   `(helm-buffer-archive ((t (:inherit helm-buffer-file))))
   `(helm-buffer-directory ((t (:foreground ,color-dark-blue :background nil))))
   `(helm-buffer-not-saved ((t (:foreground ,color-dark-red))))
   `(helm-buffer-modified ((t (:foreground ,color-red))))
   `(helm-buffer-process ((t (:foreground ,color-dark-green))))
   `(helm-buffer-size ((t (:foreground ,color-light))))
   `(helm-ff-file ((t (:inherit default))))

   ;; TeX
   `(font-latex-sedate-face ((t (:foreground ,color-dark-blue))))
   `(font-latex-math-face ((t (:foreground ,color-dark-cyan))))
   `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

   ;; adoc-mode
   `(markup-meta-hide-face ((t (:height 1.0 :foreground ,color-fg))))
   `(markup-meta-face ((t (:height 1.0 :foreground ,color-fg :family nil))))
   `(markup-reference-face ((t (:underline nil :foreground ,color-dark-blue))))
   `(markup-gen-face ((t (:foreground ,color-dark-blue))))
   `(markup-passthrough-face ((t (:inherit markup-gen-face))))
   `(markup-replacement-face ((t (:family nil :foreground ,color-green))))
   `(markup-list-face ((t (:weight bold))))
   `(markup-secondary-text-face ((t (:height 1.0 :foreground ,color-dark-green))))
   `(markup-verbatim-face ((t (:foreground ,color-middle))))
   `(markup-typewriter-face ((t (:inherit nil))))
   `(markup-title-0-face ((t (:height 1.2 :inherit markup-gen-face))))
   `(markup-title-1-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-2-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-3-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-4-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-5-face ((t (:height 1.0 :inherit markup-gen-face))))

   ;; Outline
   `(outline-1 ((t (:foreground ,color-dark-purple))))
   `(outline-2 ((t (:foreground ,color-dark-blue))))
   `(outline-3 ((t (:foreground ,color-dark-green))))
   `(outline-4 ((t (:foreground ,color-dark-cyan))))
   `(outline-5 ((t (:foreground ,color-dark-yellow))))
   `(outline-6 ((t (:foreground ,color-fg))))
   `(outline-7 ((t (:foreground ,color-fg :slant italic))))
   `(outline-8 ((t (:foreground ,color-middle))))

   ;; Org-mode
   `(org-hide ((t (:foreground ,color-bg))))
   `(org-table ((t (:foreground ,color-fg))))
   `(org-date ((t (:foreground ,color-green))))
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
   `(message-header-subject ((t (:foreground ,color-dark-green))))
   `(message-header-to ((t (:foreground ,color-dark-blue))))
   `(message-cited-text ((t (:foreground ,color-dark-yellow))))
   `(message-mml ((t (:foreground ,color-light))))

   ;; Notmuch
   `(notmuch-search-unread-face ((t (:foreground ,color-dark-blue))))
   `(notmuch-tag-face ((t (:foreground ,color-dark-green))))
   `(notmuch-tree-match-author-face ((t (:foreground ,color-dark-blue))))
   `(notmuch-tree-no-match-face ((t (:foreground ,color-bad))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
   `(notmuch-tag-unread-face ((t (:foreground ,color-red))))
   `(notmuch-message-summary-face ((t (:foreground ,color-middle))))

   ;; Compilation
   `(compilation-error ((t (:foreground ,color-dark-red))))
   `(compilation-info ((t (:foreground ,color-dark-green))))
   `(compilation-warning ((t (:foreground ,color-dark-yellow))))
   ))

(provide-theme 'lab-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lab-light-theme.el ends here
