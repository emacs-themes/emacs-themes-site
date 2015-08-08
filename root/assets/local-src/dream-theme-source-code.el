;; dream-theme -- dark, clean theme for emacs. inspired by the zenburn, sinburn
;; and similar themes

;; Copyright (C) 2013 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema &lt;djcb@djcbsoftware.nl&gt;
;; Created: 2013-01-27

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; &lt;http://www.gnu.org/licenses/&gt;.

;;; Code

(deftheme dream "The Dream Theme.")

;; my palette
(let ( (dt-fg		"#dcdccc")
       (dt-bg-3         "#777777")
       (dt-bg-2         "#232923")
       (dt-bg-1		"#2f2f2f")
       (dt-bg		"#151515")
       (dt-bg+1		"#202020")
       (dt-bg+2		"#303030")
       (dt-bg+3		"#101010")
       (dt-red+1	"#dca3a3")
       (dt-red		"#cc9393")
       (dt-red-1	"#bc8383")
       (dt-red-2	"#ac7373")
       (dt-red-3	"#9c6363")
       (dt-red-4	"#8c5353")
       (dt-orange	"#dfaf8f")
       (dt-yellow	"#f0dfaf")
       (dt-yellow-1	"#e0cf9f")
       (dt-yellow-2	"#d0bf8f")
       (dt-yellow-3     "#72715e")
       (dt-green-4      "#2e3330")
       (dt-green-1	"#5f7f5f")
       (dt-green	"#7f9f7f")
       (dt-green+1	"#8fb28f")
       (dt-green+2	"#9fc59f")
       (dt-green+3	"#afd8af")
       (dt-green+4	"#bfebbf")
       (dt-cyan		"#93e0e3")
       (dt-blue+1	"#94bff3")
       (dt-blue		"#8cd0d3")
       (dt-blue-1	"#7cb8bb")
       (dt-blue-2	"#6ca0a3")
       (dt-blue-3	"#5c888b")
       (dt-blue-4	"#4c7073")
       (dt-blue-5	"#366060")
       (dt-magenta	"#dc8cc3"))

  (custom-theme-set-faces
    'dream
    ;; basics
    `(default ((t (:background ,dt-bg :foreground ,dt-fg))))
    '(fixed-pitch ((t (:weight bold))))
    '(italic ((t (:slant italic))))
    '(underline ((t (:underline t))))
    `(fringe ((t (:background ,dt-bg-1))))
    `(header-line ((t (:box (:color ,dt-bg-1 :line-width 2)
			:weight bold :foreground ,dt-blue-1
			:background ,dt-green-4))))
    `(highlight ((t (:weight bold :underline t :background nil
		      :foreground ,dt-orange))))
    `(hover-highlight ((t (:underline t :foreground nil :background ,dt-green))))
    '(match ((t (:weight bold))))
    `(menu ((t (:background ,dt-bg+3))))
    `(mode-line ((t (:foreground nil :background ,dt-green-4
		      :box (:color ,dt-bg-1 :line-width 2)))))
    `(mode-line-inactive ((t  (:inherit mode-line :background ,dt-green-1))))
    '(mouse ((t (:inherit dt-foreground))))
    '(paren ((t (:inherit dt-lowlight-1))))
    '(trailing-whitespace ((t (:inherit font-lock-warning))))
    `(link ((t (:foreground ,dt-blue-3 :background nil :underline t))))
    `(border ((t (:background ,dt-bg))))
    `(button ((t (:foreground ,dt-yellow :background ,dt-blue-5
		   :weight bold :underline t))))
    `(cursor ((t (:background ,dt-yellow :foreground nil))))
    `(minibuffer-prompt ((t (:foreground ,dt-red :weight bold :background ,dt-bg-1))))
    `(region ((t (:foreground nil :background ,dt-yellow-3))))
    `(scroll-bar ((t (:background ,dt-bg+2))))
    `(secondary-selection ((t (:foreground nil :background ,dt-bg+2))))
    `(tool-bar ((t (:background ,dt-bg+2))))

    ;; cua's rectangle mode
    `(cua-rectangle ((t (:background ,dt-blue-5))))
    
    ;; erc
    `(erc-action-face ((t (:foreground ,dt-red-1 :slant italic))))
    '(erc-bold-face ((t (:weight bold))))
    `(erc-button ((t (:foreground ,dt-blue :background nil :underline t))))
    `(erc-current-nick-face ((t (:foreground ,dt-yellow))))
    '(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
    '(erc-direct-msg-face ((t (:inherit erc-default))))
    '(erc-error-face ((t (:inherit font-lock-warning))))
    `(erc-fool-face ((t (:foreground ,dt-red))))
    '(erc-highlight-face ((t (:inherit highlight))))
    `(erc-keyword-face ((t (:foreground ,dt-red :weight bold))))
    `(erc-my-nick-face ((t (:foreground ,dt-red :weight bold))))
    `(erc-nick-default-face ((t (:foreground ,dt-blue))))
    `(erc-nick-msg-face ((t (:foreground ,dt-yellow-1))))
    `(erc-notice-face ((t (:foreground ,dt-green))))
    `(erc-pal-face ((t (:foreground ,dt-blue))))
    `(erc-prompt-face ((t (:weight bold :foreground ,dt-yellow))))
    `(erc-timestamp-face ((t (:foreground ,dt-green+1))))
    `(erc-input-face ((t (:foreground ,dt-yellow))))
    
    ;; flyspell
    ;; wavy underlines, emacs 24.3+
    `(flyspell-incorrect ((t (:foreground ,dt-yellow :underline (:color ,dt-red :style wave)))))

    ;; font-locking (ie., syntax highlighting)
    `(font-lock-builtin-face ((t (:foreground ,dt-blue))))
    `(font-lock-comment-face ((t (:foreground ,dt-bg-3 :slant italic))))
    `(font-lock-comment-delimiter-face ((t (:foreground ,dt-bg-3))))
    `(font-lock-constant-face ((t (:foreground ,dt-green+4 :weight bold))))
    `(font-lock-doc-face ((t (:foreground ,dt-bg-3 :slant italic))))
    `(font-lock-doc-string-face ((t (:foreground ,dt-bg-3 :slant italic))))
    `(font-lock-function-name-face ((t (:foreground ,dt-green+1 :weight bold))))
    `(font-lock-keyword-face ((t (:foreground ,dt-red-1 :weight bold))))
    `(font-lock-negation-char-face ((t (:foreground ,dt-green-1))))
    `(font-lock-preprocessor-face ((t (:foreground ,dt-red+1))))
    `(font-lock-string-face ((t (:foreground ,dt-blue-2))))
    `(font-lock-type-face ((t (:foreground ,dt-blue+1))))
    `(font-lock-variable-name-face ((t (:foreground ,dt-yellow))))
    `(font-lock-warning-face ((t (:foreground ,dt-yellow))))
    `(font-lock-pseudo-keyword-face ((t (:foreground ,dt-red-2 :weight bold))))
    `(font-lock-operator-face ((t (:foreground ,dt-blue-1))))

    ;; hlline
    `(hl-line ((t (:foreground nil :background ,dt-bg-2))))

    ;; info
    `(info-xref ((t (:foreground ,dt-yellow :weight bold))))
    '(info-xref-visited ((t (:inherit info-xref :weight normal))))
    '(info-header-xref ((t (:inherit info-xref))))
    `(info-menu-star ((t (:foreground ,dt-orange :weight normal))))
    `(info-menu-5 ((t (:inherit info-menu-star))))
    `(info-title-1 ((t (:foreground ,dt-blue :background nil
			 :weight bold :height 1.2))))
    `(info-title-2 ((t (:foreground ,dt-blue :background nil
			 :weight bold :height 1.1))))
    `(info-menu-header ((t (:weight normal :foreground ,dt-blue :height 1.2))))
    '(info-node ((t (:weight bold))))
    '(info-header-node ((t (:weight normal))))

    ;; magit
    `(magit-section-title ((t (:foreground ,dt-red :height 1.1 :weight normal))))
    `(magit-item-highlight ((t (:foreground ,dt-blue
				 :background ,dt-bg-1 :underline t))))
    `(magit-branch ((t (:foreground ,dt-green :background ,dt-bg-1 :box nil))))

    ;; message-mode
    `(message-cited-text-face ((t (:inherit font-lock-comment))))
    `(message-header-name-face ((t (:foregrond ,dt-blue :weight bold))))
    `(message-header-name ((t (:foregrond ,dt-blue :weight bold))))
    `(message-header-key-face ((t (:foregrond ,dt-green+3 :weight bold))))
    `(message-header-other-face ((t (:foreground ,dt-green))))
    `(message-header-to-face ((t (:foreground ,dt-green+1))))
    `(message-header-from-face ((t (:foreground ,dt-green+2))))
    `(message-header-cc-face ((t (:foreground ,dt-green+3))))
    `(message-header-newsgroups-face ((t (:foreground ,dt-blue))))
    `(message-header-subject-face ((t (:foreground ,dt-red))))
    `(message-header-xheader-face ((t (:foreground ,dt-green))))
    `(message-mml-face ((t (:foreground ,dt-blue-1))))
    `(message-separator-face ((t (:foreground ,dt-green :background ,dt-bg+3))))

    ;; linum
    `(linum ((t (:foreground ,dt-yellow-3 :background ,dt-bg
		  :height .8 :weight normal :slant normal :underline nil))))

    ;; one-key
    `(one-key-name ((t (:foreground ,dt-yellow))))
    `(one-key-keystroke ((t (:foreground ,dt-red))))
    `(one-key-prompt ((t (:foreground ,dt-green))))


    ;; rainbow-delimiters
    `(rainbow-delimiters-depth-1-face ((t (:foreground ,dt-cyan))))
    `(rainbow-delimiters-depth-2-face ((t (:foreground ,dt-yellow))))
    `(rainbow-delimiters-depth-3-face ((t (:foreground ,dt-blue+1))))
    `(rainbow-delimiters-depth-4-face ((t (:foreground ,dt-red+1))))
    `(rainbow-delimiters-depth-5-face ((t (:foreground ,dt-green+1))))
    `(rainbow-delimiters-depth-6-face ((t (:foreground ,dt-blue-1))))
    `(rainbow-delimiters-depth-7-face ((t (:foreground ,dt-orange))))
    `(rainbow-delimiters-depth-8-face ((t (:foreground ,dt-magenta))))
    `(rainbow-delimiters-depth-9-face ((t (:foreground ,dt-yellow-2))))
    `(rainbow-delimiters-depth-10-face ((t (:foreground ,dt-green+2))))
    `(rainbow-delimiters-depth-11-face ((t (:foreground ,dt-blue+1))))
    `(rainbow-delimiters-depth-12-face ((t (:foreground ,dt-red-4))))

    ;; show-paren
    `(show-paren-mismatch ((t (:foreground ,dt-cyan :background ,dt-green
				:weight bold :underline t))))
    `(show-paren-match ((t (:foreground ,nil :background ,dt-bg-1
			     :underline t :weight normal))))

    ;; which-function mode
    `(which-func ((t (:foreground ,dt-yellow))))
    
    ))

(provide-theme 'dream)
