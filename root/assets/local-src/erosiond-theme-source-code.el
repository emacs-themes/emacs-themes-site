;;; erosiond-theme.el --- A color theme for Emacs 24.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see &lt;http://www.gnu.org/licenses/&gt;.

;;; Installation:
;;
;; Drop the theme in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;

;;; Code

(deftheme erosiond
  "A dark, sepia-ish theme")

(let ((class '((class color) (min-colors 89)))
      ;; color definitions
      ;; colors with +x are lighter, colors with -x are darker
      (erosion-fg   "#bea492")
      (erosion-bg-1 "#12100f")
      (erosion-bg   "#181512")
      (erosion-bg+1 "#22201a")
      (erosion-bg+2 "#5f5f5f")

      (erosion-red+1 "#9f7155")
      (erosion-red   "#8c644c")
      (erosion-red-1 "#7a5840")
      (erosion-red-2 "#714b3c")
      (erosion-red-3 "#684032")

      (erosion-orange "#dfaf8f")

      (erosion-yellow   "#fafac0")
      (erosion-yellow-1 "#bfba92")
      (erosion-yellow-2 "#afaa82")

      (erosion-green-1 "#b2af87")
      (erosion-green+1 "#bec17e")
      (erosion-green   "#c4be90")
      (erosion-green+2 "#cfc498")
      (erosion-green+3 "#d4d0a3")
      (erosion-green+4 "#dbd3af")

      (erosion-cyan "#3b484a")

      (erosion-blue+1 "#626e74")
      (erosion-blue   "#646a6d")
      (erosion-blue-1 "#606266")
      (erosion-blue-2 "#5e5961")
      (erosion-blue-3 "#545a5f")
      (erosion-blue-4 "#515359")
      (erosion-blue-5 "#535058")

      (erosion-magenta "#6d6871")

      (erosion-white "#f0f0f0"))

  (custom-theme-set-faces
   'erosiond

   ;;; basic coloring
   `(cursor ((,class (:foreground ,erosion-fg))))
   `(default ((,class (:foreground ,erosion-white :background ,erosion-bg))))
   `(fringe ((,class (:foreground ,erosion-cyan :background ,erosion-bg-1))))
   `(escape-glyph-face ((,class (:foreground ,erosion-red))))
   `(header-line ((,class (:foreground ,erosion-yellow
                                       :background ,erosion-bg-1
                                       :box (:line-width 1 :color ,erosion-bg-1)))))
   `(hl-line ((,class (:background ,erosion-bg+1))))
   `(linum ((,class (:foreground "#505050" :background ,erosion-bg-1))))
   `(minibuffer-prompt ((,class (:foreground ,erosion-yellow))))
   `(menu ((,class (:foreground ,erosion-fg :background ,erosion-bg))))
   `(mode-line
     ((,class (:foreground ,erosion-fg :background ,erosion-bg+1
                           :box (:line-width 1 :color ,erosion-bg+1)))))
   `(mode-line-inactive ((,class (:inherit mode-line :background ,erosion-bg+2
                                           :box (:line-width 1 :color ,erosion-bg-1)))))
   `(mode-line-buffer-id ((,class (:inherit 'erosion-yellow :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,erosion-green-1 :background ,erosion-bg+1))))
   `(mode-line-buffer-name ((,class (:foreground ,erosion-green :weight bold))))
   `(mode-line-mode-face ((,class (:foreground ,erosion-yellow))))
   `(mode-line-folder-face ((,class (:foreground ,erosion-bg+2))))
   `(mode-line-modified-face ((,class (:foreground ,erosion-orange :background ,erosion-bg+1))))
   `(mode-line-read-only-face ((,class (:foreground ,erosion-red))))
   `(region ((,class (:background ,erosion-bg+2))))
   `(secondary-selection ((,class (:background ,erosion-bg+2))))
   `(trailing-whitespace ((,class (:background ,erosion-red))))
   `(vertical-border ((,class (:foreground ,erosion-fg))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,erosion-yellow :background ,erosion-bg-1))))
   `(isearch-fail ((,class (:foreground ,erosion-fg :background ,erosion-red-3))))
   `(lazy-highlight ((,class (:foreground ,erosion-yellow :background ,erosion-bg+2))))

   ;; font-lock
   `(font-lock-builtin-face ((,class (:foreground ,erosion-blue))))
   `(font-lock-comment-face ((,class (:foreground ,erosion-bg+2))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,erosion-bg+2))))
   `(font-lock-constant-face ((,class (:foreground ,erosion-fg))))
   `(font-lock-doc-face ((,class (:foreground ,erosion-green+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,erosion-blue+1))))
   `(font-lock-function-name-face ((,class (:foreground ,erosion-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,erosion-yellow :weight bold))))
   `(font-lock-negation-char-face ((,class (:foregorund ,erosion-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,erosion-blue))))
   `(font-lock-string-face ((,class (:foreground ,erosion-red))))
   `(font-lock-type-face ((,class (:foregorund ,erosion-yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,erosion-yellow))))
   `(font-lock-warning-face ((,class (:foreground ,erosion-yellow-1 :weight bold :underline t))))
   `(font-lock-pseudo-keyword-face ((,class (:foreground ,erosion-yellow))))
   `(font-lock-operator-face ((,class (:foreground ,erosion-orange))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;;; external

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,erosion-bg+2 :foreground "white"))))
   `(ac-selection-face ((,class (:background ,erosion-blue-4 :foreground ,erosion-fg))))
   `(popup-tip-face ((,class (:background ,erosion-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((,class (:background "white"))))
   `(popup-scroll-bar-background-face ((,class (:background ,erosion-bg-1))))
   `(popup-isearch-match ((,class (:background ,erosion-bg :foreground ,erosion-fg))))

   ;; diff
   `(diff-added ((,class (:foreground ,erosion-green+4))))
   `(diff-changed ((,class (:foreground ,erosion-yellow))))
   `(diff-removed ((,class (:foreground ,erosion-red))))
   `(diff-header ((,class (:background ,erosion-bg+1))))
   `(diff-file-header
     ((,class (:background ,erosion-bg+2 :foreground ,erosion-fg :bold t))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,erosion-yellow :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,erosion-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,erosion-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,erosion-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,erosion-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:inherit erosion-yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,erosion-cyan :weight bold))))

   ;; ecb
   `(ecb-analyse-bucket-element-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-analyse-bucket-node-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-analyse-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-analyse-general-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-bucket-node-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-default-general-face ((,class (:inherit erosion-fg))))
   `(ecb-default-highlight-face ((,class (:foreground ,erosion-green :background ,erosion-bg :weight bold))))
   `(ecb-directories-general-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-directory-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-directory-not-accessible-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-history-bucket-node-dir-soure-path-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-history-bucket-node-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-history-dead-buffer-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-history-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-history-general-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-history-indirect-buffer-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-method-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-method-non-semantic-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-methods-general-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-mode-line-data-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-mode-line-prefix-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-mode-line-win-nr-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-source-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-source-in-directories-buffer-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-source-read-only-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-sources-general-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-symboldef-prototype-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-symboldef-symbol-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-tag-header-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-tree-guide-line-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-type-tag-class-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-type-tag-enum-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-type-tag-group-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-type-tag-interface-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-type-tag-struct-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(ecb-type-tag-typedef-face ((,class (:foreground ,erosion-cyan :weight bold))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,erosion-yellow :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,erosion-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,erosion-yellow))))
   `(erc-keyword-face ((,class (:foreground ,erosion-yellow :weight bold))))
   `(erc-nick-default-face ((,class (:weight bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,erosion-green))))
   `(erc-pal-face ((,class (:foreground ,erosion-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,erosion-orange :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,erosion-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; evil
   `(evil-state-face ((,class (:foreground ,erosion-bg-1 :background ,erosion-green :weight bold :box (:line-width 4 :color ,erosion-green)))))
   ;; flymake
   `(flymake-errline ((,class (:foreground ,erosion-red-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,erosion-yellow-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,erosion-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,erosion-red-1 :weight bold :underline t))))

   ;; gnus
   `(gnus-group-mail-1-face ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty-face ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2-face ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty-face ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3-face ((,class (:bold t :inherit 'nus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty-face ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4-face ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty-face ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5-face ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty-face ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6-face ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty-face ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low-face ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty-face ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1-face ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2-face ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3-face ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4-face ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5-face ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6-face ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low-face ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content-face ((,class (:inherit message-header-other))))
   `(gnus-header-from-face ((,class (:inherit message-header-from))))
   `(gnus-header-name-face ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups-face ((,class (:inherit message-header-other))))
   `(gnus-header-subject-face ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled-face ((,class (:foreground ,erosion-orange))))
   `(gnus-summary-high-ancient-face ((,class (:foreground ,erosion-blue))))
   `(gnus-summary-high-read-face ((,class (:foreground ,erosion-green :weight bold))))
   `(gnus-summary-high-ticked-face ((,class (:foreground ,erosion-orange :weight bold))))
   `(gnus-summary-high-unread-face ((,class (:foreground ,erosion-fg :weight bold))))
   `(gnus-summary-low-ancient-face ((,class (:foreground ,erosion-blue))))
   `(gnus-summary-low-read-face ((,class (:foreground ,erosion-green))))
   `(gnus-summary-low-ticked-face ((,class (:foreground ,erosion-orange :weight bold))))
   `(gnus-summary-low-unread-face ((,class (:foreground ,erosion-fg))))
   `(gnus-summary-normal-ancient-face ((,class (:foreground ,erosion-blue))))
   `(gnus-summary-normal-read-face ((,class (:foreground ,erosion-green))))
   `(gnus-summary-normal-ticked-face ((,class (:foreground ,erosion-orange :weight bold))))
   `(gnus-summary-normal-unread-face ((,class (:foreground ,erosion-fg))))
   `(gnus-summary-selected-face ((,class (:foreground ,erosion-yellow :weight bold))))
   `(gnus-cite-1-face ((,class (:foreground ,erosion-blue))))
   `(gnus-cite-10-face ((,class (:foreground ,erosion-yellow-1))))
   `(gnus-cite-11-face ((,class (:foreground ,erosion-yellow))))
   `(gnus-cite-2-face ((,class (:foreground ,erosion-blue-1))))
   `(gnus-cite-3-face ((,class (:foreground ,erosion-blue-2))))
   `(gnus-cite-4-face ((,class (:foreground ,erosion-green+2))))
   `(gnus-cite-5-face ((,class (:foreground ,erosion-green+1))))
   `(gnus-cite-6-face ((,class (:foreground ,erosion-green))))
   `(gnus-cite-7-face ((,class (:foreground ,erosion-red))))
   `(gnus-cite-8-face ((,class (:foreground ,erosion-red-1))))
   `(gnus-cite-9-face ((,class (:foreground ,erosion-red-2))))
   `(gnus-group-news-1-empty-face ((,class (:foreground ,erosion-yellow))))
   `(gnus-group-news-2-empty-face ((,class (:foreground ,erosion-green+3))))
   `(gnus-group-news-3-empty-face ((,class (:foreground ,erosion-green+1))))
   `(gnus-group-news-4-empty-face ((,class (:foreground ,erosion-blue-2))))
   `(gnus-group-news-5-empty-face ((,class (:foreground ,erosion-blue-3))))
   `(gnus-group-news-6-empty-face ((,class (:foreground ,erosion-bg+2))))
   `(gnus-group-news-low-empty-face ((,class (:foreground ,erosion-bg+2))))
   `(gnus-signature-face ((,class (:foreground ,erosion-yellow))))
   `(gnus-x-face ((,class (:background ,erosion-fg :foreground ,erosion-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,erosion-yellow :weight bold))))
   `(magit-branch ((,class (:foreground ,erosion-orange :weight bold))))

   ;; message-mode
   `(message-cited-text-face ((,class (:inherit font-lock-comment))))
   `(message-header-name-face ((,class (:foreground ,erosion-green+1))))
   `(message-header-other-face ((,class (:foreground ,erosion-green))))
   `(message-header-to-face ((,class (:inherit 'erosion-yellow :weight bold))))
   `(message-header-from-face ((,class (:inherit 'erosion-yellow :weight bold))))
   `(message-header-cc-face ((,class (:inherit 'erosion-yellow :weight bold))))
   `(message-header-newsgroups-face ((,class (:inherit 'erosion-yellow :weight bold))))
   `(message-header-subject-face ((,class (:inherit 'erosion-orange :weight bold))))
   `(message-header-xheader-face ((,class (:foreground ,erosion-green))))
   `(message-mml-face ((,class (:foreground ,erosion-yellow :weight bold))))
   `(message-separator-face ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,erosion-orange))))
   `(mew-face-header-from ((,class (:foreground ,erosion-yellow))))
   `(mew-face-header-date ((,class (:foreground ,erosion-green))))
   `(mew-face-header-to ((,class (:foreground ,erosion-red))))
   `(mew-face-header-key ((,class (:foreground ,erosion-green))))
   `(mew-face-header-private ((,class (:foreground ,erosion-green))))
   `(mew-face-header-important ((,class (:foreground ,erosion-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,erosion-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,erosion-red))))
   `(mew-face-header-xmew ((,class (:foreground ,erosion-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,erosion-red))))
   `(mew-face-body-url ((,class (:foreground ,erosion-orange))))
   `(mew-face-body-comment ((,class (:foreground ,erosion-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,erosion-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,erosion-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,erosion-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,erosion-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,erosion-red))))
   `(mew-face-mark-review ((,class (:foreground ,erosion-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,erosion-green))))
   `(mew-face-mark-delete ((,class (:foreground ,erosion-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,erosion-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,erosion-green))))
   `(mew-face-mark-unread ((,class (:foreground ,erosion-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,erosion-green))))
   `(mew-face-eof-part ((,class (:foreground ,erosion-yellow))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,erosion-yellow))))
   `(nav-face-button-num ((,class (:foreground ,erosion-cyan))))
   `(nav-face-dir ((,class (:foreground ,erosion-green))))
   `(nav-face-hdir ((,class (:foreground ,erosion-red))))
   `(nav-face-file ((,class (:foreground ,erosion-fg))))
   `(nav-face-hfile ((,class (:foreground ,erosion-red-3))))

   ;; org-mode
   `(org-agenda-date-today
     ((,class (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,erosion-fg :weight bold))))
   `(org-checkbox ((,class (:background ,erosion-bg+2 :foreground "white"
                                        :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,erosion-blue :underline t))))
   `(org-deadline-announce ((,class (:foreground ,erosion-red-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,erosion-green+3))))
   `(org-formula ((,class (:foreground ,erosion-yellow-2))))
   `(org-headline-done ((,class (:foreground ,erosion-green+3))))
   `(org-hide ((,class (:foreground ,erosion-bg-1))))
   `(org-level-1 ((,class (:foreground ,erosion-orange))))
   `(org-level-2 ((,class (:foreground ,erosion-green+1))))
   `(org-level-3 ((,class (:foreground ,erosion-blue-1))))
   `(org-level-4 ((,class (:foreground ,erosion-yellow-2))))
   `(org-level-5 ((,class (:foreground ,erosion-cyan))))
   `(org-level-6 ((,class (:foreground ,erosion-green-1))))
   `(org-level-7 ((,class (:foreground ,erosion-red-3))))
   `(org-level-8 ((,class (:foreground ,erosion-blue-4))))
   `(org-link ((,class (:foreground ,erosion-yellow-2 :underline t))))
   `(org-scheduled ((,class (:foreground ,erosion-green+4))))
   `(org-scheduled-previously ((,class (:foreground ,erosion-red-3))))
   `(org-scheduled-today ((,class (:foreground ,erosion-blue+1))))
   `(org-special-keyword ((,class (:foreground ,erosion-yellow-1))))
   `(org-table ((,class (:foreground ,erosion-green+2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,erosion-orange))))
   `(org-todo ((,class (:bold t :foreground ,erosion-red :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,erosion-red :weight bold))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; post-mode
   `(post-email-address-text-face ((,class (:foreground ,erosion-yellow :weight bold))))
   `(post-emoticon-face ((,class (:foreground ,erosion-green+2 :weight bold))))
   `(post-header-keyword-face ((,class (:foreground ,erosion-bg+1))))
   `(post-header-value-face ((,class (:foreground ,erosion-fg))))
   `(post-multiply-quoted-text-face ((,class (:foreground ,erosion-orange))))
   `(post-quoted-text-face ((,class (:foreground ,erosion-yellow))))
   `(post-signature-text-face ((,class (:foreground ,erosion-bg+2))))
   `(post-underline-face ((,class (:foreground ,erosion-green))))
   `(post-url-face ((,class (:foreground ,erosion-blue-2 :underline t))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,erosion-cyan :weight bold))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,erosion-yellow :weight bold))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,erosion-blue+1 :weight bold))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,erosion-red+1 :weight bold))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,erosion-green+1 :weight bold))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,erosion-blue-1 :weight bold))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,erosion-green+4 :weight bold))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,erosion-red-3 :weight bold))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,erosion-yellow-2 :weight bold))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,erosion-green+2 :weight bold))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,erosion-blue+1 :weight bold))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,erosion-red-3 :weight bold))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,erosion-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,erosion-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,erosion-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,erosion-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,erosion-red))))
   `(rpm-spec-package-face ((,class (:foreground ,erosion-red))))
   `(rpm-spec-section-face ((,class (:foreground ,erosion-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,erosion-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,erosion-red))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,erosion-fg :background ,erosion-red :weight bold))))
   `(show-paren-match ((,class (:foreground ,erosion-fg :background ,erosion-blue+1 :weight bold))))

   ;; speedbar
   `(speedbar-button-face ((,class (:foreground ,erosion-green+1))))
   `(speedbar-directory-face ((,class (:foreground ,erosion-blue+1))))
   `(speedbar-file-face ((,class (:foreground ,erosion-fg))))
   `(speedbar-highlight-face ((,class (:background ,erosion-bg+2))))
   `(speedbar-selected-face ((,class (:background ,erosion-bg+1))))
   `(speedbar-separator-face ((,class (:foreground ,erosion-bg+1))))
   `(speedbar-tag-face ((,class (:foreground ,erosion-yellow))))

   ;; wl (wanderlust)
   ;; some faces end with -face, while other don't; confusing
   `(wl-highlight-folder-few-face ((,class (:foreground ,erosion-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,erosion-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,erosion-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,erosion-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,erosion-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,erosion-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,erosion-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,erosion-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,erosion-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,erosion-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,erosion-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,erosion-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,erosion-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,erosion-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,erosion-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,erosion-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,erosion-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,erosion-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,erosion-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,erosion-fg
                                                              :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,erosion-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,erosion-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,erosion-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,erosion-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,erosion-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold)))))

  (custom-theme-set-variables
   'erosiond

   `(ansi-color-names-vector
     [,erosion-bg ,erosion-red ,erosion-green ,erosion-yellow
                  ,erosion-blue ,erosion-magenta ,erosion-cyan ,erosion-fg])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,erosion-bg ,erosion-red ,erosion-green ,erosion-yellow
                  ,erosion-blue ,erosion-magenta ,erosion-blue ,erosion-fg])))

(provide-theme 'erosiond)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; erosiond-theme.el ends here.
