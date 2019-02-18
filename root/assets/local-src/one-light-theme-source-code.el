;;; one-themes.el --- One Colorscheme -*- lexical-binding: t -*-

;; Copyright (C) 2018 Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>
;; URL: http://github.com/balajisivaraman/emacs-one-themes
;; Version: 0
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the Vim/Atom One colorscheme to Emacs.
;;
;;; Credits
;;
;; This theme is heavily inspired by the Vim port of One created by
;; Ramzi Akremi.
;;
;;; Code:

;;; Setup
(defconst one-themes-colors
  '((dark . ((mono1 . "#ABB2BF")
             (mono2 . "#828997")
             (mono3 . "#5C6370")
             (mono4 . "#4B5263")
             (cyan . "#56B6C2")
             (blue . "#61AFEF")
             (violet . "#C678DD")
             (green . "#98C379")
             (red1 . "#E06C75")
             (red2 . "#BE5046")
             (orange1 . "#D19A66")
             (orange2 . "#E5C07B")
             (background . "#282C34")
             (contrast-bg . "#2C323C")
             (low-contrast-bg . "#292E37")
             (fringe . "#636D83")
             (accent . "#528BFF")
             (highlight . "#3E4452")))
    (light . ((mono1 . "#494B53")
              (mono2 . "#696C77")
              (mono3 . "#A0A1A7")
              (mono4 . "#C2C2C3")
              (cyan . "#0184BC")
              (blue . "#4078F2")
              (violet . "#A626A4")
              (green . "#50A14F")
              (red1 . "#E45649")
              (red2 . "#CA1243")
              (orange1 . "#986801")
              (orange2 . "#C18401")
              (background . "#FAFAFA")
              (contrast-bg . "#F0F0F0")
              (low-contrast-bg . "#F5F5F5")
              (fringe . "#9E9E9E")
              (accent . "#526FFF")
              (highlight . "#D0D0D0"))))
  "Defines the colors to be used for the light and dark variants of One.")

(defmacro one-themes-with-color-variables (variant &rest body)
  "Helper macro to setup colors for the provided VARIANT to be used in BODY."
  (declare (indent defun))
  `(let* ((colors (cdr (assoc ,variant one-themes-colors)))
          (mono1 (cdr (assoc 'mono1 colors)))
          (mono2 (cdr (assoc 'mono2 colors)))
          (mono3 (cdr (assoc 'mono3 colors)))
          (mono4 (cdr (assoc 'mono4 colors)))
          (cyan (cdr (assoc 'cyan colors)))
          (blue (cdr (assoc 'blue colors)))
          (violet (cdr (assoc 'violet colors)))
          (green (cdr (assoc 'green colors)))
          (red1 (cdr (assoc 'red1 colors)))
          (red2 (cdr (assoc 'red2 colors)))
          (orange1 (cdr (assoc 'orange1 colors)))
          (orange2 (cdr (assoc 'orange2 colors)))
          (background (cdr (assoc 'background colors)))
          (contrast-bg (cdr (assoc 'contrast-bg colors)))
          (low-contrast-bg (cdr (assoc 'low-contrast-bg colors)))
          (fringe (cdr (assoc 'fringe colors)))
          (accent (cdr (assoc 'accent colors)))
          (highlight (cdr (assoc 'highlight colors)))
          (foreground mono1)
          (comment mono3)
          (class '((class color) (min-colors 89))))
     ,@body))

(defun one-themes-create-theme (variant theme-name)
  "Create a VARIANT of the theme named THEME-NAME."
  (one-themes-with-color-variables
    variant
    (custom-theme-set-faces
     theme-name
     ;; Emacs Interface Colors
     `(default ((,class (:foreground ,foreground :background ,background))))
     `(cursor ((,class (:background ,mono1))))
     `(fringe ((,class (:background ,low-contrast-bg :foreground ,fringe))))
     `(linum ((,class (:background ,background :foreground ,comment :italic nil :underline nil))))
     `(highlight ((,class (:background ,contrast-bg))))
     `(lazy-highlight ((,class (:foreground ,background :background ,orange2
                                            :weight normal))))
     `(vertical-border ((,class (:foreground ,contrast-bg))))
     `(border ((,class (:background ,contrast-bg :foreground ,highlight))))
     `(region ((,class (:background ,highlight :inverse-video nil))))
     `(secondary-selection ((,class (:background ,highlight))))
     `(mode-line ((,class (:foreground ,foreground :background ,contrast-bg :weight normal
                                       :box (:line-width 1 :color ,contrast-bg)))))
     `(mode-line-buffer-id ((,class (:foreground ,mono1 :background nil))))
     `(mode-line-inactive ((,class (:inherit mode-line
                                             :foreground ,comment
                                             :background ,highlight :weight normal))))
     `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
     `(mode-line-highlight ((,class (:foreground ,violet :box nil :weight bold))))
     `(match ((,class (:background ,background :foreground ,accent :weight bold))))
     `(menu ((,class (:foreground ,blue :background ,background))))
     `(minibuffer-prompt ((,class (:foreground ,blue))))
     `(header-line ((,class (:inherit mode-line-inactive :foreground ,orange1 :background nil))))

     ;; Standard font lock faces
     `(font-lock-builtin-face ((,class (:foreground ,mono2))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
     `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
     `(font-lock-constant-face ((,class (:foreground ,green))))
     `(font-lock-doc-face ((,class (:foreground ,comment))))
     `(font-lock-doc-string-face ((,class (:foreground ,orange2))))
     `(font-lock-function-name-face ((,class (:foreground ,blue))))
     `(font-lock-keyword-face ((,class (:foreground ,red1))))
     `(font-lock-negation-char-face ((,class (:foreground ,blue))))
     `(font-lock-preprocessor-face ((,class (:foreground ,orange2))))
     `(font-lock-regexp-grouping-backslash ((,class (:foreground ,orange2))))
     `(font-lock-regexp-grouping-construct ((,class (:foreground ,violet))))
     `(font-lock-string-face ((,class (:foreground ,green))))
     `(font-lock-type-face ((,class (:foreground ,blue))))
     `(font-lock-variable-name-face ((,class (:foreground ,orange2))))
     `(font-lock-warning-face ((,class (:weight bold :foreground ,red2))))
     `(shadow ((,class (:foreground ,comment))))
     `(success ((,class (:foreground ,green))))
     `(error ((,class (:foreground ,red2))))
     `(warning ((,class (:foreground ,orange1))))
     `(tooltip ((,class (:foreground ,orange2 :background ,background :inverse-video t))))
     `(link ((,class (:foreground nil :underline t))))

     ;; Company
     `(company-tooltip ((,class (:background ,contrast-bg))))
     `(company-tooltip-selection ((,class (:foreground ,comment :inverse-video t))))
     `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,red2))))
     `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,red2))))
     `(company-tooltip-annotation ((,class (:inherit company-tooltip :foreground ,green))))
     `(company-tooltip-annotation-selection ((,class (:inherit company-tooltip-selection :foreground ,green))))
     `(company-scrollbar-bg ((,class (:inherit 'company-tooltip :background ,highlight))))
     `(company-scrollbar-fg ((,class (:background ,contrast-bg))))
     `(company-tooltip-search ((,class (:inherit company-tooltip :foreground ,accent))))
     `(company-preview ((,class (:foreground ,comment :background ,contrast-bg))))
     `(company-preview-common ((,class (:inherit company-preview :foreground ,red2))))
     `(company-preview-search ((,class (:inherit company-preview :foreground ,blue))))
     `(company-echo-common ((,class (:inherit company-echo :foreground ,red2))))

     ;; Dired
     `(dired-directory ((,class (:foreground ,blue :weight normal))))
     `(dired-flagged ((,class (:foreground ,red1))))
     `(dired-header ((,class (:foreground ,background :background ,blue))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,orange1 :weight bold))))
     `(dired-marked ((,class (:foreground ,red1 :weight bold))))
     `(dired-perm-write ((,class (:foreground ,foreground :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :weight normal :slant italic))))
     `(dired-warning ((,class (:foreground ,orange2 :underline t))))

     ;; Dired+
     `(diredp-compressed-file-suffix ((,class (:foreground ,blue))))
     `(diredp-compressed-file-name ((,class (:foreground ,blue))))
     `(diredp-deletion ((,class (:inherit error :inverse-video t))))
     `(diredp-deletion-file-name ((,class (:inherit error))))
     `(diredp-date-time ((,class (:foreground ,cyan))))
     `(diredp-dir-heading ((,class (:foreground ,green :weight bold))))
     `(diredp-dir-name ((,class (:foreground ,blue))))
     `(diredp-dir-priv ((,class (:foreground ,violet :background nil))))
     `(diredp-exec-priv ((,class (:foreground ,orange1 :background nil))))
     `(diredp-executable-tag ((,class (:foreground ,red2 :background nil))))
     `(diredp-file-name ((,class (:foreground ,orange2))))
     `(diredp-file-suffix ((,class (:foreground ,green))))
     `(diredp-flag-mark ((,class (:foreground ,green :inverse-video t))))
     `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
     `(diredp-ignored-file-name ((,class (:foreground ,comment))))
     `(diredp-link-priv ((,class (:background nil :foreground ,violet))))
     `(diredp-mode-line-flagged ((,class (:foreground ,red2))))
     `(diredp-mode-line-marked  ((,class (:foreground ,green))))
     `(diredp-no-priv ((,class (:background nil))))
     `(diredp-number ((,class (:foreground ,orange1))))
     `(diredp-other-priv ((,class (:background nil :foreground ,violet))))
     `(diredp-rare-priv ((,class (:foreground ,red2 :background nil))))
     `(diredp-read-priv ((,class (:foreground ,green :background nil))))
     `(diredp-symlink ((,class (:foreground ,violet))))
     `(diredp-write-priv ((,class (:foreground ,orange1 :background nil))))

     ;; Ediff
     `(ediff-fine-diff-A ((,class (:background ,red1 :foreground ,mono4))))
     `(ediff-fine-diff-B ((,class (:background ,green :foreground ,mono4))))
     `(ediff-even-diff-A ((,class (:background ,highlight :foreground ,mono1))))
     `(ediff-odd-diff-A ((,class (:background  ,fringe :foreground ,mono1))))
     `(ediff-even-diff-B ((,class (:background ,highlight :foreground ,mono1))))
     `(ediff-odd-diff-B ((,class (:background  ,fringe :foreground ,mono1))))

     ;; ElDoc
     `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

     ;; Flycheck
     `(flycheck-error ((,class (:underline (:style wave :color ,red1)))))
     `(flycheck-info ((,class (:underline (:style wave :color ,cyan)))))
     `(flycheck-warning ((,class (:underline (:style wave :color ,orange1)))))
     `(flycheck-fringe-error ((,class (:foreground ,red1))))
     `(flycheck-fringe-info ((,class (:foreground ,cyan))))
     `(flycheck-fringe-warning ((,class (:foreground ,orange1))))
     `(flycheck-color-mode-line-error-face ((,class (:foreground ,red1))))
     `(flycheck-color-mode-line-warning-face ((,class (:foreground ,orange1))))
     `(flycheck-color-mode-line-info-face ((,class (:foreground ,cyan))))
     `(flycheck-color-mode-line-running-face ((,class (:foreground ,comment))))
     `(flycheck-color-mode-line-success-face ((,class (:foreground ,green))))

     ;; Flymake
     `(flymake-error ((,class (:underline (:style wave :color ,red1)))))
     `(flymake-note ((,class (:underline (:style wave :color ,cyan)))))
     `(flymake-warning ((,class (:underline (:style wave :color ,orange1)))))

     ;; Flyspell
     `(flyspell-incorrect ((,class (:underline (:style wave :color ,red1)))))

     ;; Helm
     `(helm-header ((,class (:inherit header-line))))
     `(helm-header-line-left-margin ((,class (:inherit header-line))))
     `(helm-match ((,class (:inherit match))))
     `(helm-M-x-key ((,class (:foreground ,red1 :underline t))))
     `(helm-buffer-size ((,class (:foreground ,foreground))))
     `(helm-candidate-number ((,class (:foreground ,mono2 :bold t))))
     `(helm-selection ((,class (:background ,highlight :foreground ,mono2))))
     `(helm-selection-line ((,class (:background ,highlight :foreground ,mono2
                                                 :underline nil))))
     `(helm-separator ((,class (:foreground ,red1))))
     `(helm-source-header ((,class (:background ,blue :foreground ,background
                                                :underline nil :bold t))))
     `(helm-visible-mark ((,class (:foreground ,violet :bold t))))

     ;; Helm Buffers
     `(helm-buffer-not-saved ((,class (:foreground ,orange1))))
     `(helm-buffer-saved-out ((,class (:foreground ,red1 :background ,background
                                                   :inverse-video t))))
     `(helm-buffer-size ((,class (:foreground ,foreground))))
     `(helm-buffer-directory ((,class (:foreground ,red2))))
     `(helm-buffer-process ((,class (:foreground ,orange1))))

     ;; Helm Find Files
     `(helm-ff-directory ((,class (:foreground ,red2))))
     `(helm-ff-dotted-directory ((,class (:background ,mono3 :foreground ,background))))
     `(helm-ff-executable ((,class (:foreground ,green))))
     `(helm-ff-file ((,class (:foreground ,foreground))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,orange1 :slant italic))))
     `(helm-ff-prefix ((,class (:background ,orange2 :foreground ,background))))
     `(helm-ff-symlink ((,class (:foreground ,cyan))))

     ;; Helm Grep
     `(helm-grep-file ((,class (:foreground ,cyan :underline t))))
     `(helm-grep-finish ((,class (:foreground ,green))))
     `(helm-grep-lineno ((,class (:foreground ,orange1))))
     `(helm-grep-match ((,class (:inherit match))))

     ;; Helm Bookmarks
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,mono2))))
     `(helm-bookmark-gnus ((,class (:foreground ,cyan))))
     `(helm-bookmark-info ((,class (:foreground ,green))))
     `(helm-bookmark-man ((,class (:foreground ,violet))))
     `(helm-bookmark-w3m ((,class (:foreground ,orange1))))
     `(helm-bookmarks-su ((,class (:foreground ,orange2))))

     ;; Isearch
     `(isearch ((,class (:foreground ,background :background ,red1 :weight normal))))
     `(isearch-fail ((,class (:foreground ,orange2 :background ,background :bold t))))

     ;; Ledger
     `(ledger-font-comment-face ((,class (:inherit font-lock-comment-face))))
     `(ledger-font-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
     `(ledger-font-occur-xact-face ((,class (:inherit highlight))))
     `(ledger-font-payee-cleared-face ((,class (:foreground ,green))))
     `(ledger-font-payee-uncleared-face ((,class (:foreground ,red1))))
     `(ledger-font-posting-date-face ((,class (:foreground ,orange1))))
     `(ledger-font-posting-amount-face ((,class (:foreground ,foreground))))
     `(ledger-font-posting-account-cleared-face ((,class (:foreground ,cyan))))
     `(ledger-font-posting-account-face ((,class (:foreground ,blue))))
     `(ledger-font-posting-account-pending-face ((,class (:foreground ,orange2))))
     `(ledger-font-xact-highlight-face ((,class (:inherit highlight))))
     `(ledger-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
     `(ledger-occur-xact-face ((,class (:inherit highlight))))

     ;; LSP
     `(lsp-face-highlight-read ((,class (:background ,red1 :foreground ,background))))
     `(lsp-face-highlight-textual ((,class (:background ,orange1 :foreground ,background))))
     `(lsp-face-highlight-write ((,class (:background ,green :foreground ,background))))
     `(lsp-ui-doc-background ((,class (:background ,contrast-bg :foreground ,foreground))))
     `(lsp-ui-doc-header ((,class (:background ,blue :foreground ,background))))
     `(lsp-ui-doc-url ((,class (:inherit link :foreground ,blue))))
     `(lsp-ui-sideline-code-action ((,class (:foreground ,orange2))))
     `(lsp-ui-sideline-current-symbol ((,class (:foreground ,foreground :weight bold
                                                            :height 0.99
                                                            :box (:line-width -1 :color ,foreground)))))
     `(lsp-ui-sideline-symbol ((,class (:foreground ,foreground :height 0.99
                                                    :box (:line-width -1 :color ,foreground)))))

     ;; Magit
    ;;;; Headings
     `(magit-section-highlight ((t (:inherit contrast-bg))))
     `(magit-section-heading ((t (:foreground ,orange2 :weight bold))))
     `(magit-section-heading-selection ((t (:foreground ,orange1 :weight bold))))
     `(magit-diff-file-heading ((t (:weight bold))))
     `(magit-diff-file-heading-highlight ((t (:background ,contrast-bg))))
     `(magit-diff-file-heading-selection ((t (:background ,contrast-bg
                                                          :foreground ,orange1))))
     `(magit-diff-hunk-heading ((t (:background ,highlight :foreground ,mono2))))
     `(magit-diff-hunk-heading-highlight ((t (:background ,highlight :foreground ,mono1 :weight bold))))
     `(magit-diff-hunk-heading-selection ((t (:background ,contrast-bg :foreground ,orange1
                                                          :weight bold))))

    ;;;; Diffs
     `(magit-diff-base ((t (:foreground ,background :background ,orange1))))
     `(magit-diff-base-highlight ((t (:foreground ,orange1 :background ,highlight))))
     `(magit-diff-added ((t (:foreground ,green))))
     `(magit-diff-added-highlight ((t (:foreground ,green :background ,contrast-bg))))
     `(magit-diff-removed ((t (:foreground ,red2))))
     `(magit-diff-removed-highlight ((t (:foreground ,red2 :background ,contrast-bg))))
     `(magit-diff-lines-heading ((t (:background ,orange2 :foreground ,background))))
     `(magit-diff-context ((t (:foreground ,mono2))))
     `(magit-diff-context-highlight ((t (:foreground ,mono2 :background ,contrast-bg))))
     `(magit-diffstat-added ((t (:foreground ,green))))
     `(magit-diffstat-removed ((t (:foreground ,red2))))

    ;;;; popup
     `(magit-popup-heading             ((t (:foreground ,orange1  :weight bold))))
     `(magit-popup-key                 ((t (:foreground ,mono4   :weight bold))))
     `(magit-popup-argument            ((t (:foreground ,cyan    :weight bold))))
     `(magit-popup-disabled-argument   ((t (:foreground ,mono2  :weight normal))))
     `(magit-popup-option-value        ((t (:foreground ,cyan    :weight bold))))

    ;;;; process
     `(magit-process-ok    ((t (:foreground ,green :weight bold))))
     `(magit-process-ng ((t (:foreground ,red2 :weight bold))))

    ;;;; sequence
     `(magit-sequence-pick ((t (:foreground ,orange1))))
     `(magit-sequence-stop ((t (:foreground ,green))))
     `(magit-sequence-part ((t (:foreground ,orange2))))
     `(magit-sequence-head ((t (:foreground ,blue))))
     `(magit-sequence-drop ((t (:foreground ,red2))))
     `(magit-sequence-done ((t (:foreground ,comment))))
     `(magit-sequence-onto ((t (:foreground ,comment))))

    ;;;; bisect
     `(magit-bisect-good ((t (:foreground ,green))))
     `(magit-bisect-skip ((t (:foreground ,orange1))))
     `(magit-bisect-bad ((t (:foreground ,red2))))

    ;;;; log
     `(magit-log-author ((t (:foreground ,orange2))))
     `(magit-log-date   ((t (:foreground ,blue))))
     `(magit-log-graph  ((t (:foreground ,comment))))

    ;;;; references etc.
     `(magit-dimmed         ((t (:foreground ,comment))))
     `(magit-hash           ((t (:foreground ,comment))))
     `(magit-tag            ((t (:foreground ,cyan :weight bold))))
     `(magit-branch-remote  ((t (:foreground ,green  :weight bold))))
     `(magit-branch-local   ((t (:foreground ,blue   :weight bold))))
     `(magit-branch-current ((t (:foreground ,blue   :weight bold :box t))))
     `(magit-head           ((t (:foreground ,blue   :weight bold))))
     `(magit-refname        ((t (:background ,contrast-bg :foreground ,mono4 :weight bold))))
     `(magit-refname-stash  ((t (:background ,contrast-bg :foreground ,mono4 :weight bold))))
     `(magit-refname-wip    ((t (:background ,contrast-bg :foreground ,mono4 :weight bold))))
     `(magit-signature-good      ((t (:foreground ,green))))
     `(magit-signature-bad       ((t (:foreground ,red2))))
     `(magit-signature-untrusted ((t (:foreground ,orange1))))
     `(magit-cherry-unmatched    ((t (:foreground ,cyan))))
     `(magit-cherry-equivalent   ((t (:foreground ,red1))))
     `(magit-reflog-commit       ((t (:foreground ,green))))
     `(magit-reflog-amend        ((t (:foreground ,red1))))
     `(magit-reflog-merge        ((t (:foreground ,green))))
     `(magit-reflog-checkout     ((t (:foreground ,blue))))
     `(magit-reflog-reset        ((t (:foreground ,red2))))
     `(magit-reflog-rebase       ((t (:foreground ,red1))))
     `(magit-reflog-cherry-pick  ((t (:foreground ,green))))
     `(magit-reflog-remote       ((t (:foreground ,cyan))))
     `(magit-reflog-other ((t (:foreground ,cyan))))

     ;; Markdown
     `(markdown-blockquote-face ((,class (:inherit font-lock-doc-face))))
     `(markdown-code-face ((,class (:inherit fixed-pitch :background ,background :foreground ,violet))))
     `(markdown-comment-face ((,class (:foreground ,comment :strike-through t))))
     `(markdown-footnote-face ((,class (:inherit default))))
     `(markdown-header-delimiter-face ((,class (:foreground ,comment))))
     `(markdown-header-face ((,class (:foreground ,blue))))
     `(markdown-header-face-1 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-2 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-3 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-4 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-5 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-6 ((,class (:inherit markdown-header-face))))
     `(markdown-list-face ((,class (:inherit font-lock-builtin-face))))
     `(markdown-math-face ((,class (:inherit font-lock-string-face))))
     `(markdown-url-face ((,class (:inherit link))))
     `(markdown-link-face ((,class (:foreground ,blue :underline t))))
     `(markdown-link-title-face ((,class (:inherit font-lock-comment-face))))
     `(markdown-inline-code-face ((,class (:inherit markdown-code-face))))

     ;; Org Mode
     `(org-agenda-structure ((,class (:foreground ,blue))))
     `(org-agenda-calendar-event ((,class (:foreground ,comment))))
     `(org-agenda-calendar-sexp ((,class (:foreground ,mono4 :slant italic))))
     `(org-agenda-date ((,class (:background ,background :foreground ,blue :weight normal
                                             :box (:line-width 2 :color ,background)
                                             :inverse-video nil :overline nil :underline nil))))
     `(org-agenda-date-weekend ((,class (:inherit org-agenda-date :inverse-video nil :background unspecified
                                                  :foreground ,blue :weight unspecified
                                                  :underline t :overline nil :box unspecified))))
     `(org-agenda-date-today
       ((,class (:inherit org-agenda-date :inverse-video t :weight bold
                          :underline unspecified :overline nil :box unspecified
                          :foreground ,blue :background ,background))))
     `(org-agenda-done ((,class (:foreground ,green :slant italic))))
     `(org-archived ((,class (:foreground ,comment :weight normal))))
     `(org-block ((,class (:foreground ,comment))))
     `(org-checkbox ((,class (:background ,background :foreground ,foreground
                                          :box (:line-width 1 :style released-button)))))
     `(org-code ((,class (:foreground ,comment))))
     `(org-date ((,class (:foreground ,blue :underline t))))
     `(org-done ((,class (:weight bold :foreground ,green))))
     `(org-ellipsis ((,class (:foreground ,comment))))
     `(org-formula ((,class (:foreground ,orange2))))
     `(org-headline-done ((,class (:foreground ,green))))
     `(org-hide ((,class (:foreground ,background))))
     `(org-level-1 ((,class (:foreground ,orange1))))
     `(org-level-2 ((,class (:foreground ,green))))
     `(org-level-3 ((,class (:foreground ,blue))))
     `(org-level-4 ((,class (:foreground ,red1))))
     `(org-level-5 ((,class (:foreground ,cyan))))
     `(org-level-6 ((,class (:foreground ,orange2))))
     `(org-level-7 ((,class (:foreground ,green))))
     `(org-level-8 ((,class (:foreground ,red2))))
     `(org-link ((,class (:foreground ,orange2 :underline t))))
     `(org-meta-line ((,class (:foreground ,comment :slant italic))))
     `(org-sexp-date ((,class (:foreground ,violet))))
     `(org-scheduled ((,class (:foreground ,green))))
     `(org-scheduled-previously ((,class (:foreground ,cyan))))
     `(org-scheduled-today ((,class (:foreground ,blue :weight normal))))
     `(org-special-keyword ((,class (:foreground ,comment :weight bold))))
     `(org-table ((,class (:foreground ,green))))
     `(org-tag ((,class (:weight bold))))
     `(org-time-grid ((,class (:foreground ,comment))))
     `(org-todo ((,class (:foreground ,red2 :weight bold))))
     `(org-upcoming-deadline ((,class (:foreground ,orange2  :weight normal :underline nil))))
     `(org-warning ((,class (:foreground ,orange1 :weight normal :underline nil))))

     ;; Parenthesis matching (built-in)
     `(show-paren-match ((,class (:background ,violet :foreground ,background))))
     `(show-paren-mismatch ((,class (:background ,red1 :foreground ,background))))

     ;; Rainbow Delimiters
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,orange1))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,red2))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,orange2))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-10-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-11-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-12-face ((,class (:foreground ,red1))))
     `(rainbow-delimiters-depth-unmatched-face ((,class (:foreground ,foreground :inverse-video t))))

     ;; Spaceline
     `(spaceline-evil-emacs ((,class (:background ,accent :foreground ,background))))
     `(spaceline-evil-normal ((,class (:background ,green :foreground ,background))))
     `(spaceline-evil-insert ((,class (:background ,blue :foreground ,background))))
     `(spaceline-evil-visual ((,class (:background ,violet :foreground ,background))))
     `(spaceline-evil-replace ((,class (:background ,red1 :foreground ,background))))
     `(spaceline-highlight-face ((,class (:background ,orange1 :foreground ,background))))
     `(spaceline-modified ((,class (:background ,blue :foreground ,background))))
     `(spaceline-readonly ((,class (:background ,violet :foreground ,background))))
     `(spaceline-unmodified ((,class (:background ,orange2 :foreground ,background))))
     `(spaceline-flycheck-error ((,class (:foreground ,red1))))
     `(spaceline-flycheck-warning ((,class (:foreground ,orange1))))
     `(spaceline-flycheck-info ((,class (:foreground ,cyan))))

     ;; VC
     `(vc-annotate-background-mode nil)
     `(vc-annotate-color-map
       '((20  . ,red2)
         (40  . ,orange2)
         (60  . ,orange1)
         (80  . ,green)
         (100 . ,cyan)
         (120 . ,blue)
         (140 . ,violet)
         (160 . ,red2)
         (180 . ,orange2)
         (200 . ,orange1)
         (220 . ,green)
         (240 . ,cyan)
         (260 . ,blue)
         (280 . ,violet)
         (300 . ,red2)
         (320 . ,orange2)
         (340 . ,orange1)
         (360 . ,green)))
     `(vc-annotate-very-old-color nil)
     `(vc-annotate-background nil)

     ;; Which Function
     `(which-func ((,class (:foreground ,blue :background nil :weight bold)))))

    (custom-theme-set-variables
     theme-name
     `(company-quickhelp-color-background ,highlight)
     `(company-quickhelp-color-foreground ,foreground))
    ))

;;; Footer

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'one-themes)
;;; one-themes.el ends here
(require 'one-themes)

(deftheme one-light "The light variant of the One colour theme")

(one-themes-create-theme 'light 'one-light)

(provide-theme 'one-light)
