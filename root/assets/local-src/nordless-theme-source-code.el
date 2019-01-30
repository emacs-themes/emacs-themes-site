;;; nordless-theme.el --- A mostly colorless theme

;; Copyright (C) 2018 Thomas Letan
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author: Thomas Letan <contact@thomasletan.fr>
;; URL: https://github.com/lethom/nordless-theme.el
;; Version: 0.1
;; License: GPL-3
;; Keywords: theme, dark

;;; Commentary:

;; nordless has two main source of inspiration: nofrils[1], an extremely
;; minimalist colorscheme for vim, and nord[2], a north-bluish color palette.
;;
;; [1]: https://github.com/robertmeta/nofrils
;; [2]: https://github.com/arcticicestudio/nord

;;; Code:

(deftheme nordless "A mostly colorless theme")

(let ((nord0   "#2E3440") ; polar night
      (nord1   "#3B4252")
      (nord2   "#434C5E")
      (nord3   "#4C566A")
      (nord4   "#D8DEE9") ; snow storm
      (nord5   "#E5E9F0")
      (nord6   "#ECEFF4")
      (nord7   "#8FBCBB") ; frost
      (nord8   "#88C0D0")
      (nord9   "#81A1C1")
      (nord10  "#5E81AC")
      (nord11  "#BF616A") ; aurora
      (nord12  "#D08770")
      (nord13  "#EBCB8B")
      (nord14  "#A3BE8C")
      (nord15  "#B48EAD")
      )

  (custom-theme-set-variables
   'nordless
   `(fci-rule-color ,nord3)
   )
  (custom-theme-set-faces
   'nordless
   `(default ((t (:background ,nord0 :foreground ,nord4))))
   `(mode-line ((t (:background ,nord3))))
   `(mode-line-inactive ((t ())))
   `(powerline-active1 ((t (:background ,nord0))))
   `(powerline-active2 ((t (:background ,nord1))))
   `(powerline-inactive1 ((t (:background ,nord3))))
   `(powerline-inactive2 ((t (:background ,nord3))))
   '(link ((t (:underline t))))
   '(secondary-selection ((t ())))
   '(shadow ((t ())))
   `(region ((t (:background ,nord4 :foreground ,nord0))))
   '(escape-glyph ((t ())))
   '(font-lock-warning-face ((t ())))
   '(font-lock-negation-char-face ((t ())))
   '(font-lock-regexp-grouping-backslash ((t ())))
   '(font-lock-regexp-grouping-construct ((t ())))
   '(font-lock-builtin-face ((t ())))
   '(minibuffer-prompt ((t ())))
   '(font-lock-constant-face ((t ())))
   '(font-lock-keyword-face ((t ())))
   '(font-lock-function-name-face ((t ())))
   '(font-lock-variable-name-face ((t ())))
   '(font-lock-preprocessor-face ((t ())))
   '(font-lock-type-face ((t ())))
   `(font-lock-comment-delimiter-face ((t (:foreground ,nord8))))
   `(font-lock-comment-face ((t (:foreground ,nord8))))
   `(font-lock-doc-face ((t (:weight bold :foreground ,nord8))))
   '(font-lock-string-face ((t (:weight bold))))
   `(whitespace-space ((t (:foreground ,nord3))))
   `(whitespace-tab ((t (:foreground ,nord3))))
   `(whitespace-newline ((t (:foreground ,nord3))))
   `(linum ((t (:foreground ,nord3))))
   `(line-number ((t (:foreground ,nord3 :height 0.8))))
   `(line-number-current-line ((t (:foreground ,nord4 :height 0.8))))
   `(hl-line ((t (:background ,nord2))))
   `(show-paren-match ((t (:foreground ,nord8 :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,nord11 :weight bold))))
   `(hl-paren-face ((t (:foreground ,nord8 :weight bold))))
   '(dired-directory ((t (:weight bold))))
   '(diredp-file-suffix ((t ())))
   '(diredp-ignored-file-name ((t ())))
   `(org-footnote ((t ())))
   `(org-level-1 ((t (:height 1.2))))
   `(org-level-2 ((t (:height 1.1))))
   `(org-level-3 ((t ())))
   `(org-level-4 ((t ())))
   `(org-level-5 ((t ())))
   `(org-level-6 ((t ())))
   `(org-level-7 ((t ())))
   `(org-level-8 ((t ())))
   `(org-date ((t ())))
   `(org-tag ((t (:foreground ,nord10))))
   `(org-todo ((t (:foreground ,nord13))))
   `(org-done ((t (:foreground ,nord14))))
   `(org-block ((t (:foreground ,nord7))))
   `(org-block-begin-line ((t (:height 0.7))))
   `(org-block-end-line ((t (:height 0.7))))
   '(org-meta-line ((t ())))
   '(org-document-info-keyword ((t ())))
   '(org-document-info ((t ())))
   '(proof-tactics-name-face ((t ())))
   '(proof-tacticals-name-face ((t ())))
   `(proof-locked-face ((t (:background ,nord3))))
   `(proof-queue-face ((t (:background ,nord1))))
   `(proof-error-face ((t (:foreground ,nord11))))
   `(proof-warning-face ((t (:foreground ,nord12))))
   `(proof-declaration-name-face ((t (:weight bold))))
   `(coq-cheat-face ((t (:foreground ,nord0 :background ,nord11))))
   `(coq-solve-tactics-face ((t ())))
   '(elixir-atom-face ((t ())))
   `(elixir-attribute-face ((t (:foreground ,nord8))))
   `(haskell-pragma-face ((t (:weight bold :foreground ,nord8))))
   `(diff-header ((t (:weight bold))))
   `(diff-context ((t ())))
   `(diff-file-header ((t (:foreground ,nord7))))
   `(diff-added ((t (:foreground ,nord14))))
   `(diff-removed ((t (:foreground ,nord11))))
   `(diff-changed ((t (:foreground ,nord13))))
   `(diff-hl-change ((t (:foreground ,nord3 :background ,nord13))))
   `(diff-hl-insert ((t (:foreground ,nord3 :background ,nord14))))
   `(diff-hl-delete ((t (:foreground ,nord3 :background ,nord11))))
   `(flycheck-info ((t (:foreground ,nord14))))
   `(flycheck-error ((t (:foreground ,nord11))))
   `(flycheck-warning ((t (:foreground ,nord12))))
   `(flycheck-fringe-info ((t (:foreground ,nord14))))
   `(flycheck-fringe-error ((t (:foreground ,nord11))))
   `(flycheck-fringe-warning ((t (:foreground ,nord12))))
   `(flyspell-incorrect ((t (:foreground ,nord11))))
   `(flyspell-duplicate ((t (:foreground ,nord12))))
   `(fringe ((t (()))))
   '(git-commit-summary ((t ())))
   `(git-commit-overlong-summary ((t (:foreground ,nord11))))
   `(git-commit-nonempty-second-line ((t (:foreground ,nord11))))
   '(magit-diff-hunk-heading ((t (:slant italic))))
   '(magit-diff-hunk-heading-highlight ((t (:slant italic))))
   '(magit-header ((t (:weight bold))))
   '(magit-section-heading ((t (:weight bold))))
   '(magit-section-heading-selection ((t ())))
   '(magit-selection-title ((t ())))
   '(magit-selection-highlight ((t (:inherit default))))
   '(magit-selection ((t ())))
   `(magit-diff-context ((t ())))
   `(magit-diff-context-highlight ((t ())))
   `(magit-diff-removed-highlight ((t (:foreground ,nord11))))
   `(magit-diff-removed ((t (:foreground ,nord11))))
   `(magit-diff-added-highlight ((t (:foreground ,nord14))))
   `(magit-diff-added ((t (:foreground ,nord14))))
   '(markdown-header-delimiter-face ((t ())))
   '(markdown-header-face-1 ((t ())))
   '(markdown-header-face-2 ((t ())))
   '(markdown-header-face-3 ((t ())))
   '(markdown-header-face-4 ((t ())))
   '(markdown-header-face-5 ((t ())))
   '(markdown-header-face-6 ((t ())))
   '(markdown-markup-face ((t ())))
   '(markdown-bold-face ((t (:weight bold))))
   '(markdown-italic-face ((t (:italic t))))
   '(markdown-link-face ((t ())))
   '(markdown-url-face ((t (:underline t))))
   '(font-latex-warning-face ((t (:slant italic))))
   '(font-latex-bold-face ((t (:weight bold))))
   '(font-latex-sedate-face ((t ())))
   '(font-latex-string-face ((t (:weight bold))))
   '(font-latex-math-face ((t ())))
   '(font-latex-subscript-face ((t ())))
   '(font-latex-italic-face ((t (:slant italic))))
   '(font-latex-superscript-face ((t ())))
   '(font-latex-script-char-face ((t ())))
   '(font-latex-sectioning-0-face ((t ())))
   '(font-latex-sectioning-1-face ((t ())))
   '(font-latex-sectioning-2-face ((t ())))
   '(font-latex-sectioning-3-face ((t ())))
   '(font-latex-sectioning-4-face ((t ())))
   '(font-latex-sectioning-5-face ((t ())))
   `(helm-header ((t (:background ,nord1))))
   `(helm-source-header ((t (:background ,nord1))))
   '(helm-match ((t (:weight bold))))
   '(helm-grep-match ((t (:weight bold))))
   `(helm-candidate-number ((t ())))
   `(helm-grep-running ((t ())))
   `(helm-selection ((t (:background ,nord4 :foreground ,nord0))))
   '(helm-ff-prefix ((t ())))
   '(helm-ff-directory ((t (:weight bold))))
   '(helm-ff-dotted-directory ((t (:weight bold))))
   `(variable-pitch ((t ())))
   `(highlight ((t (:background ,nord3))))
   '(compilation-warning ((t (:weight bold :underline t))))
   '(compilation-column-number ((t (:underline t))))
   `(company-tooltip ((t (:foreground ,nord0 :background ,nord5))))
   `(company-scrollbar-fg ((t (:background ,nord0))))
   `(company-scrollbar-bg ((t (:background ,nord4))))
   `(company-tooltip-selection ((t (:background ,nord3 :foreground ,nord6))))
   `(company-tooltip-annotation ((t (:foreground ,nord3))))
   `(company-tooltip-annotation-selection ((t (:foreground ,nord5))))
   '(company-tooltip-common ((t (:weight bold))))
   `(company-preview-common ((t ())))
   `(company-preview ((t (:background ,nord4 :foreground ,nord0))))
   `(sh-heredoc ((t (:foreground ,nord7 :weight bold))))
   `(sh-quoted-exec ((t ())))
   '(tuareg-font-lock-governing-face ((t ())))
   '(tuareg-font-lock-operator-face ((t ())))
   '(tuareg-font-double-colon-face ((t ())))
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))
               )
  )

(provide-theme 'nordless)
(provide 'nordless-theme)

;;; nordless-theme.el ends here
