;;; brutalist-build.el --- Brutalist theme builder

;; Copyright (C) 2013-2016 Marian Schubert
;; Copyright (C) 2018 Gergely Nagy

;; Author: Gergely Nagy
;; Version: 0.1
;; URL: https://git.madhouse-project.org/algernon/brutalist-theme.el

;; This program is free software; you can redistribute it and/or modify
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

;; Low distraction, minimalistic color theme, with minimal colors, preferring
;; other styles of markup (italic, bold).

;;; Credits:

;; Based on:
;; http://github.com/maio/eink-emacs

;; Which in turn was inspired by:
;;
;; https://bitbucket.org/kisom/eink.vim
;; https://github.com/dmand/eink.el
;; http://www.daveliepmann.stfi.re/tufte-css/?sf=wklwy

;;; Code:

(defmacro brutalist-build-theme-with-colors (brutalist-colors &rest body)
  "Build a theme with a given set of BRUTALIST-COLORS.  Wraps BODY in a let."
  (declare (indent defun))
  `(let ,brutalist-colors
     ,@body))

(defun brutalist-build-custom-theme (theme-name)
  "Build a custom brutalist theme named THEME-NAME.  Colors are expected to specified by wrapping the call to this function in a let."
  (custom-theme-set-faces
   theme-name

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground ,cursor))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t :slant italic))))
   `(italic ((t (:slant italic))))
   `(error ((t (:inherit fixed-pitch :foreground ,fg :weight bold))))
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground ,string))))
   `(font-lock-builtin-face ((t (:inherit fixed-pitch :foreground ,fg))))
   `(font-lock-comment-face ((t (:inherit fixed-pitch :foreground ,fg-dim))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:inherit fixed-pitch :foreground ,fg))))
   `(font-lock-doc-face ((t (:inherit fixed-pitch :foreground ,fg-dim :slant italic))))
   `(font-lock-function-name-face ((t (::inherit fixed-pitch foreground ,fg))))
   `(font-lock-keyword-face ((t (:inherit fixed-pitch :foreground ,fg :weight bold))))
   `(font-lock-preprocessor-face ((t (:inherit fixed-pitch :foreground ,fg :slant italic))))
   `(font-lock-reference-face ((t (:inherit fixed-pitch :foreground ,fg))))
   `(font-lock-string-face ((t (:inherit fixed-pitch :foreground ,string))))
   `(font-lock-type-face ((t (:inherit fixed-pitch :foreground ,fg :underline t))))
   `(font-lock-variable-name-face ((t (:inherit fixed-pitch :foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:inherit error))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(highlight ((t nil)))
   `(lazy-highlight ((t (:background ,bg-highlight-3 :slant italic))))
   `(hl-line ((t (:inverse-video t))))
   `(ido-first-match ((t (:foreground ,fg))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:background ,bg-highlight-dim :foreground ,fg))))
   `(link ((t (:foreground ,string :underline t :inherit fixed-pitch))))
   `(minibuffer-prompt ((t (:inherit fixed-pitch :foreground ,fg :weight bold))))
   `(mode-line ((t (:inherit fixed-pitch :background ,bg-light :foreground ,fg :height 0.8))))
   `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   `(mode-line-inactive ((t (:background ,bg-light :foreground ,bg-light :height 0.8))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(modeline ((t (:background ,bg :foreground ,fg :height 0.8))))
   `(region ((t (:background ,bg-highlight :foreground ,fg :inverse-video nil))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,bg-highlight-2 :foreground ,fg))))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:inherit fixed-pitch :foreground ,fg))))
   `(org-block-begin-line ((t (:inherit fixed-pitch :foreground ,fg-light :height 0.8))))
   `(org-block-end-line ((t (:inherit fixed-pitch :foreground ,fg-light :height 0.8))))
   `(org-verbatim ((t (:inherit fixed-pitch :foreground ,fg :weight semi-bold))))
   `(org-date ((t (:inherit fixed-pitch :foreground ,fg) :underline)))
   `(org-done ((t (:inherit fixed-pitch :foreground ,fg-light))))
   `(org-hide ((t (:inherit fixed-pitch :foreground ,bg))))
   ;; use :overline to give headings more top margin
   `(org-level-1 ((t (:foreground ,fg :weight semi-bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-3 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-4 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-5 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-6 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-link ((t (:foreground ,fg :underline t))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:inherit fixed-pitch :foreground ,fg))))
   `(org-todo ((t (:inherit fixed-pitch :foreground ,fg))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-table ((t (:inherit fixed-pitch :foreground ,fg-table))))
   `(org-checkbox ((t (:inherit fixed-pitch :inherit bold))))
   `(org-tag ((t (:inherit fixed-pitch :inherit bold))))

   ;; powerline
   `(powerline-active1 ((t (:background ,powerline1 :foreground ,fg-light :inherit mode-line))))
   `(powerline-active2 ((t (:background ,powerline2 :foreground ,fg-light :inherit mode-line))))

   ;; message-mode, notmuch and gnus
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:inherit fixed-pitch :foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(message-header-name ((t (:inherit fixed-pitch :foreground ,fg :weight semi-bold))))
   `(message-header-other ((t (:foreground ,fg))))
   `(message-header-to ((t (:inherit message-header-other))))
   `(message-header-cc ((t (:inherit message-header-other))))
   `(message-header-subject ((t (:inherit message-header-other :foreground ,fg-table))))
   `(message-mml ((t (:inherit fixed-pitch :box (:line-width 1 :color ,fg-slight-dim)))))
   `(message-cited-text ((t (:foreground ,fg-dim))))
   `(message-separator ((t (:inherit fixed-pitch :foreground ,fg-slight-dim))))
   `(notmuch-message-summary-face ((t (:inherit fixed-pitch :foreground ,fg))))
   `(notmuch-tree-match-date-face ((t (:inherit fixed-pitch))))
   `(notmuch-tree-match-author-face ((t (:inherit fixed-pitch :foreground ,fg))))
   `(notmuch-tree-match-tag-face ((t (:inherit fixed-pitch :foreground ,fg :weight normal))))
   `(notmuch-tree-match-tree-face ((t (:inherit fixed-pitch))))
   `(notmuch-tree-match-subject-face ((t (:inherit fixed-pitch))))
   `(notmuch-tree-no-match-date-face ((t (:inherit notmuch-tree-match-date-face :foreground ,fg-dim))))
   `(notmuch-tree-no-match-author-face ((t (:inherit notmuch-tree-match-author-face :foreground ,fg-dim))))
   `(notmuch-tree-no-match-subject-face ((t (:inherit notmuch-tree-match-subject-face :foreground ,fg-dim))))
   `(notmuch-tree-no-match-tree-face ((t (:inherit notmuch-tree-match-tree-face :foreground ,fg-dim))))
   `(notmuch-tree-no-match-tag-face ((t (:inherit notmuch-tree-match-tag-face :foreground ,fg-dim))))
   `(notmuch-wash-toggle-button ((t (:inherit message-mml))))
   `(notmuch-tag-flagged ((t (:inherit default))))

   ;; magit
   `(magit-header ((t (:inherit fixed-pitch :weight semi-bold))))
   `(magit-item-mark ((t (:inherit fixed-pitch :background ,bg-highlight))))
   `(magit-item-highlight ((t (:inherit fixed-pitch :weight normal))))
   `(magit-diff-context-highlight ((t (:inherit fixed-pitch :foreground ,fg))))
   `(magit-branch-local ((t (:inherit magit-head))))
   `(magit-branch-remote ((t (:inherit magit-head))))
   `(magit-branch-current ((t (:inherit magit-head))))
   `(magit-branch-remote-head ((t (:inherit magit-head))))
   `(magit-tag ((t (:inherit magit-branch-local :underline nil))))
   `(magit-hash ((t (:inherit default :weight semi-bold))))
   `(magit-head ((t (:inherit fixed-pitch :foreground ,fg-table :underline t :weight bold))))
   `(magit-log-author ((t (:inherit fixed-pitch :foreground ,fg-dim))))
   `(magit-section-heading ((t (:inherit fixed-pitch
                                         :weight semi-bold
                                         :underline t
                                         :height 1.2))))
   `(magit-header-line ((t (:inherit fixed-pitch))))
   `(magit-header-line-key ((t (:inherit fixed-pitch))))
   `(magit-section-highlight ((t (:fg ,fg))))

   `(git-commit-comment-branch ((t (:inherit magit-head))))
   `(git-commit-known-pseudo-header ((t (:inherit font-lock-keyword-face
                                                  :box (:line-width 1 :color ,fg-slight-dim)))))
   `(git-commit-summary ((t (:foreground ,fg :weight bold))))

   ;; diff
   `(diff-added ((t (:background ,diff-added))))
   `(diff-removed ((t (:background ,diff-removed))))
   `(diff-refine-added ((t (:background ,diff-added-highlight))))
   `(diff-refine-removed ((t (:background ,diff-removed-highlight))))
   `(magit-diff-added-highlight ((t (:weight demibold :background ,diff-added))))
   `(magit-diff-added ((t (:background ,diff-added))))
   `(magit-diff-removed-highlight ((t (:weight demibold :background ,diff-removed))))
   `(magit-diff-removed ((t (:background ,diff-removed))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-author-face ((t (:inherit default))))
   `(git-timemachine-minibuffer-detail-face ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:weight bold))))
   `(dired-subtree-depth-1-face ((t (:inherit default))))
   `(dired-subtree-depth-2-face ((t (:inherit default))))
   `(dired-subtree-depth-3-face ((t (:inherit default))))
   `(dired-subtree-depth-4-face ((t (:inherit default))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background ,bg-light :weight bold))))
   `(helm-header ((t (:foreground ,fg))))
   `(helm-selection-line ((t (:inherit region :weight bold))))
   `(helm-selection ((t (:inherit hl-line))))
   `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg-highlight))))

   ;; parens - parenface
   `(parenface-paren-face ((t (:foreground ,fg-slight-dim))))
   `(parenface-curly-face ((t (:foreground ,fg-slight-dim))))
   `(parenface-bracket-face ((t (:foreground ,fg-slight-dim))))

   ;; parens - paren-face
   `(parenthesis ((t (:foreground ,fg-slight-dim))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground ,paren-match :weight bold))))
   `(sp-show-pair-mismatch-face ((t (:background ,paren-mismatch :foreground ,paren-match :weight bold))))
   `(show-paren-match ((t (:foreground ,paren-match :weight bold))))
   `(show-paren-mismatch ((t (:background ,paren-mismatch :foreground ,paren-match :weight bold))))

   ;; sh
   `(sh-heredoc ((t (:foreground ,fg :slant italic))))
   `(sh-quoted-exec ((t (:inherit font-lock-string-face :weight semi-bold))))

   ;; js2
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-external-variable ((t (:foreground ,fg))))
   `(js2-jsdoc-type ((t (:inherit font-lock-type-face))))
   `(js2-jsdoc-value ((t (:inherit font-lock-variable-name-face))))
   `(js2-jsdoc-html-tag-name ((t (:inherit web-mode-html-tag-face))))

   ;; perl
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; linum / nlinum-relative
   `(nlinum-relative-current-face ((t (:inherit normal :weight bold))))
   `(linum ((t (:inherit normal :weight bold))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((t (:inherit normal :weight bold :foreground ,fg))))

   ;; markdown-mode
   `(markdown-blockquote-face ((t (:foreground ,fg-table :slant normal))))
   `(markdown-inline-code-face ((t (:inherit fixed-pitch :slant italic))))
   `(markdown-pre-face ((t (:inherit markdown-inline-code-face))))
   `(markdown-reference-face ((t (:inherit markdown-link-face))))
   `(markdown-url-face ((t (:inherit markdown-link-face))))
   `(markdown-link-face ((t (:inherit fixed-pitch :inherit link))))
   `(markdown-metadata-key-face ((t (:inherit fixed-pitch))))
   `(markdown-markup-face ((t (:inherit fixed-pitch :foreground ,fg-dim))))
   `(markdown-metadata-value-face ((t (:inherit fixed-pitch :foreground ,string))))
   `(markdown-html-tag-name-face ((t (:inherit fixed-pitch :foreground ,fg-dim))))
   `(markdown-html-attr-name-face ((t (:inherit fixed-pitch :foreground ,fg-dim))))

   ;; misc
   `(idle-highlight ((t (:background ,bg-highlight))))
   `(yas-field-highlight-face ((t (:background ,bg-highlight-dim :foreground ,fg))))
   `(cider-result-overlay-face ((t (:weight bold))))

   ;; eshell
   `(epe-dir-face ((t (:inherit fixed-pitch :foreground ,string))))
   `(eshell-ls-directory ((t (:inherit fixed-pitch :inherit link))))
   `(eshell-ls-product ((t (:inherit fixed-pitch :foreground ,fg-dim))))
   `(eshell-ls-archive ((t (:inherit fixed-pitch :slant italic))))
   `(eshell-ls-symlink ((t (:inherit fixed-pitch :underline t))))
   `(eshell-ls-executable ((t (:inherit fixed-pitch :weight bold))))
   `(eshell-prompt ((t (:inherit fixed-pitch :foreground ,fg :weight bold))))

   ;; evil-quickscope
   `(evil-quickscope-first-face ((t (:foreground ,fg :background ,bg-highlight-dim))))
   `(evil-quickscope-second-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((t (:foreground ,fg :background ,bg-highlight-dim))))
   `(evil-snipe-matches-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-goggles
   `(evil-goggles-delete-face ((t (:inherit diff-removed))))
   `(evil-goggles-paste-face ((t (:inherit diff-added))))
   `(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
   `(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
   `(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
   `(evil-goggles-yank-face ((t (:inherit diff-changed))))

   ;; evil
   `(evil-ex-lazy-highlight ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-matches ((t (:background ,diff-removed-highlight))))
   `(evil-ex-substitute-replacement ((t (:background ,diff-added-highlight :underline nil :foreground ,fg))))

   ;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face ((t (:background ,bg-light))))))

(provide 'brutalist-build)

;;; brutalist-build.el ends here

;;; brutalist-dark-theme.el --- Brutalist theme, dark variant

;; Copyright (C) 2013-2016 Marian Schubert
;; Copyright (C) 2018 Gergely Nagy

;; Author: Gergely Nagy
;; Version: 0.1
;; URL: https://git.madhouse-project.org/algernon/brutalist-theme.el

;; This program is free software; you can redistribute it and/or modify
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

;; Low distraction, minimalistic color theme, with minimal colors, preferring
;; other styles of markup (italic, bold).

;;; Credits:

;; Based on:
;; http://github.com/maio/eink-emacs

;; Which in turn was inspired by:
;;
;; https://bitbucket.org/kisom/eink.vim
;; https://github.com/dmand/eink.el
;; http://www.daveliepmann.stfi.re/tufte-css/?sf=wklwy

;;; Code:

(require 'brutalist-build)

(deftheme brutalist-dark
  "Minimal, low-color, low-distraction theme (dark variant).")

(brutalist-build-theme-with-colors
  ((fg "#eeeee8")
   (fg-table "light blue")
   (fg-dim "gray70")
   (fg-slight-dim "dim gray")
   (bg "gray20")
   (bg-light "#888888")
   (fg-light "#ddddd8")
   (bg-highlight "grey40")
   (bg-highlight-2 "LightCyan")
   (bg-highlight-3 "DarkGreen")
   (bg-highlight-dim "sea green")

   (diff-added "#334433")
   (diff-added-highlight "#336633")
   (diff-removed "#443333")
   (diff-removed-highlight "#883333")

   (powerline1 "gray50")
   (powerline2 "gray60")

   (string "light blue")
   (cursor "white smoke")

   (paren-match "#aaccff")
   (paren-mismatch "red"))

  (brutalist-build-custom-theme 'brutalist-dark))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'brutalist-dark)
(provide 'brutalist-dark-theme)

;;; brutalist-dark-theme.el ends here
