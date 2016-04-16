;;; github-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2016 Philip Arvidsson

;; Author: Philip Arvidsson <contact@philiparvidsson.com>
;; URL: https://github.com/philiparvidsson/emacs-github-theme
;; Version: 2.3-cvs

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

;; I took the Zenburn theme and modified it. Creds to Bozhidar Batsov!

;;; Credits:

;; Bozhidar Batsov created the Zenburn theme file which I modified to create
;; this theme.

;;; Code:

(deftheme github "The GitHub color theme")

;;; Color Palette

(defvar github-default-colors-alist
  '(("github-fg+1"     . "#333333")
    ("github-fg"       . "#a71d5d")
    ("github-fg-1"     . "#333333")
    ("github-bg-2"     . "#ffffff") ;; homerow inactive bg
    ("github-bg-1"     . "#b0cde7") ;; selection
    ("github-bg-05"    . "#f8eec7") ;; line highlight
    ("github-bg"       . "#ffffff")
    ("github-bg+05"    . "#ffffff")
    ("github-bg+1"     . "#ffffff")
    ("github-bg+2"     . "#ffffff")
    ("github-bg+3"     . "#ffffff")
    ("github-red+1"    . "#333333")
    ("github-red"      . "#183691") ;; strings
    ("github-red-1"    . "#333333")
    ("github-red-2"    . "#333333")
    ("github-red-3"    . "#333333")
    ("github-red-4"    . "#333333")
    ("github-orange"   . "#333333") ;; identifiers, html attributes
    ("github-yellow"   . "#a71d5d") ;; keywords (homerow active bg)
    ("github-yellow-1" . "#333333")
    ("github-yellow-2" . "#333333")
    ("github-green-1"  . "#969896") ;; comments
    ("github-green"    . "#969896") ;; comments
    ("github-green+1"  . "#333333") ;; homerow
    ("github-green+2"  . "#969896") ;; line numbers
    ("github-green+3"  . "#63a35c") ;; html tags
    ("github-green+4"  . "#0086b3") ;; constants
    ("github-cyan"     . "#795da3") ;; function names, html attrs
    ("github-blue+1"   . "#a71d5d") ;; preprocessor keywords
    ("github-blue"     . "#333333")
    ("github-blue-1"   . "#0086b3") ;; types
    ("github-blue-2"   . "#333333")
    ("github-blue-3"   . "#333333")
    ("github-blue-4"   . "#333333")
    ("github-blue-5"   . "#333333")
    ("github-magenta"  . "#333333"))
  "List of GitHub colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar github-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar github-colors-alist
  (append github-default-colors-alist github-override-colors-alist))

(defmacro github-with-color-variables (&rest body)
  "`let' bind all colors defined in `github-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   github-colors-alist))
     ,@body))

;;; Theme Faces
(github-with-color-variables
  (custom-theme-set-faces
   'github
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,github-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,github-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,github-fg+1 :background ,github-bg))))
   `(cursor ((t (:foreground ,github-fg :background ,github-fg+1))))
   `(escape-glyph ((t (:foreground ,github-yellow :bold t))))
   `(fringe ((t (:foreground ,github-fg :background ,github-bg+1))))
   `(header-line ((t (:foreground ,github-yellow
                                  :background ,github-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,github-bg-05))))
   `(success ((t (:foreground ,github-green :weight bold))))
   `(warning ((t (:foreground ,github-orange :weight bold))))
   `(tooltip ((t (:foreground ,github-fg :background ,github-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,github-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,github-green))))
   `(compilation-error-face ((t (:foreground ,github-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,github-fg))))
   `(compilation-info-face ((t (:foreground ,github-blue))))
   `(compilation-info ((t (:foreground ,github-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,github-green))))
   `(compilation-line-face ((t (:foreground ,github-yellow))))
   `(compilation-line-number ((t (:foreground ,github-yellow))))
   `(compilation-message-face ((t (:foreground ,github-blue))))
   `(compilation-warning-face ((t (:foreground ,github-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,github-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,github-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,github-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,github-fg-1))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,github-fg))))
   `(grep-error-face ((t (:foreground ,github-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,github-blue))))
   `(grep-match-face ((t (:foreground ,github-orange :weight bold))))
   `(match ((t (:background ,github-bg-1 :foreground ,github-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,github-yellow-2 :weight bold :background ,github-bg+2))))
   `(isearch-fail ((t (:foreground ,github-fg :background ,github-red-4))))
   `(lazy-highlight ((t (:foreground ,github-yellow-2 :weight bold :background ,github-bg-05))))

   `(menu ((t (:foreground ,github-fg :background ,github-bg))))
   `(minibuffer-prompt ((t (:foreground ,github-yellow))))
   `(mode-line
     ((,class (:foreground ,github-bg
                           :background ,github-yellow ;; homerow background
                           ;;:box (:line-width -1 :style released-button)))
                           ))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,"#000000" :weight bold)))) ;; todo: not happy with black here
   `(mode-line-inactive
     ((t (:foreground ,github-green-1
                      :background ,github-bg
                      :box (:line-width -1 :color "#d0d0d0")))))
   `(region ((,class (:background ,github-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,github-bg+2))))
   `(trailing-whitespace ((t (:background ,github-red))))
   `(vertical-border ((t (:foreground ,"#d0d0d0"))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,github-fg :weight normal))))  ;; weight bold (built-in keywords)
   `(font-lock-comment-face ((t (:foreground ,github-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,github-green-1))))
   `(font-lock-constant-face ((t (:foreground ,github-green+4))))
   `(font-lock-doc-face ((t (:foreground ,github-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,github-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,github-yellow :weight normal)))) ;; weight bold (keywords)
   `(font-lock-negation-char-face ((t (:foreground ,github-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,github-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,github-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,github-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,github-red))))
   `(font-lock-type-face ((t (:foreground ,github-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,github-orange))))
   `(font-lock-warning-face ((t (:foreground ,github-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,github-fg))))
   `(newsticker-default-face ((t (:foreground ,github-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,github-green+3))))
   `(newsticker-extra-face ((t (:foreground ,github-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,github-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,github-green))))
   `(newsticker-new-item-face ((t (:foreground ,github-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,github-red))))
   `(newsticker-old-item-face ((t (:foreground ,github-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,github-fg))))
   `(newsticker-treeview-face ((t (:foreground ,github-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,github-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,github-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,github-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,github-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,github-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,github-bg-1 :foreground ,github-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,github-fg-1 :background ,github-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,github-green+2 :background ,github-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,github-green+1))))
   `(android-mode-error-face ((t (:foreground ,github-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,github-fg))))
   `(android-mode-verbose-face ((t (:foreground ,github-green))))
   `(android-mode-warning-face ((t (:foreground ,github-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,github-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,github-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,github-yellow))))
   `(font-latex-italic-face ((t (:foreground ,github-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,github-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,github-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,github-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,github-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,github-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,github-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,github-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,github-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,github-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,github-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,github-bg :background ,github-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,github-bg :background ,github-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,github-bg :background ,github-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,github-bg :background ,github-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,github-bg :background ,github-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,github-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,github-bg+3 :foreground ,github-bg-2))))
   `(ac-selection-face ((t (:background ,github-blue-4 :foreground ,github-fg))))
   `(popup-tip-face ((t (:background ,github-yellow-2 :foreground ,github-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,github-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,github-bg-1))))
   `(popup-isearch-match ((t (:background ,github-bg :foreground ,github-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,github-fg-1 :background ,github-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,github-green+3 :background ,github-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,github-yellow :background ,github-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,github-red+1 :background ,github-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,github-cyan :background ,github-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,github-fg :background ,github-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,github-orange :background ,github-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,github-orange :background ,github-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,github-fg :background ,github-bg-1))))
   `(company-tooltip-mouse ((t (:background ,github-bg-1))))
   `(company-tooltip-common ((t (:foreground ,github-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,github-green+2))))
   `(company-scrollbar-fg ((t (:background ,github-bg-1))))
   `(company-scrollbar-bg ((t (:background ,github-bg+2))))
   `(company-preview ((t (:background ,github-green+2))))
   `(company-preview-common ((t (:foreground ,github-green+2 :background ,github-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,github-yellow-1 :foreground ,github-bg))))
   `(bm-fringe-face ((t (:background ,github-yellow-1 :foreground ,github-bg))))
   `(bm-fringe-persistent-face ((t (:background ,github-green-1 :foreground ,github-bg))))
   `(bm-persistent-face ((t (:background ,github-green-1 :foreground ,github-bg))))
;;;;; cider
   `(cider-result-overlay-face ((t (:foreground ,github-fg-1 :background unspecified))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,github-cyan))))
   `(circe-my-message-face ((t (:foreground ,github-fg))))
   `(circe-fool-face ((t (:foreground ,github-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,github-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,github-fg))))
   `(circe-server-face ((t (:foreground ,github-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,github-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,github-orange :background ,github-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,github-fg)))
   `(context-coloring-level-1-face ((t :foreground ,github-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,github-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,github-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,github-orange)))
   `(context-coloring-level-5-face ((t :foreground ,github-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,github-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,github-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,github-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,github-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,github-blue :foreground ,github-bg))))
   `(ctbl:face-continue-bar ((t (:background ,github-bg-05 :foreground ,github-bg))))
   `(ctbl:face-row-select ((t (:background ,github-cyan :foreground ,github-bg))))
;;;;; diff
   `(diff-added          ((t (:background "#335533" :foreground ,github-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,github-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,github-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,github-green+4))))
   `(diff-refine-change  ((t (:background "#888811" :foreground ,github-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,github-red))))
   `(diff-header ((,class (:background ,github-bg+2))
                  (t (:background ,github-fg :foreground ,github-bg))))
   `(diff-file-header
     ((,class (:background ,github-bg+2 :foreground ,github-fg :bold t))
      (t (:background ,github-fg :foreground ,github-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,github-blue :background ,github-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,github-red+1 :background ,github-red-1))))
   `(diff-hl-insert ((,class (:foreground ,github-green+1 :background ,github-green-1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,github-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,github-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,github-orange))))
   `(diredp-date-time ((t (:foreground ,github-magenta))))
   `(diredp-deletion ((t (:foreground ,github-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,github-red))))
   `(diredp-dir-heading ((t (:foreground ,github-blue :background ,github-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,github-cyan))))
   `(diredp-exec-priv ((t (:foreground ,github-red))))
   `(diredp-executable-tag ((t (:foreground ,github-green+1))))
   `(diredp-file-name ((t (:foreground ,github-blue))))
   `(diredp-file-suffix ((t (:foreground ,github-green))))
   `(diredp-flag-mark ((t (:foreground ,github-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,github-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,github-red))))
   `(diredp-link-priv ((t (:foreground ,github-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,github-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,github-orange))))
   `(diredp-no-priv ((t (:foreground ,github-fg))))
   `(diredp-number ((t (:foreground ,github-green+1))))
   `(diredp-other-priv ((t (:foreground ,github-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,github-red-1))))
   `(diredp-read-priv ((t (:foreground ,github-green-1))))
   `(diredp-symlink ((t (:foreground ,github-yellow))))
   `(diredp-write-priv ((t (:foreground ,github-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,github-fg :background ,github-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,github-fg :background ,github-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,github-fg :background ,github-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,github-fg :background ,github-blue-5))))
   `(ediff-even-diff-A ((t (:background ,github-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,github-bg+1))))
   `(ediff-even-diff-B ((t (:background ,github-bg+1))))
   `(ediff-even-diff-C ((t (:background ,github-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,github-fg :background ,github-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,github-fg :background ,github-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,github-fg :background ,github-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,github-fg :background ,github-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,github-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,github-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,github-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,github-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,github-fg))))
   `(egg-help-header-1 ((t (:foreground ,github-yellow))))
   `(egg-help-header-2 ((t (:foreground ,github-green+3))))
   `(egg-branch ((t (:foreground ,github-yellow))))
   `(egg-branch-mono ((t (:foreground ,github-yellow))))
   `(egg-term ((t (:foreground ,github-yellow))))
   `(egg-diff-add ((t (:foreground ,github-green+4))))
   `(egg-diff-del ((t (:foreground ,github-red+1))))
   `(egg-diff-file-header ((t (:foreground ,github-yellow-2))))
   `(egg-section-title ((t (:foreground ,github-yellow))))
   `(egg-stash-mono ((t (:foreground ,github-green+4))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,github-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,github-green))))
   `(elfeed-search-feed-face ((t (:foreground ,github-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,github-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,github-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,github-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,github-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,github-green+2 :background ,github-bg))))
   `(w3m-lnum-match ((t (:background ,github-bg-1
                                     :foreground ,github-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,github-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,github-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,github-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,github-yellow))))
   `(erc-keyword-face ((t (:foreground ,github-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,github-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,github-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,github-green))))
   `(erc-pal-face ((t (:foreground ,github-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,github-orange :background ,github-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,github-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,github-green+4 :background ,github-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,github-red :background ,github-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,github-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,github-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,github-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,github-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,github-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,github-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,github-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,github-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-red-1) :inherit unspecified))
      (t (:foreground ,github-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-yellow) :inherit unspecified))
      (t (:foreground ,github-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-cyan) :inherit unspecified))
      (t (:foreground ,github-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,github-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,github-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,github-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-orange) :inherit unspecified))
      (t (:foreground ,github-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-red) :inherit unspecified))
      (t (:foreground ,github-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,github-fg))))
   `(ack-file ((t (:foreground ,github-blue))))
   `(ack-line ((t (:foreground ,github-yellow))))
   `(ack-match ((t (:foreground ,github-orange :background ,github-bg-1 :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,github-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,github-blue+1  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,github-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,github-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,github-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,github-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,github-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,github-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,github-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,github-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, github-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,github-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,github-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,github-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,github-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,github-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,github-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,github-blue))))
   `(gnus-summary-high-read ((t (:foreground ,github-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,github-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,github-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,github-blue))))
   `(gnus-summary-low-read ((t (:foreground ,github-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,github-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,github-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,github-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,github-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,github-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,github-fg))))
   `(gnus-summary-selected ((t (:foreground ,github-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,github-blue))))
   `(gnus-cite-10 ((t (:foreground ,github-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,github-yellow))))
   `(gnus-cite-2 ((t (:foreground ,github-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,github-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,github-green+2))))
   `(gnus-cite-5 ((t (:foreground ,github-green+1))))
   `(gnus-cite-6 ((t (:foreground ,github-green))))
   `(gnus-cite-7 ((t (:foreground ,github-red))))
   `(gnus-cite-8 ((t (:foreground ,github-red-1))))
   `(gnus-cite-9 ((t (:foreground ,github-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,github-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,github-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,github-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,github-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,github-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,github-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,github-bg+2))))
   `(gnus-signature ((t (:foreground ,github-yellow))))
   `(gnus-x ((t (:background ,github-fg :foreground ,github-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,github-blue))))
   `(guide-key/key-face ((t (:foreground ,github-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,github-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,github-green
                      :background ,github-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,github-yellow
                      :background ,github-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,github-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,github-bg+1))))
   `(helm-visible-mark ((t (:foreground ,github-bg :background ,github-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,github-green+4 :background ,github-bg-1))))
   `(helm-separator ((t (:foreground ,github-red :background ,github-bg))))
   `(helm-time-zone-current ((t (:foreground ,github-green+2 :background ,github-bg))))
   `(helm-time-zone-home ((t (:foreground ,github-red :background ,github-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,github-orange :background ,github-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,github-magenta :background ,github-bg))))
   `(helm-bookmark-info ((t (:foreground ,github-green+2 :background ,github-bg))))
   `(helm-bookmark-man ((t (:foreground ,github-yellow :background ,github-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,github-magenta :background ,github-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,github-red :background ,github-bg))))
   `(helm-buffer-process ((t (:foreground ,github-cyan :background ,github-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,github-fg :background ,github-bg))))
   `(helm-buffer-size ((t (:foreground ,github-fg-1 :background ,github-bg))))
   `(helm-ff-directory ((t (:foreground ,github-cyan :background ,github-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,github-fg :background ,github-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,github-green+2 :background ,github-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,github-red :background ,github-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,github-yellow :background ,github-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,github-bg :background ,github-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,github-cyan :background ,github-bg))))
   `(helm-grep-file ((t (:foreground ,github-fg :background ,github-bg))))
   `(helm-grep-finish ((t (:foreground ,github-green+2 :background ,github-bg))))
   `(helm-grep-lineno ((t (:foreground ,github-fg-1 :background ,github-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,github-red :background ,github-bg))))
   `(helm-match ((t (:foreground ,github-orange :background ,github-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,github-cyan :background ,github-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,github-fg-1 :background ,github-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,github-fg :background ,github-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,github-fg :background ,github-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,github-yellow :background ,github-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,github-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,github-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,github-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,github-red-1 :background ,github-bg))))
   `(hydra-face-amaranth ((t (:foreground ,github-red-3 :background ,github-bg))))
   `(hydra-face-blue ((t (:foreground ,github-blue :background ,github-bg))))
   `(hydra-face-pink ((t (:foreground ,github-magenta :background ,github-bg))))
   `(hydra-face-teal ((t (:foreground ,github-cyan :background ,github-bg))))
;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,github-green :background ,github-bg))))
   `(ivy-match-required-face ((t (:foreground ,github-red :background ,github-bg))))
   `(ivy-remote ((t (:foreground ,github-blue :background ,github-bg))))
   `(ivy-subdir ((t (:foreground ,github-yellow :background ,github-bg))))
   `(ivy-current-match ((t (:foreground ,github-yellow :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,github-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,github-green-1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,github-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,github-green+1))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,github-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,github-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,github-yellow))))
   `(ido-indicator ((t (:foreground ,github-yellow :background ,github-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,github-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,github-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,github-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,github-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,github-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,github-orange))))
   `(jabber-roster-user-error ((t (:foreground ,github-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,github-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,github-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,github-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,github-green+3))))
   `(jabber-activity-face((t (:foreground ,github-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,github-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,github-orange))))
   `(js2-error ((t (:foreground ,github-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,github-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,github-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,github-green+3))))
   `(js2-function-param ((t (:foreground, github-orange))))
   `(js2-external-variable ((t (:foreground ,github-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,github-green-1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,github-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,github-red-1))))
   `(js2-object-property ((t (:foreground ,github-blue+1))))
   `(js2-magic-paren ((t (:foreground ,github-blue-5))))
   `(js2-private-function-call ((t (:foreground ,github-cyan))))
   `(js2-function-call ((t (:foreground ,github-cyan))))
   `(js2-private-member ((t (:foreground ,github-blue-1))))
   `(js2-keywords ((t (:foreground ,github-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,github-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,github-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,github-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,github-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,github-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,github-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,github-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,github-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,github-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,github-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,github-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,github-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,github-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,github-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,github-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,github-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,github-green+2 :background ,github-bg))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,github-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,github-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,github-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,github-green+2 :background ,github-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,github-blue-1))))
   `(lui-hilight-face ((t (:foreground ,github-green+2 :background ,github-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,github-green+2 :background ,github-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,github-red+1 :background ,github-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,github-blue+1 :background ,github-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,github-magenta :background ,github-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,github-yellow :background ,github-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,github-bg+05))))
   `(magit-section-heading             ((t (:foreground ,github-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,github-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,github-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,github-bg+05
                                                        :foreground ,github-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,github-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,github-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,github-bg+2
                                                        :foreground ,github-orange))))
   `(magit-diff-lines-heading          ((t (:background ,github-orange
                                                        :foreground ,github-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,github-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,github-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,github-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,github-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,github-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,github-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,github-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,github-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,github-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,github-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,github-orange))))
   `(magit-log-date      ((t (:foreground ,github-fg-1))))
   `(magit-log-graph     ((t (:foreground ,github-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,github-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,github-green))))
   `(magit-sequence-part ((t (:foreground ,github-yellow))))
   `(magit-sequence-head ((t (:foreground ,github-blue))))
   `(magit-sequence-drop ((t (:foreground ,github-red))))
   `(magit-sequence-done ((t (:foreground ,github-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,github-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,github-green))))
   `(magit-bisect-skip ((t (:foreground ,github-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,github-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,github-bg-1 :foreground ,github-blue-2))))
   `(magit-blame-hash    ((t (:background ,github-bg-1 :foreground ,github-blue-2))))
   `(magit-blame-name    ((t (:background ,github-bg-1 :foreground ,github-orange))))
   `(magit-blame-date    ((t (:background ,github-bg-1 :foreground ,github-orange))))
   `(magit-blame-summary ((t (:background ,github-bg-1 :foreground ,github-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,github-bg+3))))
   `(magit-hash           ((t (:foreground ,github-bg+3))))
   `(magit-tag            ((t (:foreground ,github-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,github-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,github-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,github-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,github-blue   :weight bold))))
   `(magit-refname        ((t (:background ,github-bg+2 :foreground ,github-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,github-bg+2 :foreground ,github-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,github-bg+2 :foreground ,github-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,github-green))))
   `(magit-signature-bad       ((t (:foreground ,github-red))))
   `(magit-signature-untrusted ((t (:foreground ,github-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,github-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,github-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,github-green))))
   `(magit-reflog-amend        ((t (:foreground ,github-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,github-green))))
   `(magit-reflog-checkout     ((t (:foreground ,github-blue))))
   `(magit-reflog-reset        ((t (:foreground ,github-red))))
   `(magit-reflog-rebase       ((t (:foreground ,github-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,github-green))))
   `(magit-reflog-remote       ((t (:foreground ,github-cyan))))
   `(magit-reflog-other        ((t (:foreground ,github-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,github-green+1))))
   `(message-header-other ((t (:foreground ,github-green))))
   `(message-header-to ((t (:foreground ,github-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,github-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,github-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,github-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,github-green))))
   `(message-mml ((t (:foreground ,github-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,github-orange))))
   `(mew-face-header-from ((t (:foreground ,github-yellow))))
   `(mew-face-header-date ((t (:foreground ,github-green))))
   `(mew-face-header-to ((t (:foreground ,github-red))))
   `(mew-face-header-key ((t (:foreground ,github-green))))
   `(mew-face-header-private ((t (:foreground ,github-green))))
   `(mew-face-header-important ((t (:foreground ,github-blue))))
   `(mew-face-header-marginal ((t (:foreground ,github-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,github-red))))
   `(mew-face-header-xmew ((t (:foreground ,github-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,github-red))))
   `(mew-face-body-url ((t (:foreground ,github-orange))))
   `(mew-face-body-comment ((t (:foreground ,github-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,github-green))))
   `(mew-face-body-cite2 ((t (:foreground ,github-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,github-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,github-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,github-red))))
   `(mew-face-mark-review ((t (:foreground ,github-blue))))
   `(mew-face-mark-escape ((t (:foreground ,github-green))))
   `(mew-face-mark-delete ((t (:foreground ,github-red))))
   `(mew-face-mark-unlink ((t (:foreground ,github-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,github-green))))
   `(mew-face-mark-unread ((t (:foreground ,github-red-2))))
   `(mew-face-eof-message ((t (:foreground ,github-green))))
   `(mew-face-eof-part ((t (:foreground ,github-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,github-cyan :background ,github-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,github-bg :background ,github-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,github-bg :background ,github-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,github-blue))))
   `(mingus-pausing-face ((t (:foreground ,github-magenta))))
   `(mingus-playing-face ((t (:foreground ,github-cyan))))
   `(mingus-playlist-face ((t (:foreground ,github-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,github-yellow))))
   `(mingus-stopped-face ((t (:foreground ,github-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,github-yellow))))
   `(nav-face-button-num ((t (:foreground ,github-cyan))))
   `(nav-face-dir ((t (:foreground ,github-green))))
   `(nav-face-hdir ((t (:foreground ,github-red))))
   `(nav-face-file ((t (:foreground ,github-fg))))
   `(nav-face-hfile ((t (:foreground ,github-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,github-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,github-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,github-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,github-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,github-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,github-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,github-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,github-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,github-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,github-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,github-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,github-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,github-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,github-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,github-fg :weight bold))))
   `(org-checkbox ((t (:background ,github-bg+2 :foreground ,github-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,github-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,github-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,github-green+3))))
   `(org-formula ((t (:foreground ,github-yellow-2))))
   `(org-headline-done ((t (:foreground ,github-green+3))))
   `(org-hide ((t (:foreground ,github-bg-1))))
   `(org-level-1 ((t (:foreground ,github-orange))))
   `(org-level-2 ((t (:foreground ,github-green+4))))
   `(org-level-3 ((t (:foreground ,github-blue-1))))
   `(org-level-4 ((t (:foreground ,github-yellow-2))))
   `(org-level-5 ((t (:foreground ,github-cyan))))
   `(org-level-6 ((t (:foreground ,github-green+2))))
   `(org-level-7 ((t (:foreground ,github-red-4))))
   `(org-level-8 ((t (:foreground ,github-blue-4))))
   `(org-link ((t (:foreground ,github-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,github-green+4))))
   `(org-scheduled-previously ((t (:foreground ,github-red))))
   `(org-scheduled-today ((t (:foreground ,github-blue+1))))
   `(org-sexp-date ((t (:foreground ,github-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,github-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,github-orange))))
   `(org-todo ((t (:bold t :foreground ,github-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,github-red :weight bold :underline nil))))
   `(org-column ((t (:background ,github-bg-1))))
   `(org-column-title ((t (:background ,github-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,github-fg :background ,github-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,github-bg :background ,github-red-1))))
   `(org-ellipsis ((t (:foreground ,github-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,github-cyan :underline t))))
   `(org-document-title ((t (:foreground ,github-blue))))
   `(org-document-info ((t (:foreground ,github-blue))))
   `(org-habit-ready-face ((t :background ,github-green)))
   `(org-habit-alert-face ((t :background ,github-yellow-1 :foreground ,github-bg)))
   `(org-habit-clear-face ((t :background ,github-blue-3)))
   `(org-habit-overdue-face ((t :background ,github-red-3)))
   `(org-habit-clear-future-face ((t :background ,github-blue-4)))
   `(org-habit-ready-future-face ((t :background ,github-green-1)))
   `(org-habit-alert-future-face ((t :background ,github-yellow-2 :foreground ,github-bg)))
   `(org-habit-overdue-future-face ((t :background ,github-red-4)))
;;;;; outline
   `(outline-1 ((t (:foreground ,github-orange))))
   `(outline-2 ((t (:foreground ,github-green+4))))
   `(outline-3 ((t (:foreground ,github-blue-1))))
   `(outline-4 ((t (:foreground ,github-yellow-2))))
   `(outline-5 ((t (:foreground ,github-cyan))))
   `(outline-6 ((t (:foreground ,github-green+2))))
   `(outline-7 ((t (:foreground ,github-red-4))))
   `(outline-8 ((t (:foreground ,github-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,github-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,github-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,github-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,github-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,github-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,github-fg :background ,github-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,github-bg :background ,github-orange))))
   `(proof-error-face ((t (:foreground ,github-fg :background ,github-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,github-bg :background ,github-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,github-bg :background ,github-orange))))
   `(proof-locked-face ((t (:background ,github-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,github-bg :background ,github-orange))))
   `(proof-queue-face ((t (:background ,github-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,github-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,github-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,github-bg))))
   `(proof-warning-face ((t (:foreground ,github-bg :background ,github-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,github-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,github-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,github-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,github-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,github-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,github-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,github-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,github-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,github-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,github-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,github-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,github-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,github-blue))))
   `(rcirc-other-nick ((t (:foreground ,github-orange))))
   `(rcirc-bright-nick ((t (:foreground ,github-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,github-blue-2))))
   `(rcirc-server ((t (:foreground ,github-green))))
   `(rcirc-server-prefix ((t (:foreground ,github-green+1))))
   `(rcirc-timestamp ((t (:foreground ,github-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,github-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,github-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,github-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,github-green))))
   `(rpm-spec-doc-face ((t (:foreground ,github-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,github-red))))
   `(rpm-spec-macro-face ((t (:foreground ,github-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,github-red))))
   `(rpm-spec-package-face ((t (:foreground ,github-red))))
   `(rpm-spec-section-face ((t (:foreground ,github-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,github-blue))))
   `(rpm-spec-var-face ((t (:foreground ,github-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,github-orange))))
   `(rst-level-2-face ((t (:foreground ,github-green+1))))
   `(rst-level-3-face ((t (:foreground ,github-blue-1))))
   `(rst-level-4-face ((t (:foreground ,github-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,github-cyan))))
   `(rst-level-6-face ((t (:foreground ,github-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,github-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,github-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,github-fg-1 :background ,github-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,github-fg-1 :background ,github-yellow :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable GitHub for sml
   `(sml/global ((,class (:foreground ,github-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,github-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,github-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,github-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,github-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,github-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,github-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,github-orange))))
   `(sml/git ((,class (:foreground ,github-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,github-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,github-red-2))))
   `(sml/outside-modified ((,class (:foreground ,github-orange))))
   `(sml/modified ((,class (:foreground ,github-red))))
   `(sml/vc-edited ((,class (:foreground ,github-green+2))))
   `(sml/charging ((,class (:foreground ,github-green+4))))
   `(sml/discharging ((,class (:foreground ,github-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,github-red+1 :background ,github-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,github-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,github-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,github-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-red)))
      (t
       (:underline ,github-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-orange)))
      (t
       (:underline ,github-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-yellow)))
      (t
       (:underline ,github-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-green)))
      (t
       (:underline ,github-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,github-green+2))))
   `(speedbar-directory-face ((t (:foreground ,github-cyan))))
   `(speedbar-file-face ((t (:foreground ,github-fg))))
   `(speedbar-highlight-face ((t (:foreground ,github-bg :background ,github-green+2))))
   `(speedbar-selected-face ((t (:foreground ,github-red))))
   `(speedbar-separator-face ((t (:foreground ,github-bg :background ,github-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,github-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,github-fg
                                    :background ,github-bg))))
   `(tabbar-selected ((t (:foreground ,github-fg
                                      :background ,github-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,github-fg
                                        :background ,github-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,github-bg
                                       :background ,github-bg-1))))
   `(term-color-red ((t (:foreground ,github-red-2
                                     :background ,github-red-4))))
   `(term-color-green ((t (:foreground ,github-green
                                       :background ,github-green+2))))
   `(term-color-yellow ((t (:foreground ,github-orange
                                        :background ,github-yellow))))
   `(term-color-blue ((t (:foreground ,github-blue-1
                                      :background ,github-blue-4))))
   `(term-color-magenta ((t (:foreground ,github-magenta
                                         :background ,github-red))))
   `(term-color-cyan ((t (:foreground ,github-cyan
                                      :background ,github-blue))))
   `(term-color-white ((t (:foreground ,github-fg
                                       :background ,github-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,github-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,github-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,github-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,github-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,github-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,github-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,github-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,github-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,github-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,github-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,github-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,github-cyan))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,github-green+3))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,github-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,github-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,github-bg+1 :foreground ,github-bg+1))))
   `(whitespace-hspace ((t (:background ,github-bg+1 :foreground ,github-bg+1))))
   `(whitespace-tab ((t (:background ,github-red-1))))
   `(whitespace-newline ((t (:foreground ,github-bg+1))))
   `(whitespace-trailing ((t (:background ,github-red))))
   `(whitespace-line ((t (:background ,github-bg :foreground ,github-magenta))))
   `(whitespace-space-before-tab ((t (:background ,github-orange :foreground ,github-orange))))
   `(whitespace-indentation ((t (:background ,github-yellow :foreground ,github-red))))
   `(whitespace-empty ((t (:background ,github-yellow))))
   `(whitespace-space-after-tab ((t (:background ,github-yellow :foreground ,github-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,github-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,github-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,github-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,github-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,github-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,github-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,github-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,github-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,github-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,github-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,github-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,github-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,github-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,github-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,github-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,github-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,github-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,github-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,github-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,github-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,github-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,github-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,github-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,github-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,github-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,github-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,github-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,github-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,github-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,github-bg :background ,github-blue+1))))
   `(cscope-separator-face ((t (:foreground ,github-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,github-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,github-bg-1 :foreground ,github-bg-1))))
   ))

;;; Theme Variables
(github-with-color-variables
  (custom-theme-set-variables
   'github
;;;;; ansi-color
   `(ansi-color-names-vector [,github-bg ,github-red ,github-green ,github-yellow
                                          ,github-blue ,github-magenta ,github-cyan ,github-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,github-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,github-red ,github-orange ,github-yellow ,github-green ,github-green+4
                    ,github-cyan ,github-blue+1 ,github-magenta))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,github-red-1)
       ( 40. . ,github-red)
       ( 60. . ,github-orange)
       ( 80. . ,github-yellow-2)
       (100. . ,github-yellow-1)
       (120. . ,github-yellow)
       (140. . ,github-green-1)
       (160. . ,github-green)
       (180. . ,github-green+1)
       (200. . ,github-green+2)
       (220. . ,github-green+3)
       (240. . ,github-green+4)
       (260. . ,github-cyan)
       (280. . ,github-blue-2)
       (300. . ,github-blue-1)
       (320. . ,github-blue)
       (340. . ,github-blue+1)
       (360. . ,github-magenta)))
   `(vc-annotate-very-old-color ,github-magenta)
   `(vc-annotate-background ,github-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar github-add-font-lock-keywords nil
  "Whether to add font-lock keywords for github color names.
In buffers visiting library `github-theme.el' the github
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar github-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after github activate)
;;   "Maybe also add font-lock keywords for github colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or github-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "github-theme.el")))
;;     (unless github-colors-font-lock-keywords
;;       (setq github-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car github-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc github-colors-alist))))))
;;     (font-lock-add-keywords nil github-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after github activate)
;;   "Also remove font-lock keywords for github colors."
;;   (font-lock-remove-keywords nil github-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'github)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; github-theme.el ends here
