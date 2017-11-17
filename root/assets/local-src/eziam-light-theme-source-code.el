;;; eziam-common.el --- Common tools and face assignment table for Eziam

;; Copyright (c) 2016-2017 Thibault Polge <thibault@thb.lt>

;; Eziam is based on Tao theme, copyright (C) 2014 Peter <11111000000
;; at email.com> with contributions by Jasonm23 <jasonm23@gmail.com>.
;; Tao also credits: "Original faces taken from Zenburn theme port (c)
;; by Bozhidar Batsov"

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintener: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: faces
;; Homepage: https://github.com/thblt/eziam-theme-emacs
;; Version: 0.4.5

;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a dark and a light version of the Eziam theme
;; for Emacs.

;;; Code:

(defgroup eziam-theme nil
  "Customization options for the Eziam theme family.")

(defcustom eziam-scale-headings t
  "Non-nil means eziam-theme is allowed to customize the height of outline headlines."  :type 'boolean
  :group 'eziam-theme)

(defcustom eziam-scale-other t
  "Non-nil means eziam-theme is allowed to customize the height of non-outline headlines faces."  :type 'boolean
  :group 'eziam-theme)

(defun eziam-heading-height (height)
  "Return HEIGHT if EZIAM-SCALE-HEADINGS is non-nil."
  (if eziam-scale-headings
      height
    1.0))

(defun eziam-other-height (height)
  "Return HEIGHT if EZIAM-SCALE-OTHER is non-nil."
  (if eziam-scale-other
      height
    1.0))

(defmacro eziam-with-color-variables (eziam-colors &rest body)
  "`let' bind all colors defined in EZIAM-COLORS around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (c)
                     (list (intern (car c)) (cdr c)))
                   eziam-colors))
     ,@body))

(defun eziam-apply-custom-theme (theme-name)
  "Apply the Eziam theme faces under the name THEME-NAME.

This function should not be called directly, but wrapped in a let
block using EZIAM-WITH-COLOR-VARIABLES."
  (let ((class '((class color) (min-colors 256)))
        (ol1                  `(:height ,(eziam-heading-height 1.8) :foreground ,ol1-fg :background ,ol1-bg :weight bold :overline t))
        (ol2                  `(:height ,(eziam-heading-height 1.5) :foreground ,ol2-fg :background ,ol2-bg :overline t ))
        (ol3                  `(:height ,(eziam-heading-height 1.2) :foreground ,ol3-fg :background ,ol3-bg :weight bold :overline t ))
        (ol4                  `(:height ,(eziam-heading-height 1.0) :foreground ,ol4-fg :background ,ol4-bg :overline t))
        (ol5                  `(:height ,(eziam-heading-height 1.0) :foreground ,ol5-fg :background ,ol5-bg :overline t :weight bold))
        (ol6                  `(:height ,(eziam-heading-height 1.0) :foreground ,ol6-fg :background ,ol6-bg :underline t :overline t :weight bold))
        (ol7                  `(:height ,(eziam-heading-height 1.0) :foreground ,ol7-fg :background ,ol7-bg :underline t :weight bold :slant italic))
        (ol8                  `(:height ,(eziam-heading-height 1.0) :foreground ,ol8-fg :background ,ol8-bg :underline t :slant italic))
        (highlight            `(:background ,color-3))
        (transient-highlight  `(:background ,transient-highlight :foreground ,transient-highlight-fg))
        (info-text            `(:underline (:color ,info)))
        (warning-text         `(:underline (:color ,warning :style wave)))
        (error-text           `(:underline (:color ,error)))
        (button-on            `(:background ,color-0 :foreground ,color-7 :overline ,color-4 :weight bold))
        (button-off           `(:background ,color-2 :foreground ,color-7 :overline ,color-4))
        )

    (custom-theme-set-faces
     theme-name
     ;; Built-in
     `(bold                                             ((t (:weight bold))))
     ;; @FIXED - Don't set foreground for bold: it is used by some modes on a different background (eg tabulated-list-mode)
     `(default                                          ((t (:foreground ,color-8 :background ,color-1))))
     `(button                                           ((t (:underline t))))
     `(link                                             ((t (:foreground ,color-8 :underline t :weight bold))))
     `(link-visited                                     ((t (:foreground ,color-8 :underline t :weight normal))))
     `(hl-paren-face                                    ((t (:foreground ,color-8 :background ,color-1 :weight bold))))
     `(cursor                                           ((t (:foreground ,color-8 :background ,color-8))))
     `(escape-glyph                                     ((t (:foreground ,color-8 :bold t))))
     `(fringe                                           ((t (:foreground ,color-4 :background ,color-2))))
     `(header-line                                      ((t (:inherit mode-line))))
     `(highlight                                        ((,class ,highlight)))
     `(region                                           ((t (:background ,color-6 :foreground ,color-1))))
     `(shadow                                           ((t (:foreground ,color-3))))
     `(success                                          ((t (:foreground ,color-6 :weight bold))))
     `(warning                                          ((t (:foreground ,color-8 :weight bold))))
     ;; compilation
     `(compilation-column-face                          ((t (:foreground ,color-8))))
     `(compilation-enter-directory-face                 ((t (:foreground ,color-6))))
     `(compilation-error-face                           ((t (:foreground ,color-6 :weight bold :underline t))))
     `(compilation-face                                 ((t (:foreground ,color-8))))
     `(compilation-info-face                            ((t (:foreground ,color-8))))
     `(compilation-info                                 ((t (:foreground ,color-8 :underline t))))
     `(compilation-leave-directory-face                 ((t (:foreground ,color-6))))
     `(compilation-line-face                            ((t (:foreground ,color-8))))
     `(compilation-line-number                          ((t (:foreground ,color-8))))
     `(compilation-message-face                         ((t (:foreground ,color-8))))
     `(compilation-warning-face                         ((t (:foreground ,color-8 :weight bold :underline t))))
     `(compilation-mode-line-exit                       ((t (:foreground ,color-8 :weight bold))))
     `(compilation-mode-line-fail                       ((t (:foreground ,color-7 :weight bold))))
     `(compilation-mode-line-run                        ((t (:foreground ,color-8 :weight bold))))
     ;; grep
     `(grep-context-face                                ((t (:foreground ,color-8))))
     `(grep-error-face                                  ((t (:foreground ,color-6 :weight bold :underline t))))
     `(grep-hit-face                                    ((t (:foreground ,color-8))))
     `(grep-match-face                                  ((t (:foreground ,color-8 :weight bold))))
     `(match                                            ((t (:background ,color-1 :foreground ,color-8 :weight bold))))
     ;; haskell-mode @TODO: This is a work-in-progress
     `(haskell-literate-comment-face                    ((t (:background ,color-0))))
     ;; ivy
     `(ivy-current-match
       ;; This is very similar to :inverse-video t, but works better with color selectors.
       ((t (:background ,color-8 :foreground ,color-0 :weight bold))))
     ;; make
     `(makefile-space                                   ((t (:background ,color-1))))
     `(makefile-targets                                 ((t (:underline t))))
     `(makefile-shell                                   ((t (:slant italic))))
     ;; isearch
     `(isearch                                          ((,class ,transient-highlight)))
     `(isearch-fail                                     ((t (:foreground ,color-8 :background ,color-4))))
     `(lazy-highlight                                   ((t (:foreground ,color-8 :weight bold :background ,color-2))))
     `(menu                                             ((t (:foreground ,color-8 :background ,color-1))))
     `(minibuffer-prompt                                ((t (:foreground ,color-8 :color ,color-1))))
     `(mode-line                                        ((t (:foreground ,color-1 :background ,color-6 :box nil ))))
     `(mode-line-inactive                               ((t (:foreground ,color-1 :background ,color-4 :box nil ))))
     `(mode-line-buffer-id                              ((t (:foreground ,color-1 :weight bold))))
     `(secondary-selection                              ((t (:background ,color-1))))
     `(cua-rectangle                                    ((t (:background ,color-1))))
     `(trailing-whitespace                              ((t (:background ,color-7))))
     `(vertical-border                                  ((t (:foreground ,color-4 :background ,color-1))))
     ;; font lock
     `(font-lock-builtin-face                           ((t (:foreground ,color-8 :weight bold))))
     `(font-lock-comment-face                           ((t (:foreground ,color-5 :slant italic))))
     `(font-lock-delimiter-face                         ((t (:foreground ,color-5 :slant italic))))
     `(font-lock-constant-face                          ((t (:foreground ,color-5 :weight bold))))
     `(font-lock-doc-face                               ((t (:foreground ,color-6 :slant italic))))
     `(font-lock-function-name-face                     ((t (:background ,color-0 :box (:color ,color-2)))))
     `(font-lock-keyword-face                           ((t (:foreground ,color-8 :weight bold))))
     `(font-lock-negation-char-face                     ((t (:foreground ,color-8 :weight bold))))
     `(font-lock-preprocessor-face                      ((t (:foreground ,color-8))))
     `(font-lock-regexp-grouping-construct              ((t (:foreground ,color-8 :weight bold))))
     `(font-lock-regexp-grouping-backslash              ((t (:foreground ,color-6 :weight bold))))
     `(font-lock-string-face                            ((t (:foreground ,color-6))))
     `(font-lock-type-face                              ((t (:foreground ,color-7 :underline t))))
     `(font-lock-variable-name-face                     ((t (:foreground ,color-8 ))))
     `(font-lock-warning-face                           ((t (:foreground ,color-8 :weight bold))))
     `(c-annotation-face                                ((t (:inherit font-lock-constant-face))))
     ;; newsticker
     `(newsticker-date-face                             ((t (:foreground ,color-8))))
     `(newsticker-default-face                          ((t (:foreground ,color-8))))
     `(newsticker-enclosure-face                        ((t (:foreground ,color-8))))
     `(newsticker-extra-face                            ((t (:foreground ,color-4 :height ,(eziam-other-height 0.8))))) ;; FIXME
     `(newsticker-feed-face ((t (:foreground ,color-8))))
     `(newsticker-immortal-item-face                    ((t (:foreground ,color-6))))
     `(newsticker-new-item-face                         ((t (:foreground ,color-8))))
     `(newstickerphone-obsolete-item-face               ((t (:foreground ,color-7))))
     `(newsticker-old-item-face                         ((t (:foreground ,color-5))))
     `(newsticker-statistics-face                       ((t (:foreground ,color-8))))
     `(newsticker-treeview-face                         ((t (:foreground ,color-8))))
     `(newsticker-treeview-immortal-face                ((t (:foreground ,color-6))))
     `(newsticker-treeview-listwindow-face              ((t (:foreground ,color-8))))
     `(newsticker-treeview-new-face                     ((t (:foreground ,color-8 :weight bold))))
     `(newsticker-treeview-obsolete-face                ((t (:foreground ,color-7))))
     `(newsticker-treeview-old-face                     ((t (:foreground ,color-5))))
     `(newsticker-treeview-selection-face               ((t (:background ,color-1 :foreground ,color-8))))
     ;; Third-party
     ;; highlight-symbol
     `(highlight-symbol-face                            ((t (:background ,color-1))))
     ;; ace-jump
     `(ace-jump-face-background                         ((t (:foreground ,color-3 :background ,color-1 :inverse-video nil))))
     `(ace-jump-face-foreground                         ((t (:foreground ,color-8 :background ,color-1 :inverse-video nil))))
     ;; anzu
     `(anzu-mode-line                                   ((t (:foreground ,color-8 :weight bold))))
     ;; full-ack
     `(ack-separator                                    ((t (:foreground ,color-8))))
     `(ack-file                                         ((t (:foreground ,color-8))))
     `(ack-line                                         ((t (:foreground ,color-8))))
     `(ack-match                                        ((t (:foreground ,color-8 :background ,color-1 :weight bold))))
     ;; auctex
     `(font-latex-bold-face                             ((t (:inherit bold))))
     `(font-latex-warning-face                          ((t (:foreground nil :inherit font-lock-warning-face))))
     `(font-latex-sectioning-5-face                     ((t (:foreground ,color-7 :weight bold ))))
     `(font-latex-sedate-face                           ((t (:foreground ,color-8))))
     `(font-latex-italic-face                           ((t (:foreground ,color-8 :slant italic))))
     `(font-latex-string-face                           ((t (:inherit ,font-lock-string-face))))
     `(font-latex-math-face                             ((t (:foreground ,color-8))))
     ;; auto-complete
     `(ac-candidate-face                                ((t (:background ,color-5 :foreground ,color-1 :underline nil))))
     `(ac-selection-face                                ((t (:background ,color-4 :foreground ,color-8 :underline nil))))
     `(ac-yasnippet-candidate-face                      ((t (:background ,color-5 :foreground ,color-1))))
     `(ac-yasnippet-selection-face                      ((t (:background ,color-4 :foreground ,color-8))))
     `(ac-slime-menu-face                               ((t (:background ,color-5 :foreground ,color-1))))
     `(ac-slime-selection-face                          ((t (:background ,color-4 :foreground ,color-8))))
     `(ac-gtags-candidate-face                          ((t (:background ,color-5 :foreground ,color-1))))
     `(ac-gtags-selection-face                          ((t (:background ,color-4 :foreground ,color-8))))
     `(ac-emmet-candidate-face                          ((t (:background ,color-5 :foreground ,color-1))))
     `(ac-emmet-selection-face                          ((t (:background ,color-4 :foreground ,color-8))))
     ;; popup
     `(popup-tip-face                                   ((t (:background ,color-8 :foreground ,color-1))))
     `(popup-scroll-bar-foreground-face                 ((t (:background ,color-3))))
     `(popup-scroll-bar-background-face                 ((t (:background ,color-1))))
     `(popup-isearch-match                              ((t (:background ,color-1 :foreground ,color-8))))
     ;; android mode
     `(android-mode-debug-face                          ((t (:foreground ,color-7))))
     `(android-mode-error-face                          ((t (:foreground ,color-8 :weight bold))))
     `(android-mode-info-face                           ((t (:foreground ,color-8))))
     `(android-mode-verbose-face                        ((t (:foreground ,color-6))))
     `(android-mode-warning-face                        ((t (:foreground ,color-8))))
     ;; bm
     `(bm-face                                          ((t (:background ,color-8 :foreground ,color-2))))
     `(bm-fringe-face                                   ((t (:background ,color-1 :foreground ,color-1))))
     `(bm-fringe-persistent-face                        ((t (:background ,color-1 :foreground ,color-1))))
     `(bm-persistent-face                               ((t (:background ,color-5 :foreground ,color-2))))
     ;; clojure-test-mode
     `(clojure-test-failure-face                        ((t (:foreground ,color-8 :weight bold :underline t))))
     `(clojure-test-error-face                          ((t (:foreground ,color-7 :weight bold :underline t))))
     `(clojure-test-success-face                        ((t (:foreground ,color-7 :weight bold :underline t))))
     ;; coq
     `(coq-solve-tactics-face                           ((t (:foreground nil :inherit font-lock-constant-face))))
     ;; ctable
     `(ctbl:face-cell-select                            ((t (:background ,color-8 :foreground ,color-2))))
     `(ctbl:face-continue-bar                           ((t (:background ,color-1 :foreground ,color-2))))
     `(ctbl:face-row-select                             ((t (:background ,color-8 :foreground ,color-2))))
     ;; diff
     `(diff-added                                       ((,class (:foreground ,color-8 :background nil)) (t (:foreground ,color-5 :background nil))))
     `(diff-changed                                     ((t (:foreground ,color-8))))
     `(diff-removed                                     ((,class (:foreground ,color-7 :background nil)) (t (:foreground ,color-5 :background nil))))
     `(diff-refine-added                                ((t :inherit diff-added :weight bold)))
     `(diff-refine-change                               ((t :inherit diff-changed :weight bold)))
     `(diff-refine-removed                              ((t :inherit diff-removed :weight bold)))
     `(diff-header                                      ((,class (:background ,color-4)) (t (:background ,color-8 :foreground ,color-2))))
     `(diff-file-header                                 ((,class (:background ,color-4 :foreground ,color-8 :bold t)) (t (:background ,color-8 :foreground ,color-2 :bold t))))
     ;; dired
     `(dired-directory                                  ((t (:foreground ,color-8 :bold t))))
     ;; dired+
     `(diredp-display-msg                               ((t (:foreground ,color-8))))
     `(diredp-compressed-file-suffix                    ((t (:foreground ,color-8))))
     `(diredp-date-time                                 ((t (:foreground ,color-7))))
     `(diredp-deletion                                  ((t (:foreground ,color-8))))
     `(diredp-deletion-file-name                        ((t (:foreground ,color-7))))
     `(diredp-dir-heading                               ((t (:foreground ,color-8 :background ,color-1 :bold t))))
     `(diredp-dir-priv                                  ((t (:foreground ,color-8 :bold t))))
     `(diredp-dir-name                                  ((t (:foreground ,color-8 :bold t))))
     `(diredp-exec-priv                                 ((t (:foreground ,color-7))))
     `(diredp-executable-tag                            ((t (:foreground ,color-7))))
     `(diredp-file-name                                 ((t (:foreground ,color-8))))
     `(diredp-file-suffix                               ((t (:foreground ,color-8 :bold t))))
     `(diredp-flag-mark                                 ((t (:foreground ,color-8))))
     `(diredp-flag-mark-line                            ((t (:foreground ,color-8))))
     `(diredp-ignored-file-name                         ((t (:foreground ,color-3))))
     `(diredp-link-priv                                 ((t (:foreground ,color-8))))
     `(diredp-mode-line-flagged                         ((t (:foreground ,color-8))))
     `(diredp-mode-line-marked                          ((t (:foreground ,color-8))))
     `(diredp-no-priv                                   ((t (:foreground ,color-8))))
     `(diredp-number                                    ((t (:foreground ,color-7))))
     `(diredp-other-priv                                ((t (:foreground ,color-8))))
     `(diredp-rare-priv                                 ((t (:foreground ,color-6))))
     `(diredp-read-priv                                 ((t (:foreground ,color-5))))
     `(diredp-symlink                                   ((t (:foreground ,color-8))))
     `(diredp-write-priv                                ((t (:foreground ,color-7))))
     ;; ediff
     `(ediff-current-diff-A                             ((t (:foreground ,color-8 :background ,color-4))))
     `(ediff-current-diff-Ancestor                      ((t (:foreground ,color-8 :background ,color-4))))
     `(ediff-current-diff-B                             ((t (:foreground ,color-8 :background ,color-5))))
     `(ediff-current-diff-C                             ((t (:foreground ,color-8 :background ,color-3))))
     `(ediff-even-diff-A                                ((t (:background ,color-3))))
     `(ediff-even-diff-Ancestor                         ((t (:background ,color-3))))
     `(ediff-even-diff-B                                ((t (:background ,color-3))))
     `(ediff-even-diff-C                                ((t (:background ,color-3))))
     `(ediff-fine-diff-A                                ((t (:foreground ,color-8 :background ,color-6 :weight bold))))
     `(ediff-fine-diff-Ancestor                         ((t (:foreground ,color-8 :background ,color-6 :weight bold))))
     `(ediff-fine-diff-B                                ((t (:foreground ,color-8 :background ,color-6 :weight bold))))
     `(ediff-fine-diff-C                                ((t (:foreground ,color-8 :background ,color-5 :weight bold ))))
     `(ediff-odd-diff-A                                 ((t (:background ,color-4))))
     `(ediff-odd-diff-Ancestor                          ((t (:background ,color-4))))
     `(ediff-odd-diff-B                                 ((t (:background ,color-4))))
     `(ediff-odd-diff-C                                 ((t (:background ,color-4))))
     ;; ert
     `(ert-test-result-expected                         ((t (:foreground ,color-8 :background ,color-1))))
     `(ert-test-result-unexpected                       ((t (:foreground ,color-7 :background ,color-1))))
     ;; eshell
     `(eshell-prompt                                    ((t (:foreground ,color-8 :weight bold))))
     `(eshell-ls-archive                                ((t (:foreground ,color-6 :weight bold))))
     `(eshell-ls-backup                                 ((t (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter                                ((t (:inherit font-lock-comment-face))))
     `(eshell-ls-directory                              ((t (:foreground ,color-8 :weight bold))))
     `(eshell-ls-executable                             ((t (:foreground ,color-8 :weight bold))))
     `(eshell-ls-unreadable                             ((t (:foreground ,color-8))))
     `(eshell-ls-missing                                ((t (:inherit font-lock-warning-face))))
     `(eshell-ls-product                                ((t (:inherit font-lock-doc-face))))
     `(eshell-ls-special                                ((t (:foreground ,color-8 :weight bold))))
     `(eshell-ls-symlink                                ((t (:foreground ,color-8 :weight bold))))
     ;; flx
     `(flx-highlight-face                               ((t (:foreground ,color-8 :weight bold))))
     ;; flycheck
     `(flycheck-error                                   ((,class ,error-text)))
     `(flycheck-warning                                 ((,class ,warning-text)))
     `(flycheck-info                                    ((,class ,info-text)))
     `(flycheck-info                                    ((t (:underline (:color ,info)))))
     `(flycheck-fringe-error                            ((t (:foreground ,error))))
     `(flycheck-error-list-error                        ((t (:foreground ,error))))
     `(flycheck-fringe-warning                          ((t (:foreground ,warning))))
     `(flycheck-error-list-warning                      ((t (:foreground ,warning))))
     `(flycheck-fringe-info                             ((t (:foreground ,info))))
     `(flycheck-error-list-info                         ((t (:foreground ,info))))
     ;; flymake
     `(flymake-errline                                  ((,class ,error-text)))
     `(flymake-warnline                                 ((,class ,warning-text)))
     `(flymake-infoline                                 ((,class ,info-text)))
     ;; flyspell
     `(flyspell-duplicate                               ((,class ,warning-text)))
     `(flyspell-incorrect                               ((,class ,error-text)))
     ;; erc
     `(erc-action-face                                  ((t (:inherit erc-default-face))))
     `(erc-bold-face                                    ((t (:weight bold))))
     `(erc-current-nick-face                            ((t (:foreground ,color-8 :weight bold))))
     `(erc-dangerous-host-face                          ((t (:inherit font-lock-warning-face))))
     `(erc-default-face                                 ((t (:foreground ,color-8))))
     `(erc-direct-msg-face                              ((t (:inherit erc-default))))
     `(erc-error-face                                   ((t (:inherit font-lock-warning-face))))
     `(erc-fool-face                                    ((t (:inherit erc-default))))
     `(erc-highlight-face                               ((t (:inherit hover-highlight))))
     `(erc-input-face                                   ((t (:foreground ,color-8))))
     `(erc-keyword-face                                 ((t (:foreground ,color-8 :weight bold))))
     `(erc-nick-default-face                            ((t (:foreground ,color-8 :weight bold))))
     `(erc-my-nick-face                                 ((t (:foreground ,color-7 :weight bold))))
     `(erc-nick-msg-face                                ((t (:inherit erc-default))))
     `(erc-notice-face                                  ((t (:foreground ,color-6))))
     `(erc-pal-face                                     ((t (:foreground ,color-8 :weight bold))))
     `(erc-prompt-face                                  ((t (:foreground ,color-8 :background ,color-1 :weight bold))))
     `(erc-timestamp-face                               ((t (:foreground ,color-8))))
     `(erc-underline-face                               ((t (:underline t))))
     ;; git-gutter
     `(git-gutter:added                                 ((t (:background ,color-1 :foreground ,color-1 :weight bold ))))
     `(git-gutter:deleted                               ((t (:background ,color-1 :foreground ,color-1 :weight bold ))))
     `(git-gutter:modified                              ((t (:background ,color-1 :foreground ,color-1 :weight bold ))))
     `(git-gutter:unchanged                             ((t (:background ,color-1 :foreground ,color-1 :weight bold ))))
     ;; git-gutter-fr
     `(git-gutter-fr:added                              ((t (:foreground ,color-1 :weight bold))))
     `(git-gutter-fr:deleted                            ((t (:foreground ,color-1 :weight bold))))
     `(git-gutter-fr:modified                           ((t (:foreground ,color-1 :weight bold))))
     ;; gnus
     `(gnus-group-mail-1                                ((t (:bold t :inherit gnus-group-mail-1-empty))))
     `(gnus-group-mail-1-empty                          ((t (:inherit gnus-group-news-1-empty))))
     `(gnus-group-mail-2                                ((t (:bold t :inherit gnus-group-mail-2-empty))))
     `(gnus-group-mail-2-empty                          ((t (:inherit gnus-group-news-2-empty))))
     `(gnus-group-mail-3                                ((t (:bold t :inherit gnus-group-mail-3-empty))))
     `(gnus-group-mail-3-empty                          ((t (:inherit gnus-group-news-3-empty))))
     `(gnus-group-mail-4                                ((t (:bold t :inherit gnus-group-mail-4-empty))))
     `(gnus-group-mail-4-empty                          ((t (:inherit gnus-group-news-4-empty))))
     `(gnus-group-mail-5                                ((t (:bold t :inherit gnus-group-mail-5-empty))))
     `(gnus-group-mail-5-empty                          ((t (:inherit gnus-group-news-5-empty))))
     `(gnus-group-mail-6                                ((t (:bold t :inherit gnus-group-mail-6-empty))))
     `(gnus-group-mail-6-empty                          ((t (:inherit gnus-group-news-6-empty))))
     `(gnus-group-mail-low                              ((t (:bold t :inherit gnus-group-mail-low-empty))))
     `(gnus-group-mail-low-empty                        ((t (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-1                                ((t (:bold t :inherit gnus-group-news-1-empty))))
     `(gnus-group-news-2                                ((t (:bold t :inherit gnus-group-news-2-empty))))
     `(gnus-group-news-3                                ((t (:bold t :inherit gnus-group-news-3-empty))))
     `(gnus-group-news-4                                ((t (:bold t :inherit gnus-group-news-4-empty))))
     `(gnus-group-news-5                                ((t (:bold t :inherit gnus-group-news-5-empty))))
     `(gnus-group-news-6                                ((t (:bold t :inherit gnus-group-news-6-empty))))
     `(gnus-group-news-low                              ((t (:bold t :inherit gnus-group-news-low-empty))))
     `(gnus-header-content                              ((t (:inherit message-header-other))))
     `(gnus-header-from                                 ((t (:inherit message-header-from))))
     `(gnus-header-name                                 ((t (:inherit message-header-name))))
     `(gnus-header-newsgroups                           ((t (:inherit message-header-other))))
     `(gnus-header-subject                              ((t (:inherit message-header-subject))))
     `(gnus-summary-cancelled                           ((t (:foreground ,color-8))))
     `(gnus-summary-high-ancient                        ((t (:foreground ,color-8))))
     `(gnus-summary-high-read                           ((t (:foreground ,color-6 :weight bold))))
     `(gnus-summary-high-ticked                         ((t (:foreground ,color-8 :weight bold))))
     `(gnus-summary-high-unread                         ((t (:foreground ,color-8 :weight bold))))
     `(gnus-summary-low-ancient                         ((t (:foreground ,color-8))))
     `(gnus-summary-low-read                            ((t (:foreground ,color-6))))
     `(gnus-summary-low-ticked                          ((t (:foreground ,color-8 :weight bold))))
     `(gnus-summary-low-unread                          ((t (:foreground ,color-8))))
     `(gnus-summary-normal-ancient                      ((t (:foreground ,color-8))))
     `(gnus-summary-normal-read                         ((t (:foreground ,color-6))))
     `(gnus-summary-normal-ticked                       ((t (:foreground ,color-8 :weight bold))))
     `(gnus-summary-normal-unread                       ((t (:foreground ,color-8))))
     `(gnus-summary-selected                            ((t (:foreground ,color-8 :weight bold))))
     `(gnus-cite-1                                      ((t (:foreground ,color-8))))
     `(gnus-cite-10                                     ((t (:foreground ,color-8))))
     `(gnus-cite-11                                     ((t (:foreground ,color-8))))
     `(gnus-cite-2                                      ((t (:foreground ,color-7))))
     `(gnus-cite-3                                      ((t (:foreground ,color-6))))
     `(gnus-cite-4                                      ((t (:foreground ,color-8))))
     `(gnus-cite-5                                      ((t (:foreground ,color-7))))
     `(gnus-cite-6                                      ((t (:foreground ,color-6))))
     `(gnus-cite-7                                      ((t (:foreground ,color-7))))
     `(gnus-cite-8                                      ((t (:foreground ,color-6))))
     `(gnus-cite-9                                      ((t (:foreground ,color-6))))
     `(gnus-group-news-1-empty                          ((t (:foreground ,color-8))))
     `(gnus-group-news-2-empty                          ((t (:foreground ,color-8))))
     `(gnus-group-news-3-empty                          ((t (:foreground ,color-7))))
     `(gnus-group-news-4-empty                          ((t (:foreground ,color-6))))
     `(gnus-group-news-5-empty                          ((t (:foreground ,color-5))))
     `(gnus-group-news-6-empty                          ((t (:foreground ,color-4))))
     `(gnus-group-news-low-empty                        ((t (:foreground ,color-4))))
     `(gnus-signature                                   ((t (:foreground ,color-8))))
     `(gnus-x                                           ((t (:background ,color-8 :foreground ,color-2))))
     ;; guide-key
     `(guide-key/highlight-command-face                 ((t (:foreground ,color-8))))
     `(guide-key/key-face                               ((t (:foreground ,color-6))))
     `(guide-key/prefix-command-face                    ((t (:foreground ,color-7))))
     ;; helm
     `(helm-header                                      ((t (:foreground ,color-6 :background ,color-1 :underline nil :box nil))))
     `(helm-source-header                               ((t (:foreground ,color-8 :background ,color-1 :underline nil :weight bold :box (:line-width -1 :style released-button)))))
     `(helm-selection                                   ((t (:background ,color-3 :underline nil))))
     `(helm-selection-line                              ((t (:background ,color-3))))
     `(helm-visible-mark                                ((t (:foreground ,color-2 :background ,color-8))))
     `(helm-candidate-number                            ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-separator                                   ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-time-zone-current                           ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-time-zone-home                              ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-bookmark-addressbook                        ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-bookmark-directory                          ((t (:foreground nil :background nil :inherit helm-ff-directory))))
     `(helm-bookmark-file                               ((t (:foreground nil :background nil :inherit helm-ff-file))))
     `(helm-bookmark-gnus                               ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-bookmark-info                               ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-bookmark-man                                ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-bookmark-w3m                                ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-buffer-not-saved                            ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-buffer-process                              ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-buffer-saved-out                            ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-buffer-size                                 ((t (:foreground ,color-4 :background ,color-1))))
     `(helm-buffer-directory                            ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-lisp-completion-info                        ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-lisp-show-completion                        ((t (:foreground ,color-8 :background ,color-3))))
     `(helm-locate-finish                               ((t (:foreground ,color-5 :background ,color-1))))
     `(helm-dotted-symlink-directory                    ((t (:foreground ,color-4 :background ,color-2))))
     `(helm-ff-dotted-symlink-directory                 ((t (:foreground ,color-4 :background ,color-2))))
     `(helm-ff-directory                                ((t (:foreground ,color-8 :background ,color-1 :weight bold))))
     `(helm-ff-file                                     ((t (:foreground ,color-8 :background ,color-1 :weight normal))))
     `(helm-ff-executable                               ((t (:foreground ,color-8 :background ,color-1 :weight normal))))
     `(helm-ff-invalid-symlink                          ((t (:foreground ,color-7 :background ,color-1 :weight bold))))
     `(helm-ff-symlink                                  ((t (:foreground ,color-8 :background ,color-1 :weight bold))))
     `(helm-ff-prefix                                   ((t (:foreground ,color-2 :background ,color-8 :weight normal))))
     `(helm-grep-cmd-line                               ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-grep-file                                   ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-grep-finish                                 ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-grep-lineno                                 ((t (:foreground ,color-4 :background ,color-1))))
     `(helm-match                                       ((t (:foreground ,color-8 :background ,color-2))))
     `(helm-grep-match                                  ((t ( :inherit helm-match))))
     `(helm-grep-running                                ((t (:foreground ,color-7 :background ,color-1))))
     `(helm-moccur-buffer                               ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-mu-contacts-address-face                    ((t (:foreground ,color-4 :background ,color-1))))
     `(helm-mu-contacts-name-face                       ((t (:foreground ,color-8 :background ,color-1))))
     `(helm-M-x-key                                     ((t (:foreground ,color-8 :background ,color-1 :weight bold))))
     ;; hl-line-mode FIXME Looks weird
     `(hl-line                                          ((,class ,highlight)))
     `(hl-line-face                                     ((,class ,highlight)))
     ;; hl-sexp
     `(hl-sexp-face                                     ((,class (:background ,color-3)) (t :weight bold)))
     ;; ido-mode
     `(ido-first-match                                  ((t (:foreground ,color-8 :weight bold))))
     `(ido-only-match                                   ((t (:foreground ,color-8 :weight bold))))
     `(ido-subdir                                       ((t (:foreground ,color-8))))
     `(ido-indicator                                    ((t (:foreground ,color-8 :background ,color-4))))
     ;; iedit-mode
     `(iedit-occurrence                                 ((t (:background ,color-4 :weight bold))))
     ;; js2-mode
     `(js2-warning                                      ((t (:underline ,color-1))))
     `(js2-error                                        ((t (:foreground ,color-7 :weight bold))))
     `(js2-jsdoc-tag                                    ((t (:foreground ,color-5))))
     `(js2-jsdoc-type                                   ((t (:foreground ,color-8))))
     `(js2-jsdoc-value                                  ((t (:foreground ,color-8))))
     `(js2-jsdoc-html-tag-name                          ((t (:foreground ,color-8))))
     `(js2-jsdoc-html-tag-delimiter                     ((t (:foreground ,color-8))))
     `(js2-function-param                               ((t (:foreground ,color-8))))
     `(js2-function-call                                ((t (:foreground ,color-8 :underline t))))
     `(js2-object-property                              ((t (:foreground ,color-8  :slant italic))))
     `(js2-external-variable                            ((t (:foreground ,color-4))))
     ;; jabber-mode
     `(jabber-roster-user-away                          ((t (:foreground ,color-8))))
     `(jabber-roster-user-online                        ((t (:foreground ,color-7))))
     `(jabber-roster-user-dnd                           ((t (:foreground ,color-8))))
     `(jabber-rare-time-face                            ((t (:foreground ,color-7))))
     `(jabber-chat-prompt-local                         ((t (:foreground ,color-7))))
     `(jabber-chat-prompt-foreign                       ((t (:foreground ,color-8))))
     `(jabber-activity-face                             ((t (:foreground ,color-8))))
     `(jabber-activity-personal-face                    ((t (:foreground ,color-8))))
     `(jabber-title-small                               ((t (:height ,(eziam-other-height 1.1) :weight bold))))
     `(jabber-title-medium                              ((t (:height ,(eziam-other-height 1.2) :weight bold))))
     `(jabber-title-medium                              ((t (:height ,(eziam-other-height 1.3) :weight bold))))
     ;; ledger-mode
     `(ledger-font-payee-uncleared-face                 ((t (:foreground ,color-6 :weight bold))))
     `(ledger-font-payee-cleared-face                   ((t (:foreground ,color-8 :weight normal))))
     `(ledger-font-xact-highlight-face                  ((t (:background ,color-3))))
     `(ledger-font-pending-face                         ((t (:foreground ,color-8 :weight normal))))
     `(ledger-font-other-face                           ((t (:foreground ,color-8))))
     `(ledger-font-posting-account-face                 ((t (:foreground ,color-7))))
     `(ledger-font-posting-account-cleared-face         ((t (:foreground ,color-8))))
     `(ledger-font-posting-account-pending-face         ((t (:foreground ,color-8))))
     `(ledger-font-posting-amount-face                  ((t (:foreground ,color-8))))
     `(ledger-font-posting-account-pending-face         ((t (:foreground ,color-8))))
     `(ledger-occur-narrowed-face                       ((t (:foreground ,color-4 :invisible t))))
     `(ledger-occur-xact-face                           ((t (:background ,color-3))))
     `(ledger-font-comment-face                         ((t (:foreground ,color-6))))
     `(ledger-font-reconciler-uncleared-face            ((t (:foreground ,color-6 :weight bold))))
     `(ledger-font-reconciler-cleared-face              ((t (:foreground ,color-8 :weight normal))))
     `(ledger-font-reconciler-pending-face              ((t (:foreground ,color-8 :weight normal))))
     `(ledger-font-report-clickable-face                ((t (:foreground ,color-8 :weight normal))))
     ;; linum-mode
     `(linum                                            ((t (:background ,color-2 :foreground ,color-6 :inherit default)))) ;; @fringe
     ;; linum-relative-mode
     `(linum-relative-current-face                      ((t (:background ,color-1 :foreground ,color-8 :weight bold :inherit default)))) ;; @fringe
     ;; macrostep
     `(macrostep-gensym-1                               ((t (:foreground ,color-8 :background ,color-1))))
     `(macrostep-gensym-2                               ((t (:foreground ,color-8 :background ,color-1))))
     `(macrostep-gensym-3                               ((t (:foreground ,color-8 :background ,color-1))))
     `(macrostep-gensym-4                               ((t (:foreground ,color-7 :background ,color-1))))
     `(macrostep-gensym-5                               ((t (:foreground ,color-8 :background ,color-1))))
     `(macrostep-expansion-highlight-face               ((t (:inherit highlight))))
     `(macrostep-macro-face                             ((t (:underline t))))
     ;; magit
     `(magit-section-title                              ((t (:foreground ,color-8 :weight bold))))
     `(magit-branch                                     ((t (:foreground ,color-8 :weight bold))))
     `(magit-item-highlight                             ((t (:inverse-video t))))
     ;; egg
     `(egg-text-base                                    ((t (:foreground ,color-8))))
     `(egg-help-header-1                                ((t (:foreground ,color-8))))
     `(egg-help-header-2                                ((t (:foreground ,color-8))))
     `(egg-branch                                       ((t (:foreground ,color-8))))
     `(egg-branch-mono                                  ((t (:foreground ,color-8))))
     `(egg-term                                         ((t (:foreground ,color-8))))
     `(egg-diff-add                                     ((t (:foreground ,color-8))))
     `(egg-diff-del                                     ((t (:foreground ,color-8))))
     `(egg-diff-file-header                             ((t (:foreground ,color-8))))
     `(egg-section-title                                ((t (:foreground ,color-8))))
     `(egg-stash-mono                                   ((t (:foreground ,color-8))))
     ;; message-mode
     `(message-cited-text                               ((t (:inherit font-lock-comment-face))))
     `(message-header-name                              ((t (:foreground ,color-7))))
     `(message-header-other                             ((t (:foreground ,color-6))))
     `(message-header-to                                ((t (:foreground ,color-8 :weight bold))))
     `(message-header-from                              ((t (:foreground ,color-8 :weight bold))))
     `(message-header-cc                                ((t (:foreground ,color-8 :weight bold))))
     `(message-header-newsgroups                        ((t (:foreground ,color-8 :weight bold))))
     `(message-header-subject                           ((t (:foreground ,color-8 :weight bold))))
     `(message-header-xheader                           ((t (:foreground ,color-6))))
     `(message-mml                                      ((t (:foreground ,color-8 :weight bold))))
     `(message-separator                                ((t (:inherit font-lock-comment-face))))
     ;; mew
     `(mew-face-header-subject                          ((t (:foreground ,color-8))))
     `(mew-face-header-from                             ((t (:foreground ,color-8))))
     `(mew-face-header-date                             ((t (:foreground ,color-6))))
     `(mew-face-header-to                               ((t (:foreground ,color-7))))
     `(mew-face-header-key                              ((t (:foreground ,color-6))))
     `(mew-face-header-private                          ((t (:foreground ,color-6))))
     `(mew-face-header-important                        ((t (:foreground ,color-8))))
     `(mew-face-header-marginal                         ((t (:foreground ,color-8 :weight bold))))
     `(mew-face-header-warning                          ((t (:foreground ,color-7))))
     `(mew-face-header-xmew                             ((t (:foreground ,color-6))))
     `(mew-face-header-xmew-bad                         ((t (:foreground ,color-7))))
     `(mew-face-body-url                                ((t (:foreground ,color-8))))
     `(mew-face-body-comment                            ((t (:foreground ,color-8 :slant italic))))
     `(mew-face-body-cite1                              ((t (:foreground ,color-6))))
     `(mew-face-body-cite2                              ((t (:foreground ,color-8))))
     `(mew-face-body-cite3                              ((t (:foreground ,color-8))))
     `(mew-face-body-cite4                              ((t (:foreground ,color-8))))
     `(mew-face-body-cite5                              ((t (:foreground ,color-7))))
     `(mew-face-mark-review                             ((t (:foreground ,color-8))))
     `(mew-face-mark-escape                             ((t (:foreground ,color-6))))
     `(mew-face-mark-delete                             ((t (:foreground ,color-7))))
     `(mew-face-mark-unlink                             ((t (:foreground ,color-8))))
     `(mew-face-mark-refile                             ((t (:foreground ,color-6))))
     `(mew-face-mark-unread                             ((t (:foreground ,color-6))))
     `(mew-face-eof-message                             ((t (:foreground ,color-6))))
     `(mew-face-eof-part                                ((t (:foreground ,color-8))))
     ;; mic-paren
     `(paren-face-match                                 ((t (:foreground ,color-8 :background ,color-1))))
     `(paren-face-mismatch                              ((t (:foreground ,color-1 :background ,color-1))))
     `(paren-face-no-match                              ((t (:foreground ,color-3 :background ,color-1))))
     ;; mingus
     `(mingus-directory-face                            ((t (:foreground ,color-8))))
     `(mingus-pausing-face                              ((t (:foreground ,color-7))))
     `(mingus-playing-face                              ((t (:foreground ,color-8))))
     `(mingus-playlist-face                             ((t (:foreground ,color-8 ))))
     `(mingus-song-file-face                            ((t (:foreground ,color-8))))
     `(mingus-stopped-face                              ((t (:foreground ,color-7))))
     ;; nav
     `(nav-face-heading                                 ((t (:foreground ,color-8))))
     `(nav-face-button-num                              ((t (:foreground ,color-8))))
     `(nav-face-dir                                     ((t (:foreground ,color-6))))
     `(nav-face-hdir                                    ((t (:foreground ,color-7))))
     `(nav-face-file                                    ((t (:foreground ,color-8))))
     `(nav-face-hfile                                   ((t (:foreground ,color-4))))
     ;; mu4e
     `(mu4e-title-face                                  ((t (:foreground ,color-8 :weight bold))))
     `(mu4e-header-highlight-face                       ((, class ,highlight)))
     `(mu4e-cited-1-face                                ((t (:foreground ,color-8 :slant italic))))
     `(mu4e-cited-2-face                                ((t (:foreground ,color-8 :slant italic))))
     `(mu4e-cited-3-face                                ((t (:foreground ,color-6 :slant italic))))
     `(mu4e-cited-4-face                                ((t (:foreground ,color-6 :slant italic))))
     `(mu4e-cited-5-face                                ((t (:foreground ,color-4 :slant italic))))
     `(mu4e-cited-6-face                                ((t (:foreground ,color-5 :slant italic))))
     `(mu4e-cited-7-face                                ((t (:foreground ,color-8 :slant italic))))
     `(mu4e-unread-face                                 ((t (:background ,color-0 :weight bold))))
     `(mu4e-replied-face                                ((t (:foreground ,color-6 :slant italic))))
     `(mu4e-trashed-face                                ((t (:foreground ,color-5 :strike-through t))))
     `(mu4e-flagged-face                                ((t (:foreground ,info))))
     `(mu4e-ok-face                                     ((t (:foreground ,ok :weight bold))))
     `(mu4e-warning-face                                ((t (:foreground ,warning :weight bold))))
     ;; mumamo
     `(mumamo-background-chunk-major                    ((t (:background nil))))
     `(mumamo-background-chunk-submode1                 ((t (:background ,color-1))))
     `(mumamo-background-chunk-submode2                 ((t (:background ,color-4))))
     `(mumamo-background-chunk-submode3                 ((t (:background ,color-5))))
     `(mumamo-background-chunk-submode4                 ((t (:background ,color-3))))
     ;; org-mode
     `(org-agenda-date-today                            ((t (:foreground ,color-8 :slant italic :weight bold))) t)
     `(org-agenda-structure                             ((t (:inherit font-lock-comment-face))))
     `(org-archived                                     ((t (:foreground ,color-8 :weight bold))))
     `(org-checkbox                                     ((t (:background ,color-4 :foreground ,color-8 :box (:line-width 1 :style released-button)))))
     `(org-code                                         ((t (:background ,color-2 :box ,color-3))))
     `(org-verbatim                                     ((t (:background ,color-2 :box ,color-3))))
     `(org-date                                         ((t (:foreground ,color-8 :underline t))))
     `(org-deadline-announce                            ((t (:foreground ,color-6))))
     `(org-formula                                      ((t (:foreground ,color-5))))
     `(org-headline-done                                ((t (:foreground ,color-8))))
     `(org-hide                                         ((t (:foreground ,color-1))))
     `(org-document-title                               ((t (:foreground ,color-8 :height ,(eziam-other-height 1.9) :bold t))))
     `(org-document-info                                ((t (:inherit default))))
     `(org-document-info-keyword                        ((t (:foreground ,color-6 :weight bold :underline t))))
     `(org-level-1                                      ((,class ,ol1)))
     `(org-level-2                                      ((,class ,ol2)))
     `(org-level-3                                      ((,class ,ol3)))
     `(org-level-4                                      ((,class ,ol4)))
     `(org-level-5                                      ((,class ,ol5)))
     `(org-level-6                                      ((,class ,ol6)))
     `(org-level-7                                      ((,class ,ol7)))
     `(org-level-8                                      ((,class ,ol8)))
     `(org-link                                         ((t (:foreground ,color-8 :underline t))))
     `(org-list-dt                                      ((t (:weight bold))))
     `(org-property-value                               ((t (:foreground ,color-5 :slant italic))))
     `(org-scheduled                                    ((t (:foreground ,color-8))))
     `(org-scheduled-previously                         ((t (:foreground ,color-7))))
     `(org-scheduled-today                              ((t (:foreground ,color-8 :bold t))))
     `(org-sexp-date                                    ((t (:foreground ,color-8 :underline t))))
     `(org-special-keyword                              ((t (:foreground ,color-5))))
     `(org-table                                        ((t (:foreground ,color-8))))
     `(org-tag                                          ((t (:foreground ,color-4))))
     `(org-target                                       ((t (:foreground ,color-4))))
     `(org-time-grid                                    ((t (:foreground ,color-8))))
     `(org-done                                         ((t (:bold t :background ,color-7 :foreground ,color-1 :weight bold))))
     `(org-todo                                         ((t (:bold t :inverse-video t))))
     `(org-upcoming-deadline                            ((t (:slant italic))))
     `(org-warning                                      ((t (:foreground ,color-7 :underline t))))
     `(org-column                                       ((t (:background ,color-1))))
     `(org-column-title                                 ((t (:background ,color-1 :underline t :weight bold))))
     `(org-mode-line-clock                              ((t (:foreground ,color-8 :background ,color-1))))
     `(org-mode-line-clock-overrun                      ((t (:foreground ,color-2 :background ,color-6))))
     `(org-ellipsis                                     ((t (:foreground ,color-8 :underline t))))
     `(org-footnote                                     ((t (:foreground ,color-8 :underline t))))
     `(org-meta-line                                    ((t (:foreground ,color-5))))
     ;; I believe the difference between org-block-background and
     ;; org-block is that org 8.x uses the former, 9.x the latter.  Both
     ;; should then be identical.
     `(org-block-background                             ((t (:background ,color-0))))
     `(org-block                                        ((t (:background ,color-0))))
     `(org-block-begin-line                             ((t (:foreground ,color-6 :background ,color-3))))
     `(org-block-end-line                               ((t (:foreground ,color-6 :background ,color-3))))
     ;; outline
     `(outline-1                                        ((,class ,ol1)))
     `(outline-2                                        ((,class ,ol2)))
     `(outline-3                                        ((,class ,ol3)))
     `(outline-4                                        ((,class ,ol4)))
     `(outline-5                                        ((,class ,ol5)))
     `(outline-6                                        ((,class ,ol6)))
     `(outline-7                                        ((,class ,ol7)))
     `(outline-8                                        ((,class ,ol8)))
     ;; outshine
     `(outshine-level-1                                 ((,class ,ol1)))
     `(outshine-level-2                                 ((,class ,ol2)))
     `(outshine-level-3                                 ((,class ,ol3)))
     `(outshine-level-4                                 ((,class ,ol4)))
     `(outshine-level-5                                 ((,class ,ol5)))
     `(outshine-level-6                                 ((,class ,ol6)))
     `(outshine-level-7                                 ((,class ,ol7)))
     `(outshine-level-8                                 ((,class ,ol8)))
     ;; p4
     `(p4-depot-added-face                              ((t :inherit diff-added)))
     `(p4-depot-branch-op-face                          ((t :inherit diff-changed)))
     `(p4-depot-deleted-face                            ((t :inherit diff-removed)))
     `(p4-depot-unmapped-face                           ((t :inherit diff-changed)))
     `(p4-diff-change-face                              ((t :inherit diff-changed)))
     `(p4-diff-del-face                                 ((t :inherit diff-removed)))
     `(p4-diff-file-face                                ((t :inherit diff-file-header)))
     `(p4-diff-head-face                                ((t :inherit diff-header)))
     `(p4-diff-ins-face                                 ((t :inherit diff-added)))
     ;; perspective
     `(persp-selected-face                              ((t (:foreground ,color-8 :inherit mode-line))))
     ;; powerline
     `(powerline-active1                                ((t (:background ,color-3 :foreground ,color-8 :box nil :inherit mode-line ))))
     `(powerline-active2                                ((t (:background ,color-1 :foreground ,color-5 :box nil :inherit mode-line ))))
     `(powerline-inactive1                              ((t (:background ,color-1 :foreground ,color-3 :inherit mode-line-inactive))))
     `(powerline-inactive2                              ((t (:background ,color-1 :foreground ,color-4 :inherit mode-line-inactive))))
     ;; proofgeneral
     `(proof-active-area-face                           ((t (:underline t))))
     `(proof-boring-face                                ((t (:foreground ,color-8 :background ,color-4))))
     `(proof-command-mouse-highlight-face               ((t (:inherit proof-mouse-highlight-face))))
     `(proof-debug-message-face                         ((t (:inherit proof-boring-face))))
     `(proof-declaration-name-face                      ((t (:inherit font-lock-keyword-face :foreground nil))))
     `(proof-eager-annotation-face                      ((t (:foreground ,color-2 :background ,color-8))))
     `(proof-error-face                                 ((t (:foreground ,color-8 :background ,color-4))))
     `(proof-highlight-dependency-face                  ((t (:foreground ,color-2 :background ,color-8))))
     `(proof-highlight-dependent-face                   ((t (:foreground ,color-2 :background ,color-8))))
     `(proof-locked-face                                ((t (:background ,color-3))))
     `(proof-mouse-highlight-face                       ((t (:foreground ,color-2 :background ,color-8))))
     `(proof-queue-face                                 ((t (:background ,color-4))))
     `(proof-region-mouse-highlight-face                ((t (:inherit proof-mouse-highlight-face))))
     `(proof-script-highlight-error-face                ((t (:background ,color-6))))
     `(proof-tacticals-name-face                        ((t (:inherit font-lock-constant-face :foreground nil :background ,color-1))))
     `(proof-tactics-name-face                          ((t (:inherit font-lock-constant-face :foreground nil :background ,color-1))))
     `(proof-warning-face                               ((t (:foreground ,color-2 :background ,color-8))))
     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face                  ((t (:foreground ,rainbow-1 :bold t))))
     `(rainbow-delimiters-depth-2-face                  ((t (:foreground ,rainbow-4 :bold t))))
     `(rainbow-delimiters-depth-3-face                  ((t (:foreground ,rainbow-2 :bold t))))
     `(rainbow-delimiters-depth-4-face                  ((t (:foreground ,rainbow-5 :bold t))))
     `(rainbow-delimiters-depth-5-face                  ((t (:foreground ,rainbow-3 :bold t))))
     `(rainbow-delimiters-depth-6-face                  ((t (:foreground ,rainbow-6 :bold t))))
     `(rainbow-delimiters-depth-7-face                  ((t (:foreground ,rainbow-1 :bold t))))
     `(rainbow-delimiters-depth-8-face                  ((t (:foreground ,rainbow-4 :bold t))))
     `(rainbow-delimiters-depth-9-face                  ((t (:foreground ,rainbow-2 :bold t))))
     `(rainbow-delimiters-depth-10-face                 ((t (:foreground ,rainbow-5 :bold t))))
     `(rainbow-delimiters-depth-11-face                 ((t (:foreground ,rainbow-3 :bold t))))
     `(rainbow-delimiters-depth-12-face                 ((t (:foreground ,rainbow-6 :bold t))))
     ;; rcirc
     `(rcirc-my-nick                                    ((t (:foreground ,color-8))))
     `(rcirc-other-nick                                 ((t (:foreground ,color-8))))
     `(rcirc-bright-nick                                ((t (:foreground ,color-8))))
     `(rcirc-dim-nick                                   ((t (:foreground ,color-6))))
     `(rcirc-server                                     ((t (:foreground ,color-6))))
     `(rcirc-server-prefix                              ((t (:foreground ,color-7))))
     `(rcirc-timestamp                                  ((t (:foreground ,color-8))))
     `(rcirc-nick-in-message                            ((t (:foreground ,color-8))))
     `(rcirc-nick-in-message-full-line                  ((t (:bold t))))
     `(rcirc-prompt                                     ((t (:foreground ,color-8 :bold t))))
     `(rcirc-track-nick                                 ((t (:inverse-video t))))
     `(rcirc-track-keyword                              ((t (:bold t))))
     `(rcirc-url                                        ((t (:bold t))))
     `(rcirc-keyword                                    ((t (:foreground ,color-8 :bold t))))
     ;; rpm-mode
     `(rpm-spec-dir-face                                ((t (:foreground ,color-6))))
     `(rpm-spec-doc-face                                ((t (:foreground ,color-6))))
     `(rpm-spec-ghost-face                              ((t (:foreground ,color-7))))
     `(rpm-spec-macro-face                              ((t (:foreground ,color-8))))
     `(rpm-spec-obsolete-tag-face                       ((t (:foreground ,color-7))))
     `(rpm-spec-package-face                            ((t (:foreground ,color-7))))
     `(rpm-spec-section-face                            ((t (:foreground ,color-8))))
     `(rpm-spec-tag-face                                ((t (:foreground ,color-8))))
     `(rpm-spec-var-face                                ((t (:foreground ,color-7))))
     ;; rst-mode
     `(rst-level-1-face                                 ((t (:foreground ,color-8))))
     `(rst-level-2-face                                 ((t (:foreground ,color-7))))
     `(rst-level-3-face                                 ((t (:foreground ,color-7))))
     `(rst-level-4-face                                 ((t (:foreground ,color-8))))
     `(rst-level-5-face                                 ((t (:foreground ,color-8))))
     `(rst-level-6-face                                 ((t (:foreground ,color-5))))
     ;; sh-mode
     `(sh-heredoc                                       ((t (:foreground ,color-8 :bold t))))
     `(sh-quoted-exec                                   ((t (:foreground ,color-7))))
     `(show-paren-mismatch                              ((t (:foreground ,color-1 :background ,color-4 :weight bold))))
     ;; smartparens
     `(sp-show-pair-mismatch-face                       ((t (:foreground ,error :background ,color-1))))
     `(sp-show-pair-match-face                          ((t (:foreground ,info :background ,color-0))))
     ;; show-paren
     `(show-paren-match                                 ((t (:inherit sp-show-pair-match-face))))
     `(show-paren-mismatch                              ((t (:inherit sp-show-pair-mismatch-face))))
     ;; sml-mode-line
     `(sml-modeline-end-face                            ((t :inherit default :width condensed)))
     ;; SLIME
     `(slime-repl-output-face                           ((t (:foreground ,color-7))))
     `(slime-repl-inputed-output-face                   ((t (:foreground ,color-6))))
     `(slime-error-face                                 ((((supports :underline (:style wave))) (:underline (:style wave :color ,color-7))) (t (:underline ,color-7))))
     `(slime-warning-face                               ((((supports :underline (:style wave))) (:underline (:style wave :color ,color-8))) (t (:underline ,color-8))))
     `(slime-style-warning-face                         ((((supports :underline (:style wave))) (:underline (:style wave :color ,color-8))) (t (:underline ,color-8))))
     `(slime-note-face                                  ((((supports :underline (:style wave))) (:underline (:style wave :color ,color-6))) (t (:underline ,color-6))))
     `(slime-highlight-face                             ((t (:inherit highlight))))
     ;; speedbar
     `(speedbar-button-face                             ((t (:foreground ,color-8))))
     `(speedbar-directory-face                          ((t (:foreground ,color-8))))
     `(speedbar-file-face                               ((t (:foreground ,color-8))))
     `(speedbar-highlight-face                          ((t (:foreground ,color-2 :background ,color-8))))
     `(speedbar-selected-face                           ((t (:foreground ,color-7))))
     `(speedbar-separator-face                          ((t (:foreground ,color-2 :background ,color-7))))
     `(speedbar-tag-face                                ((t (:foreground ,color-8))))
     ;; tabbar
     `(tabbar-button                                    ((t (:foreground ,color-8 :background ,color-1))))
     `(tabbar-selected                                  ((t (:foreground ,color-8 :background ,color-1 :box (:line-width -1 :style pressed-button)))))
     `(tabbar-unselected                                ((t (:foreground ,color-8 :background ,color-3 :box (:line-width -1 :style released-button)))))
     ;; term
     `(term-color-black                                 ((t (:foreground ,color-2 :background ,color-1))))
     `(term-color-red                                   ((t (:foreground ,color-6 :background ,color-4))))
     `(term-color-green                                 ((t (:foreground ,color-6 :background ,color-8))))
     `(term-color-yellow                                ((t (:foreground ,color-8 :background ,color-8))))
     `(term-color-blue                                  ((t (:foreground ,color-7 :background ,color-4))))
     `(term-color-magenta                               ((t (:foreground ,color-7 :background ,color-7))))
     `(term-color-cyan                                  ((t (:foreground ,color-8 :background ,color-8))))
     `(term-color-white                                 ((t (:foreground ,color-8 :background ,color-4))))
     '(term-default-fg-color                            ((t (:inherit term-color-white))))
     '(term-default-bg-color                            ((t (:inherit term-color-black))))
     ;; undo-tree
     `(undo-tree-visualizer-active-branch-face          ((t (:foreground ,color-8 :weight bold))))
     `(undo-tree-visualizer-current-face                ((t (:foreground ,color-6 :weight bold))))
     `(undo-tree-visualizer-default-face                ((t (:foreground ,color-8))))
     `(undo-tree-visualizer-register-face               ((t (:foreground ,color-8))))
     `(undo-tree-visualizer-unmodified-face             ((t (:foreground ,color-8))))
     ;; volatile-highlights
     `(vhl/default-face                                 ((t (:background ,color-1))))
     ;; emacs-w3m
     `(w3m-anchor                                       ((t (:foreground ,color-8 :background ,color-1 :underline t :weight normal))))
     `(w3m-current-anchor                               ((t (:foreground ,color-8 :background ,color-1 :underline nil :weight bold))))
     `(w3m-arrived-anchor-face                          ((t (:foreground ,color-4 :background ,color-2 :underline t :weight normal))))
     `(w3m-image-anchor-face                            ((t (:foreground ,color-7  :background ,color-2 :underline t :weight normal))))
     `(w3m-image-face                                   ((t (:foreground ,color-8 :background ,color-2 :underline t :weight normal))))
     `(w3m-image-anchor                                 ((t (:foreground ,color-8 :background ,color-2 :underline t :weight normal))))
     `(w3m-form                                         ((t (:foreground ,color-6 :underline t))))
     `(w3m-header-line-location-title                   ((t (:foreground ,color-4 :background ,color-2 :underline nil :weight normal))))
     `(w3m-header-line-location-content                 ((t (:foreground ,color-8 :background ,color-2 :underline nil :weight normal))))
     `(w3m-history-current-url                          ((t (:inherit match))))
     `(w3m-tab-background                               ((t (:foreground ,color-1 :background ,color-6 :underline nil :weight normal))) )
     `(w3m-tab-unselected-retrieving                    ((t (:foreground ,color-1 :background ,color-6 :underline nil :weight normal))) )
     `(w3m-tab-selected-background-face                 ((t (:foreground ,color-1 :background ,color-8 :underline nil :weight bold))) )
     `(w3m-tab-selected-retrieving                      ((t (:foreground ,color-1 :background ,color-8 :underline nil :weight bold))) )
     `(w3m-lnum                                         ((t (:foreground ,color-8 :background ,color-1))))
     `(w3m-lnum-match                                   ((t (:background ,color-1 :foreground ,color-8 :weight bold))))
     `(w3m-lnum-minibuffer-prompt                       ((t (:foreground ,color-8))))
     ;; web-mode
     `(web-mode-builtin-face                            ((t (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face                            ((t (:inherit ,font-lock-comment-face))))
     `(web-mode-constant-face                           ((t (:inherit ,font-lock-constant-face))))
     `(web-mode-css-at-rule-face                        ((t (:foreground ,color-8 ))))
     `(web-mode-css-prop-face                           ((t (:foreground ,color-8))))
     `(web-mode-css-pseudo-class-face                   ((t (:foreground ,color-8 :weight bold))))
     `(web-mode-css-rule-face                           ((t (:foreground ,color-8))))
     `(web-mode-doctype-face                            ((t (:inherit ,font-lock-comment-face))))
     `(web-mode-folded-face                             ((t (:underline t))))
     `(web-mode-function-name-face                      ((t (:foreground ,color-8))))
     `(web-mode-html-attr-name-face                     ((t (:foreground ,color-8))))
     `(web-mode-html-attr-value-face                    ((t (:inherit ,font-lock-string-face))))
     `(web-mode-html-tag-face                           ((t (:foreground ,color-8))))
     `(web-mode-keyword-face                            ((t (:inherit ,font-lock-keyword-face))))
     `(web-mode-preprocessor-face                       ((t (:inherit ,font-lock-preprocessor-face))))
     `(web-mode-string-face                             ((t (:inherit ,font-lock-string-face))))
     `(web-mode-type-face                               ((t (:inherit ,font-lock-type-face))))
     `(web-mode-variable-name-face                      ((t (:inherit ,font-lock-variable-name-face))))
     `(web-mode-server-background-face                  ((t (:background ,color-1))))
     `(web-mode-server-comment-face                     ((t (:inherit web-mode-comment-face))))
     `(web-mode-server-string-face                      ((t (:inherit web-mode-string-face))))
     `(web-mode-symbol-face                             ((t (:inherit font-lock-constant-face))))
     `(web-mode-warning-face                            ((t (:inherit font-lock-warning-face))))
     `(web-mode-whitespaces-face                        ((t (:background ,color-7))))
     ;; whitespace-mode
     `(whitespace-space                                 ((t (:background ,color-3 :foreground ,color-3))))
     `(whitespace-hspace                                ((t (:background ,color-3 :foreground ,color-3))))
     `(whitespace-tab                                   ((t (:background ,color-6))))
     `(whitespace-newline                               ((t (:foreground ,color-3))))
     `(whitespace-trailing                              ((t (:background ,color-7))))
     `(whitespace-line                                  ((t (:background ,color-1 :foreground ,color-7))))
     `(whitespace-space-before-tab                      ((t (:background ,color-8 :foreground ,color-8))))
     `(whitespace-indentation                           ((t (:background ,color-8 :foreground ,color-7))))
     `(whitespace-empty                                 ((t (:background ,color-8))))
     `(whitespace-space-after-tab                       ((t (:background ,color-8 :foreground ,color-7))))
     ;; wanderlust
     `(wl-highlight-folder-few-face                     ((t (:foreground ,color-6))))
     `(wl-highlight-folder-many-face                    ((t (:foreground ,color-6))))
     `(wl-highlight-folder-path-face                    ((t (:foreground ,color-8))))
     `(wl-highlight-folder-unread-face                  ((t (:foreground ,color-8))))
     `(wl-highlight-folder-zero-face                    ((t (:foreground ,color-8))))
     `(wl-highlight-folder-unknown-face                 ((t (:foreground ,color-8))))
     `(wl-highlight-message-citation-header             ((t (:foreground ,color-6))))
     `(wl-highlight-message-cited-text-1                ((t (:foreground ,color-7))))
     `(wl-highlight-message-cited-text-2                ((t (:foreground ,color-8))))
     `(wl-highlight-message-cited-text-3                ((t (:foreground ,color-8))))
     `(wl-highlight-message-cited-text-4                ((t (:foreground ,color-8))))
     `(wl-highlight-message-header-contents-face        ((t (:foreground ,color-6))))
     `(wl-highlight-message-headers-face                ((t (:foreground ,color-8))))
     `(wl-highlight-message-important-header-contents   ((t (:foreground ,color-8))))
     `(wl-highlight-message-header-contents             ((t (:foreground ,color-7))))
     `(wl-highlight-message-important-header-contents2  ((t (:foreground ,color-8))))
     `(wl-highlight-message-signature                   ((t (:foreground ,color-6))))
     `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,color-8))))
     `(wl-highlight-summary-answered-face               ((t (:foreground ,color-8))))
     `(wl-highlight-summary-disposed-face               ((t (:foreground ,color-8 :slant italic))))
     `(wl-highlight-summary-new-face                    ((t (:foreground ,color-8))))
     `(wl-highlight-summary-normal-face                 ((t (:foreground ,color-8))))
     `(wl-highlight-summary-thread-top-face             ((t (:foreground ,color-8))))
     `(wl-highlight-thread-indent-face                  ((t (:foreground ,color-7))))
     `(wl-highlight-summary-refiled-face                ((t (:foreground ,color-8))))
     `(wl-highlight-summary-displaying-face             ((t (:underline t :weight bold))))
     ;; which-func-mode
     `(which-func                                       ((t (:foreground ,color-8))))
     ;; yascroll
     `(yascroll:thumb-text-area                         ((t (:background ,color-1))))
     `(yascroll:thumb-fringe                            ((t (:background ,color-1 :foreground ,color-1))))
     `(minimap-active-region-background                 ((t (:background ,color-3 :foreground ,color-4))))
     ;; html fold/unfold face
     `(html-fold-unfolded-face                          ((t (:background ,color-1))))
     `(html-fold-folded-face                            ((t (:foreground ,color-8 :bold t))))
     ;; markdown mode
     `(markdown-header-delimiter-face                   ((t (:weight normal :foreground ,color-3))))
     `(markdown-header-face-1                           ((,class ,ol1)))
     `(markdown-header-face-2                           ((,class ,ol2)))
     `(markdown-header-face-3                           ((,class ,ol3)))
     `(markdown-header-face-4                           ((,class ,ol4)))
     `(markdown-header-face-5                           ((,class ,ol5)))
     `(markdown-header-face-6                           ((,class ,ol6)))
     `(markdown-link-face                               ((t (:underline t :foreground ,color-8))))
     `(markdown-url-face                                ((t (:underline t :foreground ,color-8))))
     `(markdown-pre-face                                ((t (:foreground ,color-8 NOL_HEIGHT(.8)))))
     `(markdown-bold-face                               ((t (:foreground ,color-5 :bold t ))))
     `(markdown-italic-face                             ((t (:foreground ,color-5 :italic t ))))
     `(markdown-list-face                               ((t (:foreground ,color-8))))
     `(markdown-markup-face                             ((t (:foreground ,color-3 ))))
     ;; swoop
     `(swoop-face-line-buffer-name                      ((t (:background ,color-6 :foreground ,color-1))))
     `(swoop-face-target-line                           ((t (:background ,color-4 :foreground ,color-8))))
     `(swoop-face-line-number                           ((t (:background ,color-1 :foreground ,color-3))))
     `(swoop-face-header-format-line                    ((t (:background ,color-3 :foreground ,color-1))))
     `(swoop-face-target-words                          ((t (:background ,color-3 :foreground ,color-8))))
     `(highlight-indentation-face                       ((t (:background ,color-2))))
     `(highlight-indentation-current-column-face        ((t (:background ,color-1))))
     ;; company
     `(company-echo-common                              ((t (:foreground ,color-8))))
     `(company-preview                                  ((t (:background ,color-3 :foreground ,color-8))))
     `(company-preview-common                           ((t (:inherit company-preview :foreground ,color-8 :weight bold))))
     `(company-scrollbar-fg                             ((t (:background ,color-4))))
     `(company-tooltip                                  ((t (:background ,color-3 :foreground ,color-8))))
     `(company-tooltip-annotation                       ((t (:inherit company-tooltip :foreground ,color-1))))
     `(company-tooltip-common                           ((t (:inherit company-tooltip :foreground ,color-8))))
     `(company-tooltip-common-selection                 ((t (:inherit company-tooltip-selection :foreground ,color-8))))
     `(company-tooltip-selection                        ((t (:inherit company-tooltip :background ,color-5 :foreground ,color-1))))
     `(company-scrollbar-bg                             ((t (:inherit company-tooltip))))
     ;; eval-sexp-fu
     `(eval-sexp-fu-flash                               ((t (:background ,color-2))))
     `(eval-sexp-fu-flash-error                         ((t (:background ,color-8 :foreground ,color-1))))
     ;; neotree
     `(neo-dir-link-face                                ((t (:inherit diredp-dir-priv))))
     `(neo-expand-btn-face                              ((t (:foreground ,color-8 :bold t))))
     `(neo-file-link-face                               ((t (:foreground ,color-6))))
     `(neo-root-dir-face                                ((t (:foreground ,color-5 :background ,color-1))))
     ;; geiser
     `(geiser-font-lock-doc-link                        ((t (:foreground ,color-8 :underline t))))
     `(geiser-font-lock-error-link                      ((t (:foreground ,color-8 :underline t))))
     `(geiser-font-lock-autodoc-identifier              ((t (:foreground ,color-5 :bold t))))
     `(compilation-error                                ((t (:foreground ,color-8 :underline t :bold t))))
     ;; elixir
     `(elixir-atom-face                                 ((t (:foreground ,color-8 :bold t))))
     ;; tuareg
     `(tuareg-font-lock-operator-face                   ((t (:inherit ,font-lock-variable-name-face))))
     `(tuareg-font-lock-governing-face                  ((t (:inherit ,font-lock-keyword-face))))
     ;; avy
     `(avy-lead-face                                    ((t (:background ,color-5 :foreground ,color-1 :bold t))))
     `(avy-lead-face-0                                  ((t (:background ,color-6  :foreground ,color-1  :bold t))))
     `(avy-lead-face-1                                  ((t (:background ,color-7  :foreground ,color-1 :bold t))))
     `(avy-lead-face-2                                  ((t (:background ,color-8  :foreground ,color-1 :bold t))))
     `(avy-background-face                              ((t (:background ,color-1 :foreground ,color-4 :bold t))))
     ;; highlight-indent-guides has been removed: newer versions automatically compute faces.
     ;; calfw
     `(cfw:face-title                                   ((t (:height ,(eziam-heading-height 2.5) :foreground ,color-5 :weight bold))))
     `(cfw:face-grid                                    ((t (:foreground ,color-5))))
     `(cfw:face-day-title                               ((t (:foreground ,color-5))))
     `(cfw:face-header                                  ((t (:foreground ,color-6 :background ,color-2 :weight bold))))
     `(cfw:face-saturday                                ((t (:inherit 'cfw:face-header))))
     `(cfw:face-sunday                                  ((t (:inherit 'cfw:face-header))))
     `(cfw:face-holiday                                 ((t (:foreground ,color-5 :slant italic))))
     `(cfw:face-today                                   ((t (:background ,color-0))))
     `(cfw:face-today-title                             ((t (:background ,color-0 :foreground ,color-8))))
     `(cfw:face-select                                  ((,class ,highlight)))
     `(cfw:face-toolbar                                 ((t (:background ,color-1))))
     `(cfw:face-toolbar-button-on                       ((,class ,button-on)))
     `(cfw:face-toolbar-button-off                      ((,class ,button-off)))
     )

    (custom-theme-set-variables
     theme-name
     ;; pdf-tools midnight-mode
     `(pdf-view-midnight-colors ,(if (string> color-1 color-8)
                                     `(quote (,color-1 . ,color-8))
                                   `(quote (,color-8 . ,color-1))))
     ;; ansi-color
     `(ansi-color-names-vector [,color-2
                                ,color-7
                                ,color-6
                                ,color-8
                                ,color-8
                                ,color-7
                                ,color-8
                                ,color-8])
     ;; fill-column-indicator
     `(fci-rule-color ,color-1
                      ;; vc-annotate
                      `(vc-annotate-color-map
                        '(( 20. . ,color-4)
                          ( 40. . ,color-5)
                          ( 60. . ,color-5)
                          ( 80. . ,color-6)
                          (100. . ,color-6)
                          (120. . ,color-7)
                          (140. . ,color-7)
                          (160. . ,color-8)
                          (180. . ,color-8)
                          (200. . ,color-8)
                          (220. . ,color-8)
                          (240. . ,color-8)
                          (260. . ,color-8)
                          (280. . ,color-8)
                          (300. . ,color-8)
                          (320. . ,color-8)
                          (340. . ,color-8)
                          (360. . ,color-8))))
;;                      `(vc-annotate-very-old-color ,color-7)
                      `(vc-annotate-background ,color-1))
     ))

;;; eziam-light-theme.el --- Light version of the Eziam theme

;; Copyright (c) 2016-2017 Thibault Polge <thibault@thb.lt>

;; Eziam is based on Tao theme, copyright (C) 2014 Peter <11111000000
;; at email.com> with contributions by Jasonm23 <jasonm23@gmail.com>.
;; Tao also credits: "Original faces taken from Zenburn theme port (c)
;; by Bozhidar Batsov"

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintener: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: faces
;; Homepage: https://github.com/thblt/eziam-theme-emacs
;; Version: 0.4.5

;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a light version of the Eziam theme for Emacs.

;;; Code:
(deftheme eziam-light "The light Eziam color theme")

(eziam-with-color-variables
  (
   ;; Base palette
   ("color-0"                . "#ffffff")
   ("color-1"                . "#eeeeee")
   ("color-2"                . "#dddddd")
   ("color-3"                . "#cccccc")
   ("color-4"                . "#aaaaaa")
   ("color-5"                . "#888888")
   ("color-6"                . "#555555")
   ("color-7"                . "#222222")
   ("color-8"                . "#000000")
   ;; Headings
   ("ol1-fg"                 . nil)
   ("ol1-bg"                 . "#ffffff")
   ("ol2-fg"                 . nil)
   ("ol2-bg"                 . "#d6eaf4")
   ("ol3-fg"                 . nil)
   ("ol3-bg"                 . "#c1d3dc")
   ("ol4-fg"                 . nil)
   ("ol4-bg"                 . "#abbbc3")
   ("ol5-fg"                 . "#eeeeee")
   ("ol5-bg"                 . "#96a4ab")
   ("ol6-fg"                 . nil)
   ("ol6-bg"                 . "#d7d7d7")
   ("ol7-fg"                 . nil)
   ("ol7-bg"                 . nil)
   ("ol8-fg"                 . nil)
   ("ol8-bg"                 . nil)
   ;; Misc
   ("transient-highlight"    . "yellow")
   ("transient-highlight-fg" . "#000000")
   ("warning"                . "DarkOrange")
   ("error"                  . "#ff0000")
   ("info"                   . "#2244ff")
   ("ok"                     . "ForestGreen")

   ("rainbow-1"              . "#ff0000")
   ("rainbow-2"              . "#ff7700")
   ("rainbow-3"              . "goldenrod")
   ("rainbow-4"              . "#00aa00")
   ("rainbow-5"              . "#0000ff")
   ("rainbow-6"              . "#8f00ff")
   )
  (eziam-apply-custom-theme 'eziam-light))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'eziam-light)

;; Local Variables:
;; mode: emacs-lisp;
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; eziam-light-theme.el ends here


