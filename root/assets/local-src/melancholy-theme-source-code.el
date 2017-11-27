;;; melancholy-theme.el --- A dark theme for dark minds

;; Copyright (C) 2016 Sod Oscarfono

;; Author: Sod Oscarfono <sod@oscarfono.com>
;; URL: http://github.com/techquila/melancholy-theme
;; Version: 1.6
;; Package-requires:

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

;; A dark theme  for dark minds.  > Emacs 24

;;; Code:

(deftheme melancholy  "A dark theme for dark minds")

;;;; Theme Faces

(custom-theme-set-faces
  'melancholy

;; Info-quoted
;; bold
;; bold-italic
;; bookmark-menu-bookmark
;; bookmark-menu-heading
;; border
;; buffer-menu-buffer
'(button ((t (:underline (:color foreground-color :style line) :foreground "#F92672"))))
;; calendar-month-header
;; calendar-today
;; calendar-weekday-header
;; calendar-weekend-header
;; comint-highlight-input
;; comint-highlight-prompt
;; company-echo
;; company-echo-common
;; company-preview
;; company-preview-common
;; company-preview-search
;; company-scrollbar-bg
;; company-scrollbar-fg
;; company-template-field
;; company-tooltip
;; company-tooltip-annotation
;; company-tooltip-annotation-selection
;; company-tooltip-common
;; company-tooltip-common-selection
;; company-tooltip-mouse
;; company-tooltip-search
;; company-tooltip-search-selection
;; company-tooltip-selection
;; compilation-column-number
;; compilation-error
;; compilation-info
;; compilation-line-number
;; compilation-mode-line-exit
;; compilation-mode-line-fail
;; compilation-mode-line-run
;; compilation-warning
;; completions-annotations
;; completions-common-part
;; completions-first-difference
'(cursor ((t (:background "light blue"))))
'(default ((t (:inherit nil :stipple nil :background "#161A1F" :foreground "#DBDBDB" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "unknown" :family "DejaVu sans mono"))))
;; diary
;; diff-added
;; diff-changed
;; diff-context
;; diff-file-header
;; diff-function
;; diff-header
;; diff-hunk-header
;; diff-index
;; diff-indicator-added
;; diff-indicator-changed
;; diff-indicator-removed
;; diff-nonexistent
;; diff-refine-added
;; diff-refine-changed
;; diff-refine-removed
;; diff-removed
;; dired-directory
;; dired-flagged
;; dired-header
;; dired-ignored
;; dired-mark
;; dired-marked
;; dired-perm-write
;; dired-symlink
;; dired-warning
;; eldoc-highlight-function-argument
;; emmet-preview-input
;; emmet-preview-output
;; epa-field-body
;; epa-field-name
;; epa-mark
;; epa-string
;; epa-validity-disabled
;; epa-validity-high
;; epa-validity-low
;; epa-validity-medium
;; error
'(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
;; ffap
;; file-name-shadow
'(fixed-pitch ((t (:family "Monospace"))))
'(font-lock-builtin-face ((t (:foreground "#96BF33"))))
'(font-lock-comment-delimiter-face ((t (:foreground "#8C8C8C"))))
'(font-lock-comment-face ((t (:foreground "#8C8C8C"))))
'(font-lock-constant-face ((t (:foreground "#DFAF8F"))))
'(font-lock-doc-face ((t (:foreground "#FFB728"))))
'(font-lock-function-name-face ((t (:foreground "#00BFFF"))))
'(font-lock-keyword-face ((t (:foreground "#F92672" :height 160 :weight extra-bold))))
'(font-lock-negation-char-face ((t (:foreground "#F37DEE"))))
'(font-lock-preprocessor-face ((t (:foreground "#F92672"))))
'(font-lock-regexp-grouping-backslash ((t (:foreground "#A63A62"))))
'(font-lock-regexp-grouping-construct ((t (:foreground "#A63A62"))))
'(font-lock-string-face ((t (:foreground "#F37DEE" :slant italic :weight extra-light))))
'(font-lock-type-face ((t (:foreground "#96BFF0"))))
'(font-lock-variable-name-face ((t (:foreground "#96BF33"))))
'(font-lock-warning-face ((t (:foreground "#FF6969"))))
'(fringe ((t (:background "#161A1F"))))
;; glyphless-char
;; gnus-group-mail-1
;; gnus-group-mail-1-empty
;; gnus-group-mail-2
;; gnus-group-mail-2-empty
;; gnus-group-mail-3
;; gnus-group-mail-3-empty
;; gnus-group-mail-low
;; gnus-group-mail-low-empty
;; gnus-group-news-1
;; gnus-group-news-1-empty
;; gnus-group-news-2
;; gnus-group-news-2-empty
;; gnus-group-news-3
;; gnus-group-news-3-empty
;; gnus-group-news-4
;; gnus-group-news-4-empty
;; gnus-group-news-5
;; gnus-group-news-5-empty
;; gnus-group-news-6
;; gnus-group-news-6-empty
;; gnus-group-news-low
;; gnus-group-news-low-empty
;; gnus-splash
;; gnus-summary-cancelled
;; gnus-summary-high-ancient
;; gnus-summary-high-read
;; gnus-summary-high-ticked
;; gnus-summary-high-undownloaded
;; gnus-summary-high-unread
;; gnus-summary-low-ancient
;; gnus-summary-low-read
;; gnus-summary-low-ticked
;; gnus-summary-low-undownloaded
;; gnus-summary-low-unread
;; gnus-summary-normal-ancient
;; gnus-summary-normal-read
;; gnus-summary-normal-ticked
;; gnus-summary-normal-undownloaded
;; gnus-summary-normal-unread
;; gnus-summary-selected
'(header-line ((t (:foreground "#DEDEDE" :background "#333333"))))
;; helm-M-x-key
;; helm-action
;; helm-bookmark-addressbook
;; helm-bookmark-directory
;; helm-bookmark-file
;; helm-bookmark-gnus
;; helm-bookmark-info
;; helm-bookmark-man
;; helm-bookmark-w3m
;; helm-buffer-directory
;; helm-buffer-file
;; helm-buffer-not-saved
;; helm-buffer-process
;; helm-buffer-saved-out
;; helm-buffer-size
;; helm-candidate-number
;; helm-etags-file
;; helm-ff-directory
;; helm-ff-dirs
;; helm-ff-dotted-directory
;; helm-ff-dotted-symlink-directory
;; helm-ff-executable
;; helm-ff-file
;; helm-ff-invalid-symlink
;; helm-ff-prefix
;; helm-ff-symlink
;; helm-grep-cmd-line
;; helm-grep-file
;; helm-grep-finish
;; helm-grep-lineno
;; helm-grep-match
;; helm-header
;; helm-header-line-left-margin
;; helm-helper
;; helm-history-deleted
;; helm-history-remote
;; helm-lisp-completion-info
;; helm-lisp-show-completion
;; helm-locate-finish
;; helm-match
;; helm-match-item
;; helm-moccur-buffer
;; helm-prefarg
;; helm-resume-need-update
;; helm-selection
;; helm-selection-line
;; helm-separator
;; helm-source-header
;; helm-visible-mark
;; help-argument-name
'(highlight ((t (:background "#151515"))))
;; holiday
;; info-header-node
;; info-header-xref
;; info-index-match
;; info-menu-header
;; info-menu-star
;; info-node
;; info-title-1
;; info-title-2
;; info-title-3
;; info-title-4
;; info-xref
;; info-xref-visited
'(isearch ((t (:background "#96BF33" :foreground "#444444"))))
'(isearch-fail ((t (:background "#00BFFF"))))
;; italic
'(lazy-highlight ((t (:foreground "#666" :background "#96BFF0"))))
'(link ((t (:foreground "#96BF33"))))
'(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
;; linum
'(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "#00BFFF")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "#00BFFF")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
;; menu
'(minibuffer-prompt ((t (:foreground "#00BFFF"))))
'(mode-line ((t (:foreground "#00BFFF" :background "#333333"))))
'(mode-line-buffer-id ((t (:weight bold))))
'(mode-line-emphasis ((t (:weight bold))))
'(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
'(mode-line-inactive ((t (:foreground "#555555" :background "#222222"))))
;; mouse
'(next-error ((t (:inherit (region)))))
;; nobreak-space
;; org-agenda-calendar-event
;; org-agenda-calendar-sexp
;; org-agenda-clocking
;; org-agenda-column-dateline
;; org-agenda-current-time
;; org-agenda-date
;; org-agenda-date-today
;; org-agenda-date-weekend
;; org-agenda-diary
;; org-agenda-dimmed-todo-face
;; org-agenda-done
;; org-agenda-filter-category
;; org-agenda-filter-regexp
;; org-agenda-filter-tags
;; org-agenda-restriction-lock
;; org-agenda-structure
;; org-archived
'(org-block ((t (:family "DejaVu sans mono" :foreground "#00BFFF" :box nil))))
'(org-block-begin-line ((t (:background "#444444" :foreground "#00BFFF"))))
'(org-block-end-line ((t (:background "#444444" :foreground "#00BFFF"))))
;; org-checkbox
;; org-checkbox-statistics-done
;; org-checkbox-statistics-todo
;; org-clock-overlay
'(org-code ((t (:family "DejaVu sans mono" ))))
;; org-column
;; org-column-title
;; org-date
;; org-date-selected
;; org-default
'(org-document-info ((t (:foreground "#00BFFF" :height 1.25 ))))
;; org-document-info-keyword
'(org-document-title ((t (:foreground "#00BFFF" :height 1.75 :weight extra-bold ))))
;; org-done
;; org-drawer
;; org-ellipsis
;; org-footnote
;; org-formula
;; org-headline-done
;; org-hide
;; org-latex-and-related
'(org-level-1 ((t  :height 1.25 :weight bold)))
'(org-level-2 ((t  :foreground "#888888" :height 1.15 )))
'(org-level-3 ((t  :foreground "#888888" )))
'(org-level-4 ((t  )))
'(org-level-5 ((t  )))
'(org-level-6 ((t  )))
'(org-level-7 ((t  )))
'(org-level-8 ((t  )))
'(org-headline-done ((t (:foreground "#96BF33" :strike-through t))))
'(org-done ((t (:foreground "#96BF33" :strike-through t))))
'(org-link ((t (:foreground "#F92672" ))))
;; org-list-dt
;; org-macro
;; org-meta-line
;; org-mode-line-clock
;; org-mode-line-clock-overrun
;; org-priority
;; org-property-value
;; org-quote
;; org-scheduled
;; org-scheduled-previously
;; org-scheduled-today
;; org-sexp-date
;; org-special-keyword
'(org-table ((t :family "Monospace")))
;; org-tag
;; org-tag-group
;; org-target
;; org-time-grid
;; org-todo
;; org-upcoming-deadline
;; org-verbatim
;; org-verse
;; org-warning
;; outline-1
;; outline-2
;; outline-3
;; outline-4
;; outline-5
;; outline-6
;; outline-7
;; outline-8
;; package-description
;; package-help-section-name
;; package-name
;; package-status-avail-obso
;; package-status-available
;; package-status-built-in
;; package-status-dependency
;; package-status-disabled
;; package-status-external
;; package-status-held
;; package-status-incompat
;; package-status-installed
;; package-status-new
;; package-status-unsigned
'(query-replace ((t (:inherit isearch))))
'(region ((t (:background "#555555"))))
;; scroll-bar
'(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
;; shadow
;; show-paren-match
;; show-paren-mismatch
;; sp-pair-overlay-face
;; sp-show-pair-enclosing
;; sp-show-pair-match-face
;; sp-show-pair-mismatch-face
;; sp-wrap-overlay-closing-pair
;; sp-wrap-overlay-face
;; sp-wrap-overlay-opening-pair
'(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
;; success
;; tool-bar
'(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "light yellow")) (t (:inherit (variable-pitch)))))
'(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
;; tty-menu-disabled-face
;; tty-menu-enabled-face
;; tty-menu-selected-face
;; underline
;; undo-tree-visualizer-active-branch-face
;; undo-tree-visualizer-current-face
;; undo-tree-visualizer-default-face
;; undo-tree-visualizer-register-face
;; undo-tree-visualizer-unmodified-face
'(variable-pitch ((t (:family "DejaVu sans")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'melancholy)
;;; melancholy-theme.el ends here
