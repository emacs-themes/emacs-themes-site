;;; faff-theme.el --- Light Emacs color theme on ivory3 background

;; Copyright (C) 2003-2014 Free Software Foundation, Inc.

;; Author: James Ferguson <(concat "wjcferguson" at-sign "gmail.com")>
;; URL: https://github.com/WJCFerguson/emacs-faff-theme
;; Version: 20140206.1101
;; Keywords: color theme

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The default Emacs theme with an ivory3 background, with tweaks applied.  Used
;; mostly for coding and magit, with some customizations for org, powerline,
;; hl-line
;;
;; This file created using customize-create-theme, rather than hand-rolled.
;; Comments and change suggestions welcome
;;
;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'faff t)
;;
;; Requirements: Emacs 24.

;;; Code:

(deftheme faff
  "Emacs default with ivory3 background and a few tweaks")

(custom-theme-set-faces
 'faff
 '(default ((t (:foreground "black" :background "ivory3"))))
 '(cursor ((t (:background "red3"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(highlight ((t (:background "white"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "blue3")) (((class color) (min-colors 88) (background light) (type gtk)) (:background "gtk_selection_bg_color" :foreground "gtk_selection_fg_color")) (((class color) (min-colors 88) (background light) (type ns)) (:background "ns_selection_color")) (((class color) (min-colors 88) (background light)) (:background "lightgoldenrod2")) (((class color) (min-colors 16) (background dark)) (:background "blue3")) (((class color) (min-colors 16) (background light)) (:background "lightgoldenrod2")) (((class color) (min-colors 8)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "gray90"))))
 '(erc-notice-face ((t (:foreground "gray65"))))
 '(erc-timestamp-face ((t (:foreground "white" :weight bold))))
 '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Orchid")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#b00"))))
 '(font-lock-constant-face ((t (:foreground "blue3"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue4"))))
 '(font-lock-keyword-face ((t (:foreground "black" :weight bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "darkgreen"))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#91b"))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(ein:cell-input-area ((t (:background "ivory2"))) t)
 '(fringe ((t (:inherit default :background "ivory3"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:background "gold" :foreground "black" :box nil))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey40" :foreground "black" :box nil :weight light))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(calendar-today ((t (:background "yellow1" :underline t))))
 '(custom-button ((((type x w32 mac) (class color)) (:background "grey85" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(diff-added ((t (:inherit diff-changed :foreground "green3"))))
 '(diff-file-header ((t (:background "grey80" :box (:line-width 2 :color "grey80") :weight bold))))
 '(diff-header ((t (:background "grey80" :box (:line-width 2 :color "grey80")))))
 '(diff-hunk-header ((t (:inherit diff-header :box (:line-width 2 :color "grey80")))))
 '(diff-removed ((t (:foreground "red"))))
 '(dired-directory ((t (:inherit font-lock-keyword-face))))
 '(dired-filetype-compress ((t (:foreground "Orchid"))))
 '(dired-filetype-execute ((t (:foreground "green4" :weight bold))))
 '(dired-filetype-omit ((t (:foreground "gray50"))))
 '(dired-filetype-source ((t (:foreground "red4" :weight bold))))
 '(dired-filetype-video ((t (:foreground "brown"))))
 '(flymake-warnline ((t (:background "LightBlue3"))))
 '(highlight-indentation-face ((t (:inherit fringe :background "ivory3"))))
 '(hl-line ((t (:background "ivory2"))))
 '(jabber-activity-face ((t (:background "green1"))))
 '(jabber-chat-error ((t (:background "pink"))))
 '(jabber-chat-prompt-system ((t (:foreground "green3" :weight bold))))
 '(jabber-chat-text-local ((t (:foreground "red4"))))
 '(jabber-title-large ((t (:weight bold :height 2.0 :width expanded))))
 '(jabber-title-medium ((t (:background "#ffb" :box (:line-width 2 :color "grey75" :style released-button) :weight bold :height 1.2 :width expanded))))
 '(helm-M-x-key ((t (:foreground "orange4" :underline t))))
 '(helm-ff-executable ((t (:foreground "darkgreen" :weight bold))))
 '(helm-ff-symlink ((t (:foreground "orange4"))))
 '(helm-grep-lineno ((t (:foreground "orange4"))))
 '(helm-source-header ((t (:background "ivory2" :foreground "black" :box (:line-width 1 :color "grey75" :style pressed-button) :weight normal :height 1.0 :family "Sans Serif"))))
 '(magit-branch ((t (:inherit magit-header :background "yellow" :box (:line-width 1 :color "grey75" :style released-button)))))
 '(magit-branch-current ((t (:inherit magit-branch-local :background "green1" :box 1))))
 '(magit-branch-local ((t (:background "yellow" :box (:line-width 1 :color "*")))))
 '(magit-branch-remote ((t (:background "ivory2" :foreground "DarkOliveGreen4" :box (:line-width 1 :color "gray25")))))
 '(magit-diff-add ((t (:inherit diff-added))))
 '(magit-diff-del ((t (:inherit diff-removed))))
 '(magit-diff-none ((t (:inherit diff-context))))
 '(magit-header ((t (:inherit header-line :background "white"))))
 '(magit-item-highlight ((t (:inherit highlight))))
 '(magit-refname ((t (:foreground "grey30" :box (:line-width 2 :color "grey75")))))
 '(magit-section-heading ((t (:background "ivory2" :box (:line-width 1 :color "grey75" :style released-button) :weight bold))))
 '(magit-section-title ((t (:inherit magit-header :box (:line-width 1 :color "grey75" :style released-button)))))
 '(magit-tag ((t (:background "ivory2" :foreground "Goldenrod4" :box (:line-width 1 :color "gray25")))))
 '(menu ((((type x-toolkit)) (:background "gray92"))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :background "white" :box (:line-width 1 :color "grey75" :style pressed-button) :height 1.0))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :slant italic :weight bold :height 1.0))))
 '(org-agenda-dimmed-todo-face ((t (:background "yellow3" :foreground "black"))))
 '(org-agenda-done ((((class color) (min-colors 16) (background light)) (:foreground "#9b9"))))
 '(org-agenda-structure ((t (:background "white" :foreground "Blue3" :box (:line-width 1 :color "grey75" :style pressed-button)))))
 '(org-hide ((((background light)) (:foreground "ivory2"))))
 '(outline-1 ((t (:weight bold))))
 '(outline-2 ((t (:foreground "red4"))))
 '(outline-3 ((t (:foreground "purple4"))))
 '(outline-4 ((t (:inherit font-lock-variable-name-face))))
 '(powerline-active1 ((t (:inherit mode-line :background "gold4"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gold3"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "grey11" :foreground "grey45"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey20" :foreground "grey55"))))
 '(scroll-bar ((t (:background "grey90"))))
 '(sh-heredoc ((t (:foreground "tan4"))))
 '(success ((t (:foreground "darkgreen" :weight bold))))
 '(tool-bar ((default (:foreground "black" :box (:line-width 1 :style released-button))) (((type x w32 mac) (class color)) (:background "grey92"))))
 '(warning ((t (:foreground "DarkOrange3" :weight bold)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'faff)
;;; faff-theme.el ends here
