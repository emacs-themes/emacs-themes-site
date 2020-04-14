;;; arc-dark-theme.el ---Arc dark theme              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Christopher Fraser

;; Author: Christopher Fraser <cfraz89@gmail.com>
;; Version: 0.1.0
;; Keywords:faces, theme
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/cfraz89/arc-dark-theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A theme to match ark-dark for gtk. Based on atom-one-dark

;;; Code:


(deftheme arc-dark
  "Based on atom-one-dark, basic colors adapted for arc-dark gtk theme")

(custom-theme-set-variables
 'arc-dark
 '(ansi-color-names-vector ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"]))

(custom-theme-set-faces
 'arc-dark
 '(cursor ((t (:background "white"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#9DA5B4"))))
 '(highlight ((t (:background "#4e5868"))))
 '(region ((t (:background "#3c4450"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50"))
           (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70"))
           (((class color) (min-colors 8) (background light)) (:foreground "green"))
           (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:background "#121417"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1"))
                        (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "#569cd6"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#5C6370"))))
 '(font-lock-constant-face ((t (:foreground "#569cd6"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#dcdcaa"))))
 '(font-lock-keyword-face ((t (:foreground "#9cdcfe" :weight normal))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground " #dcdcaa"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#7cb461"))))
 '(font-lock-type-face ((t (:foreground "#569cd6"))))
 '(font-lock-variable-name-face ((t (:foreground "#fff"))))
 '(font-lock-warning-face ((t (:foreground "#5C6370" :bold t))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "#61AFEF" :underline t :weight bold))))
 '(link-visited ((t (:foreground "#61AFEF" :underline t :weight normal))))
 '(fringe ((t (:background "#21252b"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil))
                (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90"))
                (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20"))
                (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white"))
                (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:background "#404e64" :foreground "white" :box (:line-width 1 :color "#181A1F")))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:background "#262a33" :foreground "#aaa" :box (:color "#181A1F" :line-width 1)))))
 '(isearch ((t (:foreground "#282C34" :background "#C678DD"))))
 '(isearch-fail ((t (:foreground "#BE5046" :background nil))))
 '(lazy-highlight ((t (:foreground "#C678DD" :background "#121417" :underline "#C678DD"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1"))
          (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3"))
          (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow"))
          (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue"))
          (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(success ((t (:foreground "#98C379"))))
 '(warning ((t (:foreground "#E5C07B"))))
 '(error ((t (:foreground "#E06C75" :weight bold))))
 '(vertical-border ((t (:background "#181A1F" :foreground "#181A1F"))))
 '(window-divider ((t (:foreground "#181A1F"))))
 '(window-divider-first-pixel ((t (:foreground "#181A1F"))))
 '(window-divider-last-pixel ((t (:foreground "#181A1F"))))
 '(company-tooltip ((t (:foreground "#ABB2BF" :background "#121417"))))
 '(company-tooltip-annotation ((t (:foreground "#828997" :background "#121417"))))
 '(company-tooltip-selection ((t (:foreground "#ABB2BF" :background "#3E4451"))))
 '(company-tooltip-mouse ((t (:background "#3E4451"))))
 '(company-tooltip-common ((t (:foreground "#E5C07B" :background "#121417"))))
 '(company-tooltip-common-selection ((t (:foreground "#E5C07B" :background "#3E4451"))))
 '(company-preview ((t (:background "#282C34"))))
 '(company-preview-common ((t (:foreground "#E5C07B" :background "#282C34"))))
 '(company-scrollbar-fg ((t (:background "#ABB2BF"))))
 '(company-scrollbar-bg ((t (:background "#121417"))))
 '(flymake-error ((t (:underline (:color "#E06C75" :style wave)))))
 '(flymake-note ((t (:underline (:color "#98C379" :style wave)))))
 '(flymake-warning ((t (:underline (:color "#D19A66" :style wave)))))
 '(flycheck-error ((t (:underline (:color "#E06C75" :style wave)))))
 '(flycheck-info ((t (:underline (:color "#98C379" :style wave)))))
 '(flycheck-warning ((t (:underline (:color "#D19A66" :style wave)))))
 '(compilation-line-number ((t (:foreground "#828997"))))
 '(compilation-column-number ((t (:foreground "#828997"))))
 '(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
 '(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))
 '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
 '(dired-flagged ((t (:inherit (diff-hl-delete)))))
 '(dired-symlink ((t (:foreground "#FD5FF1"))))
 '(info-menu-star ((t (:foreground "#E06C75"))))
 '(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground "#98C379"))))
 '(ivy-current-match ((t (:background "#3E4451" :weight normal))))
 '(ivy-highlight-face ((t (:inherit font-lock-builtin-face))))
 '(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground "#E06C75"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "#2C323C"))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background "#21252B" :foreground "#C678DD" :weight semi-bold))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2 :background "#21252B" :foreground "#98C379" :weight semi-bold))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2 :background "#21252B" :foreground "#E5C07B" :weight semi-bold))))
 '(ivy-minibuffer-match-highlight ((t (:foreground "#61AFEF"))))
 '(ivy-modified-buffer ((t (:inherit default :foreground "#D19A66"))))
 '(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))
 '(counsel-key-binding ((t (:foreground "#E5C07B" :weight bold))))
 '(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
 '(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
 '(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
 '(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#61AFEF"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#98C379"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#D19A66"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#56B6C2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#C678DD"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#E5C07B"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#61AFEF"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#98C379"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#D19A66"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#E06C75" :weight bold))))
 '(line-number ((t (:foreground "#4B5363" :background "#21252b"))))
 '(line-number-current-line ((t (:foreground "#bcc3cf" :background "#21252b"))))
 '(undo-tree-visualizer-current-face ((t (:foreground "#E06C75"))))
 '(undo-tree-visualizer-register-face ((t (:foreground "#D19A66"))))
 '(undo-tree-visualizer-unmodified-face ((t (:foreground "#56B6C2"))))
 '(default ((t (:background "#21252b" :foreground "white")))))

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))

(provide-theme 'arc-dark)

(provide 'arc-dark-theme)

;;; arc-dark-theme.el ends here
