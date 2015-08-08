;;; evenhold-theme.el --- Custom theme for faces

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Franz Victorio &lt;evenhold@gmail.com&gt;

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

;;; Code:

(deftheme evenhold
  "Face colors using a dark blue background.")

(let ((class '((class color) (min-colors 209))))
  (custom-theme-set-faces
   'evenhold
   `(Info-title-1-face ((,class (:family "helv" :weight bold :height 1.728))))
   `(Info-title-2-face ((,class (:family "helv" :weight bold :height 1.44))))
   `(Info-title-3-face ((,class (:family "helv" :weight bold :height 1.2))))
   `(Info-title-4-face ((,class (:family "helv" :weight bold))))
   `(compilation-column-number ((,class (:foreground "LightGreen"))))
   `(compilation-error ((,class (:foreground "Red1"))))
   `(compilation-info ((,class (:weight normal :foreground "LightSkyBlue"))))
   `(compilation-line-number ((,class (:foreground "LightGreen"))))
   `(compilation-mode-line-exit ((,class (:foreground "blue4"))))
   `(cperl-array-face ((,class (:foreground "yellow2"))))
   `(cperl-hash-face ((,class (:foreground "green"))))
   `(cursor ((,class (:background "#34ADF2"))))
   `(default ((,class (:background "#000000" :foreground "#D0D0D0"))))
   `(diff-added ((,class (nil))))
   `(diff-changed ((,class (nil))))
   `(diff-context ((,class (:foreground "seashell4"))))
   `(diff-file-header ((,class (:background "grey60"))))
   `(diff-function ((,class (:inherit diff-header))))
   `(diff-header ((,class (:background "grey45"))))
   `(diff-hunk-header ((,class (:inherit diff-header))))
   `(diff-index ((,class (:inherit diff-file-header))))
   `(diff-indicator-added ((,class (:foreground "white" :background "darkolivegreen"))))
   `(diff-indicator-changed ((,class (:foreground "white" :background "dodgerblue4"))))
   `(diff-indicator-removed ((,class (:foreground "white" :background "indianred4"))))
   `(diff-refine-change ((,class (:background "skyblue4"))))
   `(diff-removed ((,class (nil))))
   `(dired-marked ((,class (:background "dodgerblue3" :foreground "white"))))
   `(ediff-current-diff-A ((,class (:background "green4" :foreground "white"))))
   `(ediff-current-diff-B ((,class (:background "darkorange3" :foreground "white"))))
   `(ediff-even-diff-B ((,class (:background "Grey50" :foreground "White"))))
   `(ediff-fine-diff-A ((,class (:background "skyblue4" :foreground "white"))))
   `(ediff-fine-diff-B ((,class (:background "cyan4" :foreground "white"))))
   `(ediff-odd-diff-A ((,class (:background "Grey50" :foreground "White"))))
   `(error ((,class (:foreground "#BB0000"))))
   `(flymake-errline ((,class (:background nil :underline "red"))))
   `(flymake-warnline ((,class (:background nil :underline "green"))))
   `(font-lock-builtin-face ((,class (:foreground "#FF983C" :weight bold))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#A5A6A6"))))
   `(font-lock-comment-face ((,class (:foreground "#626262"))))
   `(font-lock-constant-face ((,class (:foreground "#C61A1A"))))
   `(font-lock-doc-face ((,class (:foreground "#EFEFEF"))))
   `(font-lock-doc-string-face ((,class (:foreground "moccasin"))))
   `(font-lock-function-name-face ((,class (:foreground "#32BAFF" ))))
   `(font-lock-keyword-face ((,class (:foreground "#8600AF" ))))
   `(font-lock-preprocessor-face ((,class (:foreground "#007DC5"))))
   `(font-lock-reference-face ((,class (:foreground "green"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground "#0097BD"))))
   `(font-lock-type-face ((,class (:foreground "#FF983C" :weight bold ))))
   `(font-lock-variable-name-face ((,class (:foreground "#53E3FF" :weight bold))))
   `(fringe ((,class (:background "#000000"))))
   `(highlight ((,class (:background "DodgerBlue4"))))
   `(ido-first-match ((,class (:weight normal :foreground "orange"))))
   `(ido-only-match ((,class (:foreground "green"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   `(info-header-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-header-xref ((,class (:foreground "SeaGreen2"))))
   `(info-menu-header ((,class (:family "helv" :weight bold))))
   `(info-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-xref ((,class (:foreground "SeaGreen2"))))
   `(isearch ((,class (:background "coral2" :foreground "white"))))
   `(isearch-lazy-highlight-face ((,class (:background "coral4" :foreground "white"))))
   `(lazy-highlight ((,class (:background "cadetblue" :foreground "white"))))
   `(match ((,class (:background "DeepPink4"))))
   `(minibuffer-prompt ((,class (:foreground "#00A2CF"))))
   `(mode-line ((,class (:background "#004A63" :foreground "#F2F2F2" :box (:line-width 1 :color "#004A63":style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold :background nil :foreground "#2CF5FF" ))))
   `(mode-line-inactive ((,class (:background "black" :foreground "#00A3CC" :box (:line-width 1 :color "#009AC0" :style nil)))))
   `(outline-1 ((,class (:foreground "SkyBlue1"))))
   `(outline-2 ((,class (:foreground "CadetBlue1"))))
   `(outline-3 ((,class (:foreground "LightSteelBlue1"))))
   `(outline-4 ((,class (:foreground "turquoise2"))))
   `(outline-5 ((,class (:foreground "aquamarine1"))))
   `(primary-selection ((,class (:background "blue3"))))
   `(region ((,class (:background "#103050"))))
   `(show-paren-match-face ((,class (:background "dodgerblue1" :foreground "white"))))
   `(show-paren-mismatch-face ((,class (:background "red" :foreground "white"))))
   `(success ((,class (:foreground "SeaGreen2"))))
   `(warning ((,class (:foreground "Yellow"))))))

(provide-theme 'evenhold)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; evenhold-theme.el ends here
