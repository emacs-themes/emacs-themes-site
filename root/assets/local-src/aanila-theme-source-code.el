;;; aanlia-theme.el --- A moderately dark theme

;; Copyright (C) 2015 by Santosh Sivaraj

;; Author: Santosh Sivaraj <santosh@fossix.org>
;; URL: https://github.com/santoshs/aanila
;; Version: 0.01

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

;;; Code:

(deftheme aanila
  "aanila theme")

(custom-theme-set-faces
 'aanila

 '(default ((t (:background "black" :foreground "lightgray"))))
 '(background-toolbar-color ((t (:foregroung "#000"))))
 '(border ((t (:foreground "black"))))
 '(bottom-toolbar-shadow-color ((t (:foreground "black"))))
 '(cursor ((t (:foreground "black"))))
 '(top-toolbar-shadow-color ((t (:foreground "#111"))))
 '(modeline ((t (:background "gray30" :foreground "goldenrod"))))
 '(modeline-inactive ((t (:background "gray5" :foreground "gray40"))))
 '(modeline-buffer-id ((t (:foreground "dark green"
                                       :box (:line-width 1 :style none)))))
 '(modeline-mousable ((t (:background "#000" :foreground "#555"))))
 '(modeline-mousable-minor-mode ((t (:background "#000" :foreground "#555"))))
 '(which-func ((t (:inherit mode-line))))
 '(fringe ((t (:background "#111" :foreground "#444"))))
 '(linum ((t (:background "gray10" :foreground "dim gray"))))
 '(region ((t (:foreground "cyan" :background "dark cyan"))))
 '(show-paren-match ((t (:background "gray12"))))
 '(highlight-changes ((t (:foreground nil :background "midnight blue"))))
 '(highlight-changes-delete ((t (:foreground nil :background "chocolate4" :underline nil))))
 ;; All font locks
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-face ((t (:foreground "peachpuff3" :italic t :background "gray9"))))
 '(font-lock-constant-face ((t (:foreground "indianred"))))
 '(font-lock-doc-string-face ((t (:foreground "SeaGreen2" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "SkyBlue" :bold t))))
 '(font-lock-preprocessor-face ((t (:foreground "sky blue" :background "gray10"))))
 '(font-lock-reference-face ((t (:foreground "blue"))))
 '(font-lock-string-face ((t (:foreground "DarkSlateGray2"))))
 '(font-lock-function-name-face ((t (:foreground "steelblue"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-name-face ((t (:foreground "DarkOrange"))))
 '(isearch ((t (:foreground "red4" :background "CadetBlue4"))))
 '(underline ((t (:underline t))))
 '(italic ((t (:italic t))))
 '(bold-italic ((t (:bold t :italic t)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'aanila)
