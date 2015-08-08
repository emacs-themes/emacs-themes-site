;;; tommyh-theme.el --- A bright, bold-colored theme for emacs

;; Copyright (c) 2013 William Glass

;; Author: William Glass &lt;william.glass@gmail.com&gt;
;; Version: 1.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

;;; Credits:

;; The organization of the code is borrowed heavily from the zenburn-theme
;; package by Bozhidar Batsov.

;;; Code:

(deftheme tommyh
  "Color theme based on a certain fashion designer named Tommy H.")

(defvar tommyh-colors-alist
    '(("tommyh-bright-fire"   . "#f62b4d")
      ("tommyh-burnt"         . "#b8574e")
      ("tommyh-salmon"        . "#f8978e")
      ("tommyh-pink-salmon"   . "#ff9c9f")
      ("tommyh-light-green"   . "#c5ffc5")
      ("tommyh-deep-green"    . "#499e4d")
      ("tommyh-seafoam"       . "#449999")
      ("tommyh-pastel-blue"   . "#acd4ea")
      ("tommyh-pastel-blue2"  . "#8ac5e0")
      ("tommyh-pastel-blue3"  . "#74a6bd")
      ("tommyh-pastel-blue4"  . "#4d96b8")
      ("tommyh-pastel-blue5"  . "#5377ad")
      ("tommyh-dark-blue"     . "#19346b")
      ("tommyh-almost-black"  . "#0d1b37")
      ("tommyh-gunmetal"      . "#bfbebf"))
    )

(defmacro tommyh-with-colors (&amp;rest body)
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   tommyh-colors-alist))
     ,@body))

(tommyh-with-colors
 (custom-theme-set-faces
  'tommyh
  `(default ((t (:foreground ,tommyh-almost-black :background "white"))))
;; cperl
  `(cperl-array-face ((t (:foreground ,tommyh-deep-green :slant italic :weight bold))))
  `(cperl-hash-face ((t (:foreground ,tommyh-deep-green :underline t :weight bold))))
  `(cperl-nonoverridable-face ((((class color) (background light)) (:foreground ,tommyh-pastel-blue5 :slant italic))))
  ;; diffs
  `(diff-added ((t (:inherit diff-changed :background ,tommyh-light-green))))
  `(diff-file-header ((t (:foreground ,tommyh-pastel-blue5 :weight bold))))
  `(diff-function ((t (:foreground ,tommyh-salmon))))
  `(diff-header ((t nil)))
  `(diff-removed ((t (:background ,tommyh-pink-salmon))))
  ;; erc
  `(erc-current-nick-face ((t (:foreground ,tommyh-burnt :weight bold))))
  `(erc-input-face ((t (:foreground ,tommyh-almost-black))))
  `(erc-my-nick-face ((t (:foreground ,tommyh-pastel-blue5 :weight bold))))
  `(erc-nick-default-face ((t (:weight bold))))
  `(erc-notice-face ((t (:foreground ,tommyh-pastel-blue4 :slant italic :weight normal))))
  `(erc-prompt-face ((t (:foreground ,tommyh-burnt :slant italic :weight bold))))
  ;; font lock
  `(font-lock-keyword-face ((t (:foreground ,tommyh-burnt :weight bold))))
  `(font-lock-type-face ((t (:foreground ,tommyh-almost-black :weight bold))))
  `(font-lock-comment-face ((t (:foreground ,tommyh-bright-fire :slant italic))))
  `(font-lock-string-face ((t (:foreground ,tommyh-pastel-blue4))))
  `(font-lock-function-name-face ((t (:foreground ,tommyh-pastel-blue5 :weight bold))))
  `(font-lock-variable-name-face ((t (:foreground ,tommyh-deep-green :weight bold))))
  `(font-lock-builtin-face ((t (:foreground ,tommyh-burnt :weight bold))))
  `(font-lock-constant-face ((t (:inherit font-lock-keyword-face))))
  `(font-lock-doc-face ((t (:inherit font-lock-type-face))))
  `(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :slant italic))))
  ;; basic stuff
  `(fringe ((t (:background ,tommyh-pastel-blue3 :foreground "white"))))
  `(hl-line ((t (:background ,tommyh-pastel-blue))))
  `(linum ((t (:background ,tommyh-pastel-blue3 :foreground "white"))))
  `(region ((t (:background ,tommyh-pastel-blue2))))
  `(mode-line ((t (:background ,tommyh-dark-blue :foreground "white" :box (:line-width -1 :style released-button) :weight bold))))
  `(mode-line-inactive ((t (:inherit mode-line :background "white" :foreground ,tommyh-dark-blue :box (:line-width -1 :color ,tommyh-gunmetal) :weight light))))
  ;; python
  `(py-number-face ((t (:foreground ,tommyh-seafoam :weight extra-bold))))
  `(py-variable-name-face ((t (:inherit default))))
  ))

;;;###autoload
(when load-file-name
    (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'tommyh)

;;; tommyh-theme.el ends here
