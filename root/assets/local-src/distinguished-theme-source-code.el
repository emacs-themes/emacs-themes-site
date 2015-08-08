;;; distinguished-theme.el --- A dark and elegant theme for emacs.

;; Copyright © 2014 Kim Silkebækken

;; Author: Kim Silkebækken &lt;kim.silkebaekken@gmail.com&gt;
;; URL: https://github.com/Lokaltog/distinguished-theme
;; Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; A modified port of the distinguished theme for vim.

;;; Code:
(deftheme distinguished "Distinguished color theme")

(let ((dst-fg "#ffffff")
      (dst-bg "#000000")
      (dst-bg+0 "#151311")
      (dst-bg+1 "#252321")
      (dst-bg+2 "#474544")
      (dst-gray "#969385")
      (dst-gray+1 "#b4b1a2")
      (dst-gray+2 "#d0cbc0")
      (dst-steel "#8a9496")
      (dst-steel+1 "#acb0b3")
      (dst-steel+2 "#c0c7ca")
      (dst-blue "#67809c")
      (dst-blue+1 "#b2c3cc")
      (dst-blue+2 "#d9e2ff")
      (dst-green-2 "#646d14")
      (dst-green-1 "#869038")
      (dst-green "#a4ac64")
      (dst-green+1 "#ccc768")
      (dst-red-3 "#3f1c0f")
      (dst-red-2 "#7c2a09")
      (dst-red-1 "#a7502d")
      (dst-red "#d47c59")
      (dst-red+1 "#edb08f")
      (dst-red+2 "#edbca2")
      (dst-yellow "#d7af5f")
      (dst-yellow+1 "#ffd75f")
      (dst-yellow+2 "#f9ee98")
      (dst-intense-red "#ff2a00")
      )
  (custom-theme-set-faces
   'distinguished
   `(default ((t (:foreground ,dst-fg :background ,dst-bg))))
   `(cursor ((t (:foreground ,dst-bg :background ,dst-fg))))
   `(hl-line ((t (:background ,dst-bg+0))))
   `(minibuffer-prompt ((t (:foreground ,dst-green :weight bold))))
   `(region ((t (:background ,dst-bg+2))))
   `(fringe ((t (:foreground ,dst-gray+2 :background ,dst-bg+1))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,dst-yellow+1 :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,dst-gray))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,dst-bg+2))))
   `(font-lock-doc-face ((t (:foreground ,dst-gray))))
   `(font-lock-constant-face ((t (:foreground ,dst-yellow+1 :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,dst-red :weight bold :underline ,dst-red-3))))
   `(font-lock-keyword-face ((t (:foreground ,dst-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,dst-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,dst-steel+1 :weight bold :slant italic))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,dst-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,dst-red :weight bold))))
   `(font-lock-string-face ((t (:foreground ,dst-green))))
   `(font-lock-type-face ((t (:foreground ,dst-green+1 :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,dst-blue+1 :weight normal :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,dst-intense-red :weight bold))))

   ;; basic whitespace-mode (tabs/newlines)
   `(whitespace-tab ((t (:foreground ,dst-bg+2 :background nil :weight normal))))
   `(whitespace-newline ((t (:foreground ,dst-red-3 :background nil :weight normal))))

   ;; show parens
   `(show-paren-mismatch ((t (:foreground ,dst-fg :background ,dst-red-1 :weight bold))))
   `(show-paren-match ((t (:foreground ,dst-fg :background ,dst-green-2 :weight bold))))

   ;; search highlight
   `(isearch ((t (:foreground ,dst-fg :background ,dst-green-2 :weight bold :slant normal))))
   `(isearch-fail ((t (:foreground ,dst-fg :background ,dst-red-1 :weight bold :slant normal))))
   `(lazy-highlight ((t (:foreground ,dst-bg :background ,dst-yellow+1 :weight bold :slant normal))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dst-yellow+2))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dst-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dst-red+1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dst-blue+1))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dst-yellow+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dst-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dst-red+1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dst-blue+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dst-yellow+2))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,dst-green-1 :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,dst-red-1 :weight bold))))
   `(git-gutter:modified ((t (:foreground ,dst-blue :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,dst-fg :weight bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,dst-green-1  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,dst-red-1 :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,dst-blue :weight bold))))

   ;; custom stuff from Lokaltog/emacsfiles
   `(font-lock-format-specifier-face ((t (:foreground ,dst-yellow :weight bold))))
   `(font-lock-number-face ((t (:foreground ,dst-red :weight bold))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'distinguished)
;;; distinguished-theme.el ends here
