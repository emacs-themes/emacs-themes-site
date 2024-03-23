;;; masked-theme.el --- Masked color theme for GNU Emacs.

;; Copyright (C) 2024 M. Enes Kaya

;; Author: M. Enes Kaya
;; E-mail: enoks@tutanota.com
;; URL: https://github.com/enoks1/masked-theme
;; Version: 0.1

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; TODO: magit, isearch, icomplete

(deftheme masked ()
		  "Masked theme for GNU Emacs")

;; colors with `+c' are lighter; and with `-c' darker
(let ((masked-bg+3        "#41417b")
	  (masked-bg+2        "#2f2f4d")
	  (masked-bg+1        "#222238")
	  (masked-bg+01       "#0f0f10")
	  (masked-bg+00       "#121212")
	  (masked-bg          "#000010")
	  (masked-bg-alt      "#0e0e0e")

	  (masked-ws          "#121212")

	  (masked-red         "#a34443")
	  (masked-green       "#8ba446")
	  (masked-yellow      "#987d3e")
	  (masked-blue        "#496f94")
	  (masked-magenta     "#897399")
	  (masked-cyan        "#518a8a")

	  (masked-fg          "#bbbbbb")
	  (masked-fg-1        "#969696")
	  (masked-fg-2        "#696969")

	  (masked-blue-alt    "#004daa")
	  (masked-magenta-alt "#c617e6")

	  (masked-gold        "#ffd700")
	  (masked-black       "#000000")
	  (masked-white       "#ffffff")

	  ;; disable bold/italic change them to 'normal'
	  (bold               'bold)
	  (italic             'italic))

  (custom-theme-set-faces
   'masked

   ;; ansi-term / vterm
   `(term-color-black ((t (:foreground ,masked-black :background ,masked-black))))
   `(term-color-red ((t (:foreground ,masked-red :background ,masked-red))))
   `(term-color-green ((t (:foreground ,masked-green :background ,masked-green))))
   `(term-color-blue ((t (:foreground ,masked-blue :background ,masked-blue))))
   `(term-color-yellow ((t (:foreground ,masked-yellow :background ,masked-yellow))))
   `(term-color-magenta ((t (:foreground ,masked-magenta :background ,masked-magenta))))
   `(term-color-cyan ((t (:foreground ,masked-cyan :background ,masked-cyan))))
   `(term-color-white ((t (:foreground ,masked-fg :background ,masked-fg))))

   ;; compilation
   `(compilation-info ((t (:foreground ,masked-green))))
   `(compilation-warning ((t (:foreground ,masked-yellow))))
   `(compilation-error ((t (:foreground ,masked-red))))
   `(compilation-mode-line-fail ((t (:foreground ,masked-red :weight ,bold))))
   `(compilation-mode-line-exit ((t (:foreground ,masked-green :weight ,bold))))

   ;; dired
   `(dired-directory ((t (:foreground ,masked-blue :weight ,bold))))
   `(dired-ignored ((t (:foreground ,masked-cyan))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,masked-green))))
   `(font-lock-comment-face ((t (:foreground ,masked-blue :slant ,italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,masked-blue :slant ,italic))))
   `(font-lock-constant-face ((t (:foreground ,masked-cyan))))
   `(font-lock-doc-face ((t (:foreground ,masked-green))))
   `(font-lock-function-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-keyword-face ((t (:foreground ,masked-yellow :weight ,bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,masked-magenta))))
   `(font-lock-string-face ((t (:foreground ,masked-red))))
   `(font-lock-type-face ((t (:foreground ,masked-magenta))))
   `(font-lock-variable-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-warning-face ((t (:foreground ,masked-red))))
   `(font-lock-negation-char-face ((t (:foreground ,masked-red))))

   ;; general
   `(cursor ((t (:background ,masked-gold))))
   `(default ((t (:foreground ,masked-fg :background ,masked-bg))))
   `(fringe ((t (:foreground ,masked-fg :background ,masked-bg+00))))
   `(minibuffer-prompt ((t (:foreground ,masked-white))))
   `(region ((t (:foreground ,masked-white :background ,masked-blue-alt))))
   `(link ((t (:foreground ,masked-magenta-alt :underline t))))
   `(link-visited ((t (:foreground ,masked-magenta :underline t))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,masked-red))))

   ;; line-numbers
   `(line-number ((t (:inherit default :foreground ,masked-bg+1 :background ,masked-bg-alt))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,masked-bg+2 :background ,masked-bg-alt
											:weight ,bold))))

   ;; mode-line
   `(mode-line-active ((t (:foreground ,masked-black :background ,masked-yellow :slant ,italic))))
   `(mode-line-inactive ((t (:foreground ,masked-fg-1 :background ,masked-bg+01 :slant ,italic))))
   `(mode-line-buffer-id ((t (:slant ,italic))))

   ;; org
   `(org-date ((t (:foreground ,masked-blue :background ,masked-bg))))
   `(org-hide ((t (:foreground ,masked-fg-1 :background ,masked-bg))))
   `(org-todo ((t (:foreground ,masked-red :background ,masked-bg))))
   `(org-done ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-headline-done ((t (:inherit org-done))))
   `(org-level-1 ((t (:foreground ,masked-red :background ,masked-bg))))
   `(org-level-2 ((t (:foreground ,masked-magenta :background ,masked-bg))))
   `(org-level-3 ((t (:foreground ,masked-blue :background ,masked-bg))))
   `(org-level-4 ((t (:foreground ,masked-cyan :background ,masked-bg))))
   `(org-level-5 ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-level-6 ((t (:foreground ,masked-yellow :background ,masked-bg))))
   `(org-level-7 ((t (:foreground ,masked-bg+3 :background ,masked-bg))))

   ;; powerline
   `(powerline-active0 ((t (:foreground ,masked-white  :background ,masked-bg+1))))
   `(powerline-active1 ((t (:foreground ,masked-white :background ,masked-bg-alt))))
   `(powerline-active2 ((t (:foreground ,masked-white :background ,masked-bg-alt))))
   `(powerline-inactive0 ((t (:foreground ,masked-fg-1  :background ,masked-bg-alt))))
   `(powerline-inactive1 ((t (:foreground ,masked-fg-1 :background ,masked-bg-alt))))
   `(powerline-inactive2 ((t (:foreground ,masked-fg-1 :background ,masked-bg-alt))))

   ;; whitespace
   `(whitespace-space ((t (:foreground ,masked-ws :background ,masked-bg ))))
   `(whitespace-tab ((t (:foreground ,masked-ws :background ,masked-bg ))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
			   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'masked)

;;; masked-theme.el ends here.
