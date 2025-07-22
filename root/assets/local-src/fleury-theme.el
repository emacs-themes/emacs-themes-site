;;; fleury-theme.el --- The fleury color theme

;; Copyright (C) 2025 Shams Parvez Arka
;; See end of file for extended copyright information

;; Author    : Shams Parvez Arka <parvez6826@gmail.com>
;; URL       : https://github.com/ShamsParvezArka/fleury-theme.el
;; Version   : 0.2
;; Commentary: "Coming up with an original idea in 21st century
;;             is tough, even my dreams aren't original anymore!"


(deftheme fleury "The fleury color theme")

;; Color palette
(let ((rich-black         "#020202")
      (light-bronze       "#b99468")
      (lime-green         "#003a3a")
      (charcoal-gray      "#212121")
      (charcoal-gray-lite "#1e1e1e")
      (gunmetal-blue      "#303040")
      (dark-slate         "#222425")
      (amber-gold         "#fcaa05")
      (medium-gray        "#404040")
      (jet-black          "#121212")
      (dim-gray           "#666666")
      (goldenrod          "#f0c674")
      (bright-orange      "#ffaa00")
      (dusty-rose         "#dc7575")
      (sunflower-yellow   "#edb211")
      (burnt-orange       "#de451f")
      (sky-blue           "#2895c7")
      (sky-blue-lite      "#2f2f38")
      (bright-red         "#ff0000")
      (fresh-green        "#2ab34f")
      (lime-green         "#002928")
      (vivid-vermilion    "#f0500c")
      (golden-yellow      "#f0bb0c")
      (pure-black         "#000000")
      (aqua-ice           "#8ffff2")
      (dusty-sage         "#9ba290")
      (coffee-brown       "#63523d")
      )

  (custom-theme-set-faces
   'fleury

   ;; UI Elements
   `(default ((t (:background ,rich-black :foreground ,light-bronze))))
   `(cursor ((t (:background ,fresh-green))))
   `(region ((t (:background ,lime-green))))
   `(highlight ((t (:background ,charcoal-gray-lite))))
   `(fringe ((t (:background ,dark-slate))))
   `(vertical-border ((t (:foreground ,dark-slate))))
   `(minibuffer-prompt ((t (:foreground ,amber-gold :weight bold))))

   ;; Line Numbers
   `(line-number ((t (:foreground ,medium-gray :background ,rich-black))))
   `(line-number-current-line ((t (:background ,charcoal-gray-lite :foreground ,light-bronze))))

   ;; Font Lock Faces
   `(font-lock-comment-face ((t (:foreground ,dim-gray))))
   `(font-lock-keyword-face ((t (:foreground ,goldenrod))))
   `(font-lock-string-face ((t (:foreground ,bright-orange))))
   `(font-lock-constant-face ((t (:foreground ,bright-orange))))
   `(font-lock-builtin-face ((t (:foreground ,dusty-rose))))
   `(font-lock-type-face ((t (:foreground ,sunflower-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,burnt-orange))))
   `(font-lock-variable-name-face ((t (:foreground ,sky-blue))))
   `(font-lock-preprocessor-face ((t (:foreground ,dusty-rose))))
   `(font-lock-warning-face ((t (:foreground ,bright-red :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fresh-green))))

   ;; Mode Line
   `(mode-line ((t (:background ,dark-slate :foreground ,light-bronze :box nil))))
   `(mode-line-inactive ((t (:background ,jet-black :foreground ,medium-gray :box nil))))

   ;; Search
   `(isearch ((t (:background ,vivid-vermilion :foreground ,pure-black))))
   `(lazy-highlight ((t (:background ,golden-yellow :foreground ,pure-black))))

   ;; Custom Elements
   `(show-paren-match ((t (:background ,sky-blue-lite))))
   `(show-paren-mismatch ((t (:background ,dusty-sage))))

   ;; Tooltip and Popup
   `(tooltip ((t (:background ,coffee-brown :foreground ,amber-gold))))

   ;; Compilation
   `(flycheck-error ((t (:underline (:color ,bright-red :style wave)))))

   
   `(compilation-info ((t ,(list :foreground fresh-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground coffee-brown
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,bright-red))))
   `(compilation-mode-line-fail ((t ,(list :foreground bright-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground fresh-green
                                           :weight 'bold
                                           :inherit 'unspecified))))
   ))

(global-hl-line-mode +1)
;; (setq-default cursor-type '(bar . 3))
(provide-theme 'fleury)


;; MIT License

;; Copyright (c) 2025 Shams Parvez Arka

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
	
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
