;; -*- eval: (rainbow-mode); -*-
;;; Deviant-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010, 2011, 2012 Darksair.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

;;; Code:

(defconst dv-macp nil "Are we running in Mac OS?")

(defun color-sRGB (color-tuple)
  (car color-tuple))

(defun color-genRGB (color-tuple)
  (car (cdr color-tuple)))

(defun dv-color (color-tuple)
  (if dv-macp
      (color-genRGB color-tuple)
    (color-sRGB color-tuple)))

(deftheme Deviant
  "Inspired by the color scheme of deviantart.com (v2)")

;; Colors, in (sRGB GenericRGB)
(defvar dv-default-bg '("#2e3735" "#232a28"))
(defvar dv-default-fg '("#b6beb4" "#a8b1a4"))
(defvar dv-cursor-bg '("#c73a7c" "#b0256c"))
(defvar dv-cursor-fg dv-default-bg)
(defvar dv-region-bg '("#afc81c" "#a0c116"))
(defvar dv-region-fg dv-default-bg)
(defvar dv-modeline-bg '("#3e4745" "#303735"))
(defvar dv-modeline-fg dv-default-fg)
(defvar dv-modeline-inact-bg dv-default-bg)
(defvar dv-modeline-inact-fg '("#666e64" "#545b50"))
(defvar dv-fringe-bg dv-modeline-bg)
(defvar dv-mb-prompt-fg dv-region-bg)
(defvar dv-builtin-fg '("#d5b613" "#c9ab00"))
(defvar dv-comment-fg '("#706a53" "#5c583f"))
(defvar dv-constant-fg '("#bebeb4" "#b1b1a4"))
(defvar dv-function-fg dv-region-bg)
(defvar dv-keyword-fg '("#57a6ff" "#4d8fff"))
(defvar dv-string-fg '("#e3795c" "#d36545"))
(defvar dv-type-fg '("#68b91a" "#63af00"))
(defvar dv-var-fg dv-default-fg)
(defvar dv-warn-fg '("#c73a7c" "#b0256c"))
(defvar dv-link-fg '("#599bb0" "#4f89a3"))
(defvar dv-search-bg dv-link-fg)
(defvar dv-search-fg dv-default-bg)
(defvar dv-lazy-hl-bg '("#465451" "#37433f"))
(defvar dv-link-old-fg '("#818780" "#6f746c"))
(defvar dv-button-bg '("#242b2a" "#1c201f"))
(defvar dv-header-bg '("#3e4745" "#303735"))
(defvar dv-header-fg '("#868e84" "#747c70"))

(custom-theme-set-faces
 'Deviant
 `(default ((t (:background ,(dv-color dv-default-bg)
                :foreground ,(dv-color dv-default-fg)))))
 `(cursor ((t (:background ,(dv-color dv-cursor-bg)
               :foreground ,(dv-color dv-cursor-fg)))))
 `(region ((t (:background ,(dv-color dv-region-bg)
               :foreground ,(dv-color dv-region-fg)))))
 `(mode-line ((t (:background ,(dv-color dv-modeline-bg)
                  :foreground ,(dv-color dv-modeline-fg)))))
 `(mode-line-inactive ((t (:background ,(dv-color dv-modeline-inact-bg)
                           :foreground ,(dv-color dv-modeline-inact-fg)))))
 `(fringe ((t (:background ,(dv-color dv-fringe-bg)))))
 `(minibuffer-prompt ((t (:slant italic :foreground ,(dv-color dv-mb-prompt-fg)))))
 `(font-lock-builtin-face ((t (:foreground ,(dv-color dv-builtin-fg)))))
 `(font-lock-comment-face ((t (:slant italic :foreground ,(dv-color dv-comment-fg)))))
 `(font-lock-constant-face ((t (:slant italic :foreground ,(dv-color dv-constant-fg)))))
 `(font-lock-function-name-face ((t (:foreground ,(dv-color dv-function-fg)))))
 `(font-lock-keyword-face ((t (:foreground ,(dv-color dv-keyword-fg)))))
 `(font-lock-string-face ((t (:foreground ,(dv-color dv-string-fg)))))
 `(font-lock-type-face ((t (:foreground ,(dv-color dv-type-fg)))))
 `(font-lock-variable-name-face ((t (:foreground ,(dv-color dv-var-fg)))))
 `(font-lock-warning-face ((t (:foreground ,(dv-color dv-warn-fg)))))
 `(isearch ((t (:background ,(dv-color dv-search-bg)
                :foreground ,(dv-color dv-search-fg)))))
 `(lazy-highlight ((t (:background ,(dv-color dv-lazy-hl-bg)))))
 `(link ((t (:foreground ,(dv-color dv-link-fg) :underline t))))
 `(link-visited ((t (:foreground ,(dv-color dv-link-old-fg) :underline t))))
 `(button ((t (:background ,(dv-color dv-button-bg) :underline t))))
 `(header-line ((t (:background ,(dv-color dv-header-bg)
                    :foreground ,(dv-color dv-header-fg))))))

;; Rainbow delimiters
(defun dv-rainbow-delim-set-face ()
  (set-face-attribute
   'rainbow-delimiters-depth-1-face nil
   :foreground (dv-color dv-default-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-2-face nil
   :foreground (dv-color dv-builtin-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-3-face nil
   :foreground (dv-color dv-keyword-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-4-face nil
   :foreground (dv-color dv-string-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-5-face nil
   :foreground (dv-color dv-region-bg))
  (set-face-attribute
   'rainbow-delimiters-depth-6-face nil
   :foreground (dv-color dv-link-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-7-face nil
   :foreground (dv-color dv-comment-fg))
  (set-face-attribute
   'rainbow-delimiters-unmatched-face nil
   :foreground (dv-color dv-warn-fg)))

;; Moinmoin mode
(defun dv-moinmoin-set-face ()
  (set-face-attribute
   'moinmoin-wiki-link nil
   :foreground (dv-color dv-link-fg)
   :weight 'normal)
  (set-face-attribute
   'moinmoin-url nil
   :foreground (dv-color dv-link-fg)
   :height 1)
  (set-face-attribute
   'moinmoin-url-title nil
   :foreground (dv-color dv-link-fg))
  (set-face-attribute
   'moinmoin-h4 nil
   :foreground (dv-color dv-region-bg))
  (set-face-attribute
   'moinmoin-item nil
   :foreground (dv-color dv-builtin-fg)
   :weight 'normal)
  (set-face-attribute
   'moinmoin-tt nil
   :foreground (dv-color dv-string-fg))
  (set-face-attribute
   'moinmoin-code-braces nil
   :foreground (dv-color dv-default-fg))
  (set-face-attribute
   'moinmoin-code nil
   :foreground (dv-color dv-string-fg))
  (set-face-attribute
   'moinmoin-rule nil
   :foreground (dv-color dv-builtin-fg)
   :weight 'normal)
  (set-face-attribute
   'moinmoin-blockquote-text nil
   :foreground (dv-color dv-default-fg))
  (set-face-attribute
   'moinmoin-blockquote-indent nil
   :background (dv-color dv-default-bg)))

;; CUA
(defun dv-cua-set-face ()
  (set-face-attribute
   'cua-rectangle nil
   :foreground (dv-color dv-region-fg)
   :background (dv-color dv-region-bg)))
(if cua-mode
    (dv-cua-set-face)
  (add-hook 'cua-mode-hook 'dv-cua-set-face))

;; Powerline
(defun dv-powerline-set-face ()
  (set-face-attribute
   'mode-line nil
   :foreground (dv-color dv-default-bg)
   :background (dv-color dv-region-bg)
   :box nil))
  ;; (setq powerline-color2 (dv-color dv-default-bg)))

(eval-after-load "rainbow-delimiters" '(dv-rainbow-delim-set-face))
(eval-after-load "moinmoin-mode" '(dv-moinmoin-set-face))
(eval-after-load "powerline" '(dv-powerline-set-face))

(provide-theme 'Deviant)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; Deviant-theme.el  ends here
