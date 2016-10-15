;;; punpun-common.el --- A bleak theme.

;; Copyright (C) 2016 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/punpun-theme

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A bleak theme.  Uses shades of gray and the occasional splash of
;; color.  Designed for 256-color terminals.  Comes in light and dark!

;;; Code:

(defvar punpun-colors
  '((base0  ("#eeeeee" "#080808") ("color-255" "color-232"))
    (base1  ("#d0d0d0" "#1c1c1c") ("color-252" "color-234"))
    (base2  ("#b2b2b2" "#3a3a3a") ("color-249" "color-237"))
    (base3  ("#949494" "#585858") ("color-246" "color-240"))
    (base4  ("#767676" "#767676") ("color-243" "color-243"))
    (base5  ("#585858" "#949494") ("color-240" "color-246"))
    (base6  ("#3a3a3a" "#b2b2b2") ("color-237" "color-249"))
    (base7  ("#1c1c1c" "#d0d0d0") ("color-234" "color-252"))

    (yellow ("#ffaf00" "#ffd700") ("color-214" "color-220"))
    (orange ("#d75f00" "#ff5f00") ("color-166" "color-202"))
    (red    ("#d70000" "#ff0000") ("color-160" "color-196"))
    (green  ("#00af00" "#00d700") ("color-34"  "color-40" ))
    (blue   ("#5f00ff" "#005fff") ("color-57"  "color-27" ))
    (cyan   ("#0087ff" "#00d7ff") ("color-33"  "color-45" ))
    (pink   ("#ff005f" "#ff0087") ("color-197" "color-198"))
    (violet ("#8700d7" "#af00d7") ("color-92"  "color-128"))))

(defvar punpun-faces
 '(;; compile.el
   (compilation-mode-line-fail :inherit error :weight bold)
   (compilation-mode-line-exit :inherit success :weight bold)

   ;; cua-base.el
   (cua-rectangle :inherit region)

   ;; dired.el
   (dired-symlink :foreground blue)

   ;; em-ls.el
   (eshell-ls-directory :inherit dired-directory)
   (eshell-ls-symlink :inherit dired-symlink)
   (eshell-ls-executable :foreground green)
   (eshell-ls-readonly :foreground pink)
   (eshell-ls-unreadable :foreground cyan)
   (eshell-ls-special :foreground orange)
   (eshell-ls-missing :foreground red)
   (eshell-ls-archive :inherit dired-ignored)
   (eshell-ls-backup :inherit dired-ignored)
   (eshell-ls-product :inherit dired-ignored)
   (eshell-ls-clutter :inherit dired-ignored)

   ;; em-prompt.el
   (eshell-prompt :weight bold)

   ;; eww.el
   (eww-form-submit :foreground base6 :background base1 :box (:line-width -1))
   (eww-form-checkbox :inherit eww-form-submit)
   (eww-form-select :inherit eww-form-submit)
   (eww-form-text :background base1)
   (eww-form-textarea :inherit eww-form-text)

   ;; faces.el
   (default :foreground base5 :background base0)
   (cursor :background base4)
   (region :foreground unspecified :background base2)
   (fringe :foreground base7 :background base1)
   (vertical-border :foreground base6)
   (shadow :inherit font-lock-comment-face)
   (link :slant italic :underline t)
   (link-visited :inherit link :foreground base4)
   (highlight :background base1)
   (secondary-selection :background base1)
   (trailing-whitespace :background red)
   (escape-glyph :foreground green :weight bold)

   (mode-line :foreground base5 :background base1)
   (mode-line-inactive :foreground base3 :background base1)
   (mode-line-highlight :slant italic)
   (mode-line-highlight :box (:line-width -1))
   (header-line :inherit mode-line)
   (vertical-border :foreground base4)
   (window-divider :inherit vertical-border)
   (minibuffer-prompt :inherit font-lock-comment-face)

   (error :foreground red)
   (success :foreground green)
   (warning :foreground orange :weight bold)

   (show-paren-match :weight bold)
   (show-paren-mismatch :foreground red :weight bold)

   ;; flymake
   (flymake-errline :underline (:style wave :color red))
   (flymake-warnline :underline (:style wave :color orange))

   ;; flyspell
   (flyspell-duplicate :underline (:style wave :color orange))
   (flyspell-incorrect :underline (:style wave :color red))

   ;; font-lock.el
   (font-lock-builtin-face :foreground base4 :weight bold)
   (font-lock-comment-face :foreground base4)
   (font-lock-constant-face :slant italic)
   (font-lock-function-name-face :slant italic)
   (font-lock-keyword-face :foreground base4 :weight bold)
   (font-lock-negation-char-face :weight bold)
   (font-lock-regexp-grouping-construct :weight bold)
   (font-lock-regexp-grouping-backslash :weight bold)
   (font-lock-string-face :foreground base4)
   (font-lock-type-face :slant italic)
   (font-lock-variable-name-face :slant italic)
   (font-lock-warning-face :foreground orange :weight bold)

   ;; gdb-mi.el
   (breakpoint-enabled :foreground red)
   (breakpoint-enabled :inherit font-lock-comment-face)

   ;; ido.el
   (ido-only-match :weight bold)
   (ido-subdir :inherit font-lock-string-face)

   ;; isearch.el
   (isearch :background base2)
   (isearch-fail :inherit error)
   (lazy-highlight :background base1)

   ;; info.el
   (Info-quoted :inherit font-lock-constant-face)
   (info-node :weight bold)
   (info-title-1 :inherit info-node)
   (info-title-2 :inherit info-node)
   (info-title-3 :inherit info-node)
   (info-title-4 :inherit info-node)
   (info-menu-header :inherit default)
   (info-menu-star :foreground red)

   ;; ivy.el
   (ivy-confirm-face :foreground green)
   (ivy-current-match :weight bold :background base1)
   (ivy-match-required-face :foreground red)
   (ivy-minibuffer-match-face-1 :foreground orange)
   (ivy-minibuffer-match-face-2 :foreground orange)
   (ivy-minibuffer-match-face-3 :foreground orange)
   (ivy-minibuffer-match-face-4 :foreground orange)
   (ivy-modified-buffer :inherit default)
   (ivy-remote :foreground blue)
   (ivy-subdir :inherit dired-directory)
   (ivy-virtual :inherit font-lock-builtin-face)

   ;; linum.el
   (linum :inherit shadow :background base1)

   ;; make-mode.el
   (makefile-space :background red)

   ;; outline.el
   (outline-1 :weight bold)
   (outline-2 :inherit outline-1)
   (outline-3 :inherit outline-1)
   (outline-4 :inherit outline-1)
   (outline-5 :inherit outline-1)
   (outline-6 :inherit outline-1)
   (outline-7 :inherit outline-1)
   (outline-8 :inherit outline-1)

   ;; pulse.el
   (pulse-highlight-start-face :background yellow)

   ;; re-builder.el
   (reb-match-0 :foreground base7 :background base1)
   (reb-match-1 :foreground base7 :background base2)
   (reb-match-2 :foreground base7 :background yellow)
   (reb-match-3 :foreground base7 :background orange)

   ;; replace.el
   (match :background yellow)

   ;; rst.el
   (rst-level-1 :inherit outline-1)
   (rst-level-2 :inherit outline-2)
   (rst-level-3 :inherit outline-3)
   (rst-level-4 :inherit outline-4)
   (rst-level-5 :inherit outline-5)
   (rst-level-6 :inherit outline-6)

   ;; sh-script.el
   (sh-heredoc :inherit font-lock-string-face)
   (sh-quoted-exec :slant italic)

   ;; tooltip.el
   (tooltip :inherit default :background base0)

   ;; which-func.el
   (which-func :slant italic)

   ;; whitespace.el
   (whitespace-space :foreground unspecified :background base1)
   (whitespace-hspace :foreground unspecified :background base2)
   (whitespace-tab :foreground unspecified :background red)
   (whitespace-newline :inherit font-lock-comment-face)
   (whitespace-line :inherit warning :underline t)
   (whitespace-space-before-tab :foreground unspecified :background orange)
   (whitespace-space-after-tab :foreground unspecified :background orange)
   (whitespace-indentation :foreground unspecified :background orange)
   (whitespace-empty :foreground unspecified :background yellow)
   (whitespace-trailing :foreground unspecified :background red)

   ))

(defun punpun-set-faces (theme darkp)
  (apply #'custom-theme-set-faces theme
         (mapcar (lambda (face) (punpun-transform-face face darkp))
                 punpun-faces)))

(defun punpun-transform-face (face darkp)
  (let* ((name (car face))
         (spec (cdr face))
         (graphic-spec (punpun-transform-spec spec t darkp))
         (tty-spec (punpun-transform-spec spec nil darkp)))
    `(,name ((((type graphic)) ,@graphic-spec)
             (((type tty)) ,@tty-spec)))))

(defun punpun-transform-spec (spec display darkp)
  (let (output)
    (while spec
      (let* ((key (car spec))
             (value (cadr spec))
             (color (punpun-color value display darkp)))
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (setq output (append output
                               (list key (punpun-transform-spec value display darkp)))))
         ((and (not (memq value '(t unspecified)))
               (memq key '(:foreground :background :underline :overline :strike-through :color))
               color)
          (setq output (append output (list key color))))
         (t (setq output (append output (list key value))))))
      (setq spec (cddr spec)))
    output))

(defun punpun-color (shade display darkp)
  (nth (if darkp 1 0)
       (nth (if display 1 2)
            (assoc shade punpun-colors))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'punpun-common)

;;; punpun-common.el ends here


(require 'punpun-common)

(deftheme punpun-dark "A bleak theme (dark version)")

(punpun-set-faces 'punpun-dark t)

(provide-theme 'punpun-dark)





(require 'punpun-common)

(deftheme punpun-light "A bleak theme (light version)")

(punpun-set-faces 'punpun-light nil)

(provide-theme 'punpun-light)
