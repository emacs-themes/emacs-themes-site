;;; Amelie-theme.el --- A dark place with bright sights.

;; Copyright (C) 2012  Ranmocy Sheng

;; Author: Ranmocy Sheng &lt;ranmocy@gmail.com&gt;
;; Keywords: faces
;; URL: https://github.com/ranmocy/amelie-theme
;; Version: 1.1

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

;;; Commentary:
;;
;; Inspired by slim-lang.org. Reference to zenburn-theme.
;; Drop it into themes directory defined by `custom-theme-set-faces' and enjoy it.

;;; Requirements:
;;
;; Emacs 24.

;;; Code:

(deftheme Amelie
  "Amelie brighten your eyes. Created 2012-06-16.")

(let ((amelie-fg "#ddd")
      (amelie-bg "#2b2b2b")
      (amelie-fg-hi "white")
      (amelie-bg-hi "#545454")
      (amelie-grey "#ccc")
      (amelie-grey-1 "#bbb")
      (amelie-grey-2 "#888")
      (amelie-grey-3 "#444")
      (amelie-red "#EA3E33")
      (amelie-orange+1 "#EF907E")
      (amelie-orange "#E75544")
      (amelie-orange-1 "#AC4123")
      (amelie-yellow+1 "#FE8")
      (amelie-yellow "#FB0")
      (amelie-yellow-1 "#B90")
      (amelie-green+1 "#0FB")
      (amelie-green "#50d42b")
      (amelie-green-1 "#54ab54")
      (amelie-cyan "#00eaff")
      (amelie-blue "#01aafe")
      (amelie-magenta "#99aFfF"))
  (custom-theme-set-faces
   'Amelie
   `(default ((t (:background ,amelie-bg :foreground ,amelie-fg))))

   ;; Editor
   `(cursor ((t (:background ,amelie-fg :foreground ,amelie-bg))))
   `(highlight ((t (:background ,amelie-bg-hi))))
   `(region ((t (:background ,amelie-bg-hi :foreground ,amelie-fg-hi))))
   `(header-line ((t (:foreground ,amelie-yellow
                                  :box (:line-width -1 :style released-button)))))
   ;; UI
   `(menu ((t (:foreground ,amelie-fg :background ,amelie-bg))))
   `(mode-line ((t (:foreground ,amelie-green-1
                                :background ,amelie-bg
                                :box (:line-width -1 :style pressed-button)))))
   `(mode-line-inactive ((t (:foreground ,amelie-grey-2
                                         :background ,amelie-bg
                                         :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((t (:foreground ,amelie-magenta :weight bold))))
   `(minibuffer-prompt ((t (:foreground ,amelie-yellow))))

   ;; font lock
   `(font-lock-comment-face ((t (:foreground ,amelie-grey-2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,amelie-grey-3))))
   `(font-lock-constant-face ((t (:foreground ,amelie-cyan))))
   `(font-lock-builtin-face ((t (:foreground ,amelie-cyan))))
   `(font-lock-function-name-face ((t (:foreground ,amelie-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,amelie-orange))))
   `(font-lock-keyword-face ((t (:foreground ,amelie-yellow))))
   `(font-lock-string-face ((t (:foreground ,amelie-yellow+1))))
   `(font-lock-doc-string-face ((t (:foreground ,amelie-fg-hi))))
   `(font-lock-type-face ((t (:foreground ,amelie-green+1))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'Amelie)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; Amelie-theme.el ends here
