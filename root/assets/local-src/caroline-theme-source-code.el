;;; caroline-theme.el --- A trip down to New Orleans...

;; Author: Jack Killilea <jaaacckz1@gmail.com>
;; URL: https://github.com/xjackk/carolines-theme
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

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

;;; Credits:

;; Thanks to Paulik Christoph's material-theme for the boilerplate.

;; To get to usin', chuck this into your .emacs or whatever load file you kids are using these days...
;;
;;   (load-theme 'caroline t)
;;
;; Requirements: Emacs 24.
;; Written for Emacs 24.

;;; Code:

(deftheme caroline
  "Just a trip to New Orleans...")

(let ((caroline-fg "#FDF5E6")
      (caroline-bg "#2b2b2b")
      (caroline-fg-hi "white")
      (caroline-bg-hi "#FFB6C1")
      (caroline-grey "#ccc")
      (caroline-grey-1 "#bbb")
      (caroline-grey-2 "#888")
      (caroline-grey-3 "#444")
      (caroline-red "#EA3E33")
      (caroline-orange+1 "#EF907E")
      (caroline-orange "#E75544")
      (caroline-orange-1 "#AC4123")
      (caroline-yellow+1 "#FE8")
      (caroline-yellow "#FB0")
      (caroline-yellow-1 "#B90")
      (caroline-green+1 "#0FB")
      (caroline-green "#50d42b")
      (caroline-green-1 "#54ab54")
			(caroline-brightgrey-1 "#C5C1AA")
      (caroline-cyan "#00eaff")
      (caroline-lightblue-1 "#7D9EC0")
			(caroline-skyblue "#A4D3EE")
      (caroline-blue "#01aafe")
			(caroline-turkblue "#00C78C")
			(caroline-lightpink "#FFB6C1")
			(caroline-darkpink "#DB7093")
			(caroline-tealish "#00EEEE")
      (caroline-magenta "#99aFfF")

			(header-color (if window-system "#455A64" "#5f5f5f"))
			(current-line (if window-system  "#37474f" "#3a3a3a"))
)

;; held my hand all through Burbon Street
  (custom-theme-set-faces

   'caroline ;; Some of the bluest eyes you have ever seen

   `(default ((t (:background ,caroline-bg :foreground ,caroline-fg))))

   ;; The Editor itself..
   `(cursor ((t (:background ,caroline-fg :foreground ,caroline-bg))))
   `(highlight ((t (:background ,caroline-bg-hi))))
   `(region ((t (:background ,caroline-bg-hi :foreground ,caroline-fg-hi))))
   `(header-line ((t (:foreground ,caroline-yellow
                                  :box (:line-width -1 :style released-button)))))
 ;; Flycheck
 ;; Baby, I love you - The Ramones
   `(flycheck-error ((t (:underline (:style wave :color ,caroline-lightpink)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,caroline-lightpink)))))

 ;; Flymake
   `(flymake-warnline ((t (:underline (:style wave :color ,caroline-tealish) :background ,caroline-bg))))
   `(flymake-errline ((t (:underline (:style wave :color ,caroline-red) :background ,caroline-bg))))

	 ;; Markdown
;;	 (markdown-url-face ((:inherit caroline-blue))))
;;	 (markdown-link-face ((t (:foreground ,caroline-blue :underline t))))

   ;; User Interface stuff
   `(menu ((t (:foreground ,caroline-fg :background ,caroline-bg))))
   `(mode-line ((t (:foreground ,caroline-bg
                                :background ,caroline-lightpink
                                :box (:line-width -1 :style pressed-button)))))
   `(mode-line-inactive ((t (:foreground ,caroline-bg
                                         :background ,caroline-grey-2
                                         :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((t (:foreground ,caroline-bg :weight bold))))
   `(minibuffer-prompt ((t (:foreground ,caroline-tealish))))


	 ;; Org Mode
	 `(org-agenda-structure ((t (:foreground ,caroline-cyan :bold t))))
   `(org-agenda-date ((t (:foreground ,caroline-blue :underline nil))))
   `(org-agenda-done ((t (:foreground ,caroline-lightpink))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,caroline-lightpink))))
   `(org-block ((t (:foreground ,caroline-blue :background ,caroline-bg))))
   `(org-document-info-keyword ((t (:foreground ,caroline-turkblue :height 1.35))))
   `(org-document-title ((t (:weight bold :foreground ,caroline-fg :height 1.35))))
   `(org-done ((t (:foreground ,caroline-turkblue :bold t :background,"#1b5e20"))))
   `(org-ellipsis ((t (:foreground ,caroline-skyblue))))
   `(org-footnote ((t (:foreground ,caroline-turkblue))))
   `(org-formula ((t (:foreground ,caroline-red))))
   `(org-hide ((t (:foreground ,caroline-bg-hi :background ,caroline-bg-hi))))
   `(org-link ((t (:foreground ,caroline-lightblue-1 :underline t))))
   `(org-scheduled ((t (:foreground ,caroline-green))))
   `(org-scheduled-previously ((t (:foreground ,caroline-lightpink))))
   `(org-scheduled-today ((t (:foreground ,caroline-lightpink))))
   `(org-block-background ((t (:background ,caroline-bg))))
   `(org-code ((t (:foreground ,caroline-lightpink :background ,caroline-bg))))
   `(org-column ((t (:background ,current-line))))
   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
   `(org-date ((t (:foreground ,"#80cbc4" :underline t))))
   `(org-document-info ((t (:foreground ,caroline-turkblue :height 1.35))))
   `(org-special-keyword ((t (:foreground ,caroline-bg-hi))))
   `(org-table ((t (:foreground ,"#e3f2fd" :background ,caroline-bg))))
   `(org-todo ((t (:foreground ,"#ffab91" :bold t :background ,"#dd2c00"))))
   `(org-upcoming-deadline ((t (:foreground ,caroline-red))))
   `(org-warning ((t (:weight bold :foreground ,caroline-red))))
   `(org-block-begin-line ((t (:foreground ,"#b3e5fc" :background "#1e2930" :underline ,"#e1f5fe"))))
   `(org-block-end-line ((t (:foreground ,"#b3e5fc" :background "#1e2930" :overline ,"#e1f5fe"))))
   `(org-kbd ((t (:background ,caroline-grey :foreground ,caroline-fg
                                   :box (:line-width 1 :color nil :style pressed-button)))))

   `(org-level-1 ((t (:inherit outline-1
                         :weight bold
												 :background ,header-color
                         :height 1.3))))
   `(org-level-2 ((t (:inherit outline-2 :height 1.2 :background ,header-color ))))
   `(org-level-3 ((t (:inherit outline-3 :height 1.1 :foreground, caroline-lightpink :background ,header-color))))
   `(org-level-4 ((t (:inherit outline-4 :height 1.0 :foreground, caroline-skyblue :weight bold))))
   `(org-level-5 ((t (:inherit outline-5 ))))
   `(org-level-6 ((t (:inherit outline-6 ))))
   `(org-level-7 ((t (:inherit outline-7 ))))
   `(org-level-8 ((t (:inherit outline-8 ))))
   `(org-level-9 ((t (:inherit outline-9 ))))


	 ;; Web Mode (HTML)
	 ;; Tell me that you feel, tell me that you feel the same
	 '(web-mode-html-tag-face         ((t (:foreground "#FFB6C1" ))))
;;	 '(web-mode-html-tag-bracket-face ((t (:foreground "#FFB6C1" :weight bold))))
;;	 '(web-mode-html-attr-name-face   ((t (:foreground "#FFB6C1" ))))

	 ;; CPerl Mode
	 '(cperl-array-face ((t (:foreground "#EA3E33" :background nil ))))
	 '(cperl-hash-face ((t (:foreground "#EA3E33" :background nil ))))

   ;; font lock
   `(font-lock-comment-face ((t (:foreground ,caroline-grey-2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,caroline-grey-2))))
   `(font-lock-constant-face ((t (:foreground ,caroline-cyan))))
   `(font-lock-builtin-face ((t (:foreground ,caroline-cyan))))
   `(font-lock-function-name-face ((t (:foreground ,caroline-red :background, nil))))
   `(font-lock-variable-name-face ((t (:foreground ,caroline-lightpink :background, nil))))
   `(font-lock-keyword-face ((t (:foreground ,caroline-turkblue))))
   `(font-lock-string-face ((t (:foreground ,caroline-tealish :background, nil))))
   `(font-lock-doc-string-face ((t (:foreground ,caroline-fg-hi))))
   `(font-lock-type-face ((t (:foreground ,caroline-lightpink))))
   ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'caroline)
;;; caroline-theme.el ends here
