;; Local Variables:
;; no-bytpe-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:



;;; zonokai-theme.el ends here
;;; zonokai-theme.el --- blue based theme for emacs

;; Copyright (C) 2013-2014

;; Author: Alex Sanchez &lt;ar.sanchez@keythings.co&gt;
;; URL: http://github.com/ZehCnaS34/zonokai.git
;; Version: 20140523.2151
;; X-Original-Version: 20140310.1330
;; X-Original-Version: 0.2.0

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
;; my personal touch on the Emacs monokai theme, with more a blue
;; base to the theme, with green and different hues of blue and red
;; for accenting
;;
;;; Code:

(require 'dash)
(require 'color)

(unless (&gt;= 24 emacs-major-version)
  (error "The Zonokai theme requires Emacs 24 or later!"))

(defgroup zonokai nil
  "Zonokai theme options.
The theme will have to be reloded after changing options."
  :group 'faces
  :prefix "zk-")

(deftheme zonokai "The zonokai color theme")

(defun in-terminal-p ()
  "Return true if in a terminal."
  (not (display-graphic-p)))


(defun zonokai-color-name-to-rgb (color &amp;optional frame)
  (let ((valmax (float (car (color-values "#fff")))))
    (mapcar (lambda (x) (/ x valmax)) (color-values color frame))))


(defun create-zonokai-theme ()
  "Create the zonokai theme.
Takes an optional `FRAME' as reference."
  (let* ((class '((class color) (min-colors 89)))
         ;; BG color
         (zk-hl "#096BAA")
         (zk-fg "#eee")
         (zk-bg (color-darken-name "#032840" 5))
         (zk-hl-line (color-darken-name zk-bg 5))
         (zk-emph "#2F157F")
         (zk-comments "#00A1FF")
         (zk-cursor "#9FFF24")
         (zk-region (color-darken-name zk-bg 10))

         ;; Accented colors
         (brown      "#B26800")
         (brown+1    (color-lighten-name brown 10))
         (brown-1    (color-darken-name brown 10))
         (red        "#CC1514")
         (red+1      (color-lighten-name red 10))
         (red-1      (color-darken-name red 10))
         (red        "#CC1514")
         (magenta    "#E518FF")
         (orange     "#FF5C40")
         (yellow     "#E2D511")
         (green      "#A6E22E")
         (cyan       "#00FFDA")
         (cyan+1     (color-lighten-name cyan 30))
         (cyan-1     (color-darken-name cyan 30))
         (blue       "#3D7599")
         (dark-gray  "#444444")
         (lite-gray  "#eeeeee")

         ;; rainbow colors
         (rb-1 "#FF400D")
         (rb-2 "#008EFF")
         (rb-3 "#E80C6A")
         (rb-4 "#570CF8")
         (rb-5 "#AA309F"))

    (custom-theme-set-faces
     'zonokai

     ;; basic coloring
     `(highlight
       ((t
         (:background ,(color-darken-name zk-bg 5)))))
     `(default
        ((t
          (:foreground ,zk-fg :background ,zk-bg))))
     `(fringe
       ((,class
         (:foreground ,zk-comments))))
     `(shadow
       ((,class
         (:foreground ,zk-comments :background ,zk-fg))))
     `(match
       ((,class
         (:background ,zk-hl))))
     `(cursor
       ((,class
         (:foreground ,zk-bg :background ,zk-cursor :invserse-video t))))
     `(mouse
       ((,class
         (:foreground ,zk-bg :background ,zk-fg :inverse-video t))))
     `(button
       ((,class
         (:background ,zk-bg :foreground ,green :weight bold :underline t))))
     `(escape-glyph-face
       ((,class
         (:foreground ,red))))
     `(fringe
       ((,class
         (:foreground ,zk-fg :background ,zk-bg))))
     `(region
       ((,class
         (:background ,zk-region  :weight bold))))
     `(idle-highlight
       ((,class
         (:foreground ,cyan
                      :background ,blue))))
     `(hl-line
       ((,class
         (:background ,zk-hl-line
                      :foreground nil))))


     ;; compilation
     `(compilation-column-face
       ((,class
         (:foreground ,cyan :underline nil))))
     `(compilation-column-number

       ((,class
         (:foreground ,zk-bg :foreground ,cyan))))


     ;; diary
     `(diary
       ((,class
         (:foreground ,yellow))))


     ;;; help
     `(help-argument-name
       ((,class
         (:foreground ,cyan))))

     ;;; webmod
     `(web-mode-html-tag-face
       ((,class
         (:foreground ,yellow :weight bold :underline t))))


     ;; fontlock
     `(font-lock-builtin-face
       ((,class
         (:foreground ,orange :weight bold))))
     `(font-lock-comment-delimiter-face
       ((,class
         (:foreground ,zk-comments))))

     `(font-lock-comment-face
       ((,class
         (:foreground ,zk-comments))))

     `(font-lock-constant-face
       ((,class
         (:foreground ,yellow))))

     `(font-lock-string-face
       ((,class
         (:foreground ,magenta  :style :italic))))

     `(font-lock-keyword-face
       ((,class
         (:foreground ,blue
                      :weight bold))))

     `(font-lock-function-name-face
       ((,class
         (:foreground ,cyan
                      :weight bold))))

     `(font-lock-type-face
       ((,class
         (:foreground ,cyan))))

     `(font-lock-variable-name-face
       ((,class
         (:foreground ,green))))

     `(font-lock-doc-face
       ((,class
         (:foreground ,magenta))))
     `(font-lock-warning-face
       ((,class
         (:foreground ,yellow
                      :background ,zk-bg
                      :underline  t
                      :weight bold))))

     ;; misc faces
     `(mode-line
       ((,class
         (:inverse-video unspecified
                         :underline unspecified
                         :background ,(color-darken-name zk-bg 7)
                         :foreground ,zk-fg
                         :weight bold
                         :box (:line-width 1 :color ,(color-darken-name cyan 10) :style unspecified)))))
     `(mode-line-buffer-id
       ((,class
         (:foreground ,green :weight bold))))


     `(mode-line-inactive
       ((,class
         (:inverse-video unspecified
                         :underline unspecified
                         :foreground ,zk-fg
                         :weight bold
                         :background ,(color-darken-name zk-bg 30)
                        ))))

     `(secondary-selection
       ((,class
         (:foreground ,red))))

     ;;; Enh-ruby
     `(enh-ruby-string-delimiter-face
       ((,class
         (:foreground ,orange
                      :weight bold))))
     `(enh-ruby-op-face
       ((,class
         (:foreground ,yellow
                      :weight bold))))



     ;; Rainbow delimiters
     `(rainbow-delimiters-depth-1-face
       ((,class
         (:foreground ,rb-1))))

     `(rainbow-delimiters-depth-2-face
       ((,class
         (:foreground ,rb-2))))

     `(rainbow-delimiters-depth-3-face
       ((,class
         (:foreground ,rb-3))))

     `(rainbow-delimiters-depth-4-face
       ((,class
         (:foreground ,rb-4))))

     `(rainbow-delimiters-depth-5-face
       ((,class
         (:foreground ,rb-5))))

     `(rainbow-delimiters-depth-6-face
       ((,class
         (:foreground ,rb-1))))

     `(rainbow-delimiters-depth-7-face
       ((,class
         (:foreground ,rb-2))))

     `(rainbow-delimiters-depth-8-face
       ((,class
         (:foreground ,rb-3))))

     `(rainbow-delimiters-depth-9-face
       ((,class
         (:foreground ,rb-4))))

     `(rainbow-delimiters-depth-10-face
       ((,class
         (:foreground ,rb-5))))

     `(rainbow-delimiters-depth-11-face
       ((,class
         (:foreground ,rb-1))))

     `(rainbow-delimiters-depth-12-face
       ((,class
         (:foreground ,rb-2))))

     `(rainbow-delimiters-unmatched-face
       ((,class
         (:foreground ,zk-fg
                      :background ,zk-bg
                      :inverse-video t))))


     `(column-enforce-face
       ((,class
         (:foreground ,blue))))


     ;;; git gutter +
     `(git-gutter+-modified
       ((,class
         (:foreground ,magenta
                      :background ,zk-bg))))
     `(git-gutter+-added
       ((,class
         (:foreground ,green
                      :background ,zk-bg))))
     `(git-gutter+-deleted
       ((,class
         (:foreground ,red
                      :background ,zk-bg))))

     ;;; git gutter
     `(git-gutter:modified
       ((,class
         (:foreground ,magenta
                      :background ,zk-bg))))
     `(git-gutteradded
       ((,class
         (:foreground ,green
                      :background ,zk-bg))))
     `(git-gutter:deleted
       ((,class
         (:foreground ,red
                      :background ,zk-bg))))

   `(git-gutter-fr:added ((t (:foreground ,green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,magenta :weight bold))))

     ;; isearch
     `(isearch
       ((,class
         (:foreground ,zk-bg
                      :background ,green
                      :weight bold))))
     `(isearch-fail
       ((,class
         (:foreground ,zk-bg
                      :background ,red
                      :weight bold))))



     ;; external
     ;; ace-jump-mode
     `(ace-jump-face-background
       ((,class
         (:foreground ,dark-gray :background ,zk-bg
                      :inverse-video nil
                      :weight bold))))
     `(ace-jump-face-foreground
       ((,class
         (:foreground ,red :background ,zk-bg :inverse-video nil :weight bold))))


     ;; dired
     `(dired-directory
       ((,class
         (:foreground ,cyan :weight normal))))
     `(dired-flagged
       ((,class
         (:foreground ,red))))
     `(dired-header
       ((,class
         (:foreground ,magenta :background ,zk-bg))))
     `(dired-ignored
       ((,class
         (:inherit shadow))))



     ;;; magit
     ;; commit
     `(git-commit-summary-face
       ((,class
         (:foreground ,magenta))))
     `(git-commit-branch-face
       ((,class
         (:foreground ,cyan))))
     `(git-commit-comment-file-face
       ((,class
         (:foreground ,green))))
     `(git-commit-comment-heading-face
       ((,class
         (:foreground ,orange))))
     ;; log
     `(magit-section-title
       ((,class
         (:foreground ,green :background ,zk-bg :weight bold))))
     `(magit-log-sha1
       ((,class
         (:foreground ,cyan))))
     `(magit-branch
       ((,class
         (:foreground ,magenta :background ,zk-bg))))

     `(magit-key-mode-header-face
       ((,class
         (:foreground ,green))))
     `(magit-key-mode-button-face
       ((,class
         (:foreground ,yellow))))

     ;; rhtml-mode
     `(erb-out-delim-face
       ((,class
         (:foreground ,orange :background ,dark-gray))))
     `(erb-out-face
       ((,class
         (:background ,dark-gray :foreground ,yellow))))

     `(js2-function-call
       ((,class
         (:foreground ,cyan))))

     ;; sp pair overlay face
     `(sp-pair-overlay-face
       ((,class
         (:background ,zk-bg))))


     ;; Compnay
     `(company-tooltip
       ((t (:foreground ,zk-fg :background ,(color-darken-name zk-bg 10)))))
     `(company-tooltip-selection
       ((t (:foreground ,dark-gray :background ,cyan))))
     `(company-tooltip-mouse
       ((t (:background ,(color-darken-name red 30)))))
     `(company-tooltip-common
       ((t (:foreground ,green ))))
     `(company-tooltip-common-selection
       ((t (:background ,(color-darken-name blue 10)))))
     `(company-scrollbar-fg
       ((t (:background ,(color-darken-name cyan 20)))))
     `(company-scrollbar-bg
       ((t (:background ,(color-darken-name cyan 40)))))
     `(company-preview
       ((t (:background ,red))))
     `(company-preview-common
       ((t (:background ,zk-bg))))

      ;; eshell
     `(eshell-prompt ((t (:foreground ,yellow :weight bold))))
     `(eshell-ls-archive ((t (:foreground ,red :weight bold))))
     `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
     `(eshell-ls-directory ((t (:foreground ,blue :weight bold))))
     `(eshell-ls-executable ((t (:foreground ,red :weight bold))))
     `(eshell-ls-unreadable ((t (:foreground ,zk-fg))))
     `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
     `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
     `(eshell-ls-special ((t (:foreground ,yellow :weight bold))))
     `(eshell-ls-symlink ((t (:foreground ,cyan :weight bold))))


;;;;; smartparens
     `(sp-show-pair-mismatch-face ((t (:foreground ,red :background ,zk-bg :weight bold))))
     `(sp-show-pair-match-face ((t (:background ,zk-bg :weight bold))))


;;;;; undo-tree
     `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zk-fg :weight bold))))
     `(undo-tree-visualizer-current-face ((t (:foreground ,red :weight bold))))
     `(undo-tree-visualizer-default-face ((t (:foreground ,zk-fg))))
     `(undo-tree-visualizer-register-face ((t (:foreground ,yellow))))
     `(undo-tree-visualizer-unmodified-face ((t (:foreground ,cyan))))


;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zk-bg :foreground ,zk-bg))))
   `(whitespace-hspace ((t (:background ,zk-bg :foreground ,zk-bg))))
   `(whitespace-tab ((t (:background ,(color-lighten-name zk-bg 2)))))
   `(whitespace-newline ((t (:foreground ,zk-bg))))
   `(whitespace-trailing ((t (:background ,(color-darken-name magenta 20)))))
   `(whitespace-line ((t (:background ,zk-bg :foreground ,magenta))))
   `(whitespace-space-before-tab ((t (:background ,orange :foreground ,orange))))
   `(whitespace-indentation ((t (:background ,yellow :foreground ,red))))
   `(whitespace-empty ((t (:background ,yellow))))
   `(whitespace-space-after-tab ((t (:background ,yellow :foreground ,red))))
     )))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
(create-zonokai-theme)
(provide-theme 'zonokai)

;; Local Variables:
;; no-bytpe-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:

;;; zonokai-theme.el ends here
