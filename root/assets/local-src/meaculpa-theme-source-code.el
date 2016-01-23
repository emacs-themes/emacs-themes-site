;;; meacupla-theme.el --- meacupla theme for emacs

;; Copyright (C) 2015 Jeff Tecca

;; Author: Jeff Tecca <jefftecca@gmail.com>
;; Keywords: color theme meacupla faces
;; X-URL: https://gitlab.com/jtecca/meacupla-theme
;; URL: https://gitlab.com/jtecca/meacupla-theme

;; This file is not a part of GNU Emacs.

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

;; Meacupla is a light color theme for Emacs 24+  (deftheme).

;;; Usage:

;; Load the theme using the `load-theme' command or interactively by using
;; customize: `customize-themes'.

;; Credit:

;; Steve Purcell, from whom I used the basis of his Solarized color theme code
;; to set up the loading mechanisms for this theme.  In an earlier version, the
;; loading mechanism was much closer to his own code, but I heavily hacked it
;; to get it to its current state today.

;; William Stevenson, who wrote the bundled Adwaita theme on which this theme
;; was primarily forked from color-wise.

;;; Contributing:

;; Please feel free to send PRs for adjusting face colors or to add face
;; customizations for packages that aren't already covered.  Any help would be
;; appreciated.

;;; Code:

(deftheme meacupla "A light color theme with highlights of blue.")

(defmacro meacupla-with-colors (&rest body)
"Execute BODY with the color variables defined."
`(let ((black   "#2E3436")
       (onyx    "#555753")
       (grey0   "#888a85")
       (grey1   "#CCCCCC")
       (grey2   "#C3C7CF")
       (grey3   "#DBDBDB")
       (slate   "#C2D5E9")
       (white   "#FFFFFF")
       (hilite  "#FEFFBF")
       (yellow  "#EDD400")
       (orange  "#AB3400")
       (aqua    "#0076c1")
       (sky     "#0066CC")
       (royal   "#00578E")
       (deep    "#204A87")
       (leaf    "#324E16")
       (seaweed "#527c00")
       (green   "#4E9A06")
       (cyan    "#004d41")
       (violet  "#c80084")
       (berry   "#A52A2A")
       (rose    "#B50000")
       (apple   "#CC0000")
       (red     "#EF2929"))
   (let ((class '((class color) (min-colors 89))))
     ,@body)))

(defmacro meacupla-apply-faces (faces)
  "Create a face list to pass to `custom-theme-set-faces' based on the FACES list argument."
  `(mapcar
    (lambda (face)
      (list (car face) `((,class ,@(cdr face)))))
    ,faces))

;;; apply the faces
;; faces are separated into multiple face lists in the initial let bind in
;; order to keep the number of elements below what might trigger the
;; `max-lisp-eval-depth'.  if you add any forms to a face list and it errors
;; out because of the eval-depth, then break the new forms out into a new face
;; list.
;; these faces in these lists are grouped arbitrarily.
(progn
  (meacupla-with-colors
   (let ((faces1
          `(;; standard faces
            (default (:foreground ,black :background ,grey3))
            (bold (:weight bold))
            (italic (:slant italic))
            (bold-italic (:slant italic :weight bold))
            (underline (:underline t))
            (fixed-pitch (:inherit default))
            (border-color (:background ,grey3))
            (variable-pitch (:inherit default))
            (shadow (:foreground ,onyx))

            ;; frame faces
            (mode-line (:box (:line-width -1 :style released-button)
                             :background ,grey2 :foreground ,black :height
                             0.9))
            (mode-line-inactive (:foreground ,black :background ,grey0
                                             :height 0.9))
            (mode-line-highlight (:foreground ,black :background ,hilite
                                              :height 0.9))
            (mode-line-buffer-id (:foreground ,black :height 0.9))
            ;; header-line looks awesome with a dark background
            ;; but links are too dark to read
            (header-line (:foreground ,onyx :background ,grey2)) ; eg top of info
            (veritcal-border (:foreground ,black))
            (minibuffer-prompt (:foreground ,sky :bold t))
            (linum (:background ,grey2 :foreground ,black :height 0.9))
            (fringe (:background ,grey3))
            (cursor (:background ,red))
            ;;(tooltip ()) ; is this needed or used?  gtk values usually override
            (mouse (:inherit cursor))
            (widget-button (:bold t :foreground ,aqua))
            (widget-field (:background ,onyx :foreground ,grey3 :box
                                       (:line-width 1 :color ,black)))

            ;; dired faces
            (dired-header (:foreground ,grey3 :background ,black))
            (dired-directory (:foreground ,aqua :bold t))
            (dired-flagged (:foreground ,black :background ,hilite))
            (dired-ignored (:foreground ,grey0))
            (dired-mark (:foreground ,apple))
            (dired-marked (:foreground ,apple))
            (dired-perm-write (:foreground ,black :bold t))
            (dired-symlink (:foreground ,deep))
            (dired-warning (:foreground ,apple))

            ;; dired+
            (diredp-deletion (:inherit ,berry :inverse-video t))
            (diredp-deletion-file-name (:inherit ,berry))
            (diredp-dir-heading (:inherit dired-directory))
            (diredp-file-name (:inherit default))
            (diredp-file-suffix (:inherit default))
            (diredp-flag-mark (:inherit dired-marked))
            (diredp-flag-mark-line (:background ,grey2 :inherit diredp-flag-mark))

            ;; font lock faces
            (font-lock-builtin-face (:weight bold))
            (font-lock-comment-face (:foreground ,deep))
            (font-lock-comment-delimiter-face (:inherit font-lock-comment-face))
            (font-lock-constant-face (:foreground ,red))
            (font-lock-doc-face (:foreground ,seaweed))
            (font-lock-doc-string-face (:foreground ,seaweed))
            (font-lock-function-name-face (:foreground ,royal :weight bold))
            (font-lock-keyword-face (:foreground ,berry))
            (font-lock-negation-char-face (:foreground ,deep))
            (font-lock-preprocessor-face (:foreground ,violet))
            (font-lock-regexp-grouping-backslash (:foreground ,onyx))
            (font-lock-regexp-grouping-construct (:foreground ,grey0))
            (font-lock-string-face (:foreground ,aqua))
            (font-lock-type-face (:foreground ,cyan))
            (font-lock-variable-name-face (:foreground ,aqua :bold t))
            (font-lock-warning-face (:weight bold :foreground ,apple))

            ;; status faces
            (success (:foreground ,green))
            (warning (:foreground ,orange))
            (error (:foreground ,rose))

            ;; highlighting faces
            (link (:underline t :foreground ,sky))
            (link-visited (:underline t :foreground ,violet))
            (highlight (:foreground ,black :background ,hilite))
            (isearch (:foreground ,white :background ,deep)) ; for matches
            (match (:foreground ,white :background ,royal))
            (lazy-highlight (:foreground ,onyx :background ,hilite))
            (isearch-lazy-highlight-face (:inherit lazy-highlight))
            (isearch-fail (:foreground ,white :background ,apple))
            (region (:foreground unspecified :background ,hilite))
            (secondary-selection (:foreground unspecified :background ,yellow))
            (trailing-whitespace (:foreground ,hilite :background ,grey3))
            (escape-glyph (:foreground ,aqua))
            (nobreak-space (:foreground ,aqua))

            ;; highlight numbers faces
            (highlight-numbers-number (:foreground ,apple))

            ;; built-in parens faces
            (show-paren-match (:foreground ,white :background ,royal))
            (show-paren-mismatch (:foreground ,white :background ,rose))))

         (faces2
          `(
            ;; anzu faces
            (anzu-mode-line (:bold t :foreground ,sky :height 0.9))
            (anzu-replace-highlight (:foreground ,white :background ,royal))
            (anzu-replace-to (:bold t :box t :foreground ,apple))
            (anzu-match-1 (:foreground ,white :background ,aqua))
            (anzu-match-2 (:foreground ,white :background ,orange))
            (anzu-match-3 (:foreground ,white :background ,leaf))


            ;; grep faces
            (grep-context-face (:foreground ,black))
            (grep-error-face (:inherit error))
            (grep-hit-face (:foreground ,green))
            (grep-match-face (:inherit match))

            ;; evil faces
            (evil-ex-commands (:inherit default :underline t))
            (evil-ex-lazy-highlight (:inherit lazy-highlight))
            (evil-ex-search (:inherit isearch))
            (evil-ex-substitute-matches (:foreground ,white :background ,royal))
            (evil-ex-substitute-replacement (:bold t :box t :foreground ,apple))

            ;; rainbow delimiter faces
            (rainbow-delimiters-depth-1-face (:foreground ,black))
            (rainbow-delimiters-depth-2-face (:foreground ,apple))
            (rainbow-delimiters-depth-3-face (:foreground ,grey0))
            (rainbow-delimiters-depth-4-face (:foreground ,leaf))
            (rainbow-delimiters-depth-5-face (:foreground ,orange))
            (rainbow-delimiters-depth-6-face (:foreground ,cyan))
            (rainbow-delimiters-depth-7-face (:foreground ,green))
            (rainbow-delimiters-depth-8-face (:foreground ,sky))
            (rainbow-delimiters-depth-9-face (:foreground ,violet))
            (rainbow-delimiters-unmatched-face (:inherit show-paren-mismatch))

            ;; smartparens faces
            (sp-show-pair-match-face (:inherit show-paren-match))
            (sp-show-pair-mismatch-face (:inherit show-paren-mismatch))

            ;; mic-paren faces
            (paren-face-match (:inherit show-paren-match))
            (paren-face-mismatch (:inherit show-paren-mismatch))
            (paren-face-no-match (:inherit show-paren-mismatch))

            ;; paren-face faces
            (paren-face (:foreground ,grey1))

            ;; diff faces
            (diff-added (:bold t :foreground ,green))
            (diff-removed (:bold t :foreground ,rose))

            ;; regex-tool faces
            (reb-match-0 (:inherit match))
            (reb-match-1 (:foreground ,white :background ,leaf))
            (reb-match-2 (:foreground ,white :background ,violet))
            (reb-match-3 (:foreground ,white :background ,berry))

            ;; helm faces
            (helm-M-x-key (:foreground ,orange))
            (helm-buffer-directory (:foreground ,aqua :bold t))
            (helm-buffer-file (:foreground ,black :italic t))
            (helm-buffer-saved-out (:foreground ,black :bold t))
            (helm-buffer-not-saved (:foreground ,violet :italic t))
            (helm-buffer-process (:foreground ,apple :bold t))
            (helm-buffer-size (:foreground ,grey0))
            (helm-etags-file (:box t :foreground ,aqua))
            (helm-ff-directory (:foreground ,aqua :bold t))
            (helm-ff-dotted-directory (:inherit helm-ff-directory))
            (helm-ff-dotted-symlink-directory (:inherit helm-ff-directory :box t))
            (helm-ff-executable (:foreground ,apple))
            (helm-ff-invalid-symlink (:foreground ,white :background ,berry))
            (helm-ff-symlink (:foreground ,orange))
            (helm-ff-file (:foreground ,black))
            (helm-header (:foreground ,white :background ,black :height 1.1))
            (helm-candidate-number (:bold t :box t :foreground ,white :background ,aqua))
            (helm-selection (:foreground ,black :background ,grey1))
            (helm-source-header (:bold t :foreground ,white :background ,royal :height 1.1))
            (helm-visible-mark (:background ,grey2))
            (helm-visual-mark (:bold t :foreground ,white :background ,yellow))

            ;; magit faces
            (magit-branch-local (:foreground ,royal))
            (magit-branch-current (:foreground ,aqua))
            (magit-branch-remote (:foreground ,violet))
            (magit-diff-added (:bold t :foreground ,green))
            (magit-filename (:inherit default))
            (magit-section-heading (:foreground ,apple :box t))
            (magit-section-heading-selection (:background ,grey2))
            (magit-section-highlight (:inherit magit-section-heading-selection))
            (magit-section-secondary-heading (:foreground ,violet))
            (magit-hash (:foreground ,onyx))
            (magit-header-line (:foreground ,onyx :background ,grey2))
            (magit-log-auther (:foreground ,cyan))
            (magit-log-date (:foreground ,black))
            (magit-popup-argument (:foreground ,green))
            (magit-popup-disabled-argument (:foreground ,grey0))
            (magit-popup-heading (:foreground ,sky))
            (magit-popup-key (:bold t :foreground ,royal))
            (magit-popup-option-value (:foreground ,leaf))
            (magit-tag (:foreground ,onyx :box t))

            ;; org faces
            (org-priority (:bold t :foreground ,black))
            (org-date (:underline t :foreground ,violet))
            (org-agenda-date (:underline t :foreground ,violet))
            (org-agenda-done (:box t :foreground ,green))
            (org-sexp-date (:underline t :foreground ,aqua))
            (org-done (:box t :foreground ,green))
            (org-todo (:box t :foreground ,berry))
            (org-table (:foreground ,onyx))
            (org-document-title (:bold t :foreground ,black :height 1.1))
            (org-special-keyword (:foreground ,orange))
            (org-link (:underline t :foreground ,violet))
            (org-hide (:foreground ,grey3 :background ,grey3))
            (org-document-info (:foreground ,grey0))
            (org-document-info-keyword (:inherit org-document-info :bold t))
            (org-warning (:bold t :foreground ,rose))
            (org-code (:foreground ,black))
            (org-block (:background ,grey2))
            (org-block-background (:background ,slate))
            (org-clock-overlay (:foreground ,black :background ,yellow))
            (org-drawer (:foreground ,deep))

            ;; smart-mode-line faces
            (sml/charging (:foreground ,green))
            (sml/col-number (:foreground ,black :bold t))
            (sml/discharging (:foreground ,berry))
            (sml/filename (:foreground ,black :bold t))
            (sml/folder (:foreground ,onyx))
            (sml/git (:foreground ,royal))
            (sml/line-number (:foreground ,black :bold t))
            (sml/minor-modes (:foreground ,onyx))
            (sml/modes (:foreground ,onyx :bold t))
            (sml/modified (:foreground ,berry))
            (sml/numbers-separator (:inherit sml/col-number))
            (sml/outside-modified (:foreground ,violet))
            (sml/position-percentage (:bold t :foreground ,black))
            (sml/prefix (:foreground ,onyx))
            (sml/read-only (:bold t :foreground ,orange))
            (sml/vc (:foreground ,green))
            (sml/vc-edited (:foreground ,berry))

            ;; nxml faces
            (nxml-name-face (:foreground unspecified :inherit font-lock-constant-face))
            (nxml-attribute-local-name-face (:foreground unspecified :inherit font-lock-variable-name-face))
            (nxml-ref-face (:foreground unspecified :inherit font-lock-preprocessor-face))
            (nxml-delimiter-face (:foreground unspecified :inherit font-lock-keyword-face))
            (nxml-delimited-data-face (:foreground unspecified :inherit font-lock-string-face))
            (rng-error-face (:inherit error))

            ;; eshell faces
            (eshell-ls-archive (:foreground ,cyan))
            (eshell-ls-backup (:foreground ,white :background ,cyan))
            (eshell-ls-clutter (:foreground ,grey0))
            (eshell-ls-directory (:foreground ,aqua :bold t))
            (eshell-ls-executable (:foreground ,apple))
            (eshell-ls-missing (:foreground ,violet))
            (eshell-ls-product (:foreground ,onyx))
            (eshell-ls-readonly (:foreground ,black :underline t))
            (eshell-ls-special (:foreground ,leaf))
            (eshell-ls-symlink (:foreground ,orange))
            (eshell-ls-unreadable (:foreground ,berry :underline t))
            (eshell-prompt (:foreground ,deep :bold t))))

         (faces3
          `(
            ;; undo-tree
            (undo-tree-visualizer-default-face (:inherit default))
            (undo-tree-visualizer-current-face (:foreground ,red :bold t))
            (undo-tree-visualizer-active-branch-face (:foreground ,deep))
            (undo-tree-visualizer-register-face (:inherit default :bold t))
            (undo-tree-visualizer-unmodified-face (:inherit default :underline t))

            ;; ido faces
            (ido-subdir (:foreground ,aqua :bold t))
            (ido-first-match (:foreground ,orange))
            (ido-only-match (:foreground ,red))
            (ido-indicator (:foreground ,red :underline t))
            (ido-virtual (:foreground ,deep))

            ;; company faces
            (company-preview (:foreground ,grey0))
            (company-preview-common (:inherit company-preview))
            (company-preview-search (:foreground ,aqua))
            (company-tooltip (:background ,grey1))
            (company-tooltip-selection (:foreground ,grey3 :background ,deep))
            (company-tooltip-common (:inherit company-tooltip
                                              :foreground ,aqua))
            (company-tooltip-common-selection (:inherit
                                               company-tooltip-selection
                                               :foreground ,white))
            (company-scrollbar-bg (:inherit company-tooltip
                                            :background ,grey0))
            (company-scrollbar-fg (:background ,grey2))
            (company-tooltip-search (:inherit company-tooltip
                                              :foreground ,deep))
            (company-tooltip-annotation (:inherit company-tooltip :foreground ,black))
            (company-echo-common (:inherit company-echo :foreground ,royal))
            (company-tooltip-mouse (:foreground ,black :background ,hilite))

            ;; slime faces
            (slime-error-face (:inherit error))
            (slime-highlight-face (:inherit highligh))
            (slime-reader-conditional-face (:inherit default :italic t))
            (slime-repl-input-face (:inherit default))
            (slime-repl-inputed-output-face (:foreground ,royal))
            (slime-repl-output-face (:foreground ,deep))
            (slime-repl-output-mouseover-face (:foreground ,deep :box t))
            (slime-repl-prompt-face (:foreground ,green :bold t))
            (slime-repl-result-face (:foreground ,black :bold t))
            (slime-style-warning-face (:inherit warning :underline t))
            (slime-warning-face (:inherit warning))

            ;; python faces
            (py-builtins-face (:foreground ,berry))

            ;; compiltion faces
            ;; most faces inherit from 'success, 'error, 'warning', etc
            (compilation-column-number (:foreground ,black))
            (compilation-line-number (:foreground ,black))
            (compilation-message-face (:foreground ,black))
            (compilation-mode-line-exit (:foreground ,green))
            (compilation-mode-line-fail (:foreground ,apple))
            (compilation-mode-line-run (:foreground ,orange))

            ;; flycheck faces
            (flycheck-error (:inherit error))
            (flycheck-info (:foreground ,leaf))
            (flycheck-warning (:inherit warning))
            (flycheck-fringe-error (:foreground ,grey3 :background ,red))
            (flycheck-fringe-info (:foreground ,grey3 :background ,leaf))
            (flycheck-fringe-warning (:foreground ,grey3 :background ,orange))

            ;; flymake faces
            (flymake-warnline (:inherit warning))
            (flymake-errline (:inherit error))

            ;; flx faces
            (flx-highlight-face (:inherit :hilite))

            ;; term faces
            (term-color-black (:background ,grey3 :foreground ,black))
            (term-color-deep (:background grey3 :foreground ,deep))
            (term-color-cyan (:background ,grey3 :foreground ,cyan))
            (term-color-green (:background ,grey3 :foreground ,green))
            (term-color-violet (:background ,grey3 :foreground ,violet))
            (term-color-red (:background ,grey3 :foreground ,red))
            (term-color-white (:background ,grey3 :foreground ,onyx))
            (term-color-yellow (:background ,grey3 :foreground ,orange))

            ;; erc faces
            (erc-direct-msg-face (:foreground ,orange))
            (erc-error-face (:inherit error))
            (erc-header-face (:foreground ,onyx :background ,grey2))
            (erc-input-face (:foreground ,green))
            (erc-current-nick-face (:foreground ,aqua))
            (erc-my-nick-face (:foreground ,green))
            (erc-nick-default-face (:foreground ,violet))
            (erc-nick-msg-face (:bold t :foreground ,orange))
            (erc-notice-face (:foreground ,deep))
            (erc-pal-face (:foreground ,orange))
            (erc-prompt-face (:foreground ,deep))
            (erc-timestamp-face (:foreground ,cyan))
            (erc-keyword-face (:foreground ,green))

            ;; powerline faces
            (powerline-active1 (:foreground ,white :background ,grey0))
            (powerline-active2 (:foreground ,white :background ,onyx))
            (powerline-inactive1 (:foreground ,grey2 :background ,black))
            (powerline-inactive2 (:foreground ,grey1 :background ,onyx))

            ;; customize faces
            (custom-group-tag (:foreground ,deep :height 1.1 :bold t))
            (custom-group-tag-1 (:foreground ,deep :bold t))
            (custom-state (:foreground ,aqua))
            (custom-variable-tag (:inherit custom-group-tag-1))

            ;; whitespace mode faces
            (whitespace-line (:foreground ,hilite :background ,onyx))
            (whitespace-newline (:foreground ,onyx))
            (whitespace-space (:inherit default :foreground ,red))
            (whitespace-tab (:foreground ,grey3 :background ,onyx))
            (whitespace-trailing (:foreground ,red :background ,hilite))

            ; avy faces
            (avy-goto-char-timer-face (:foreground ,black :background ,hilite))
            (avy-lead-face (:foreground ,white :background ,aqua))
            (avy-lead-face-0 (:foreground ,white :background ,orange))
            (avy-lead-face-1 (:foreground ,white :background ,leaf))
            (avy-lead-face-2 (:foreground ,white :background ,rose))

            ; swiper faces
            (swiper-minibuffer-match-face-1 (:foreground ,white :background ,aqua))
            (swiper-minibuffer-match-face-2 (:foreground ,white :background ,orange))
            (swiper-minibuffer-match-face-3 (:foreground ,white :background ,leaf))
            (swiper-minibuffer-match-face-4 (:foreground ,white :background ,violet))
            (swiper-match-face-1 (:foreground ,black :background ,hilite))
            (swiper-match-face-2 (:foreground ,white :background ,deep))
            (swiper-match-face-3 (:foreground ,white :background ,violet))
            (swiper-match-face-4 (:foreground ,white :background ,orange))
            (swiper-line-face (:foreground ,black :background ,hilite))
            )))
     ;; apply theme face colors for each of the face lists
     (progn
       (apply 'custom-theme-set-faces 'meacupla
              (meacupla-apply-faces faces1))
       (apply 'custom-theme-set-faces 'meacupla
              (meacupla-apply-faces faces2))
       (apply 'custom-theme-set-faces 'meacupla
              (meacupla-apply-faces faces3))
       (provide-theme 'meacupla)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; meacupla-theme.el ends here
