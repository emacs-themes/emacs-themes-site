;;; farmhouse-theme-common.el --- Farmhouse Theme, Face-setting macro

;; Copyright 2015 Matthew Lyon

;; Author: Matthew Lyon <matthew@lyonheart.us>
;; URL: https://github.com/mattly/emacs-farmhouse-theme
;; Package-Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Requires Emacs 24 or greater.
;;

;;; Code:
(defvar farmhouse-color-defs
  '((gray0 "#1D1D29")
    (gray1 "#272b34")
    (gray2 "#495259")
    (gray4 "#7b8383")
    (gray5 "#9c9b95")
    (gray6 "#c0beb9")
    (gray7 "#dedbd7")
    (gray8 "#ede9e7")
    (gray9 "#f6f2f3")
    (red1 "#9e000d")
    (red2 "#c60013")
    (red3 "#ef001b")
    (yellow1 "#b7690a")
    (yellow2 "#eea806")
    (yellow3 "#fcce16")
    (green1 "#4a8b0d")
    (green2 "#8cc610")
    (green3 "#b0e60e")
    (cyan1 "#2c9f6b")
    (cyan2 "#1ee079")
    (cyan3 "#2aed75")
    (blue1 "#124cd3")
    (blue2 "#088afb")
    (blue3 "#23cbfe")
    (purple1 "#941458")
    (purple2 "#ec3691")
    (purple3 "#fa5fa8")))

(defun farmhouse-color (symbol)
  "Extracs a color from farmhouse-color-defs by SYMBOL."
  (nth 1 (assoc symbol farmhouse-color-defs)))

(defun farmhouse-color-remap (defs symbol)
  "Builds palette DEFS by remapping color definition at SYMBOL."
  (list defs (farmhouse-color symbol)))

(defmacro farmhouse-theme--set-faces (name palette)
  "Called by the theme with a NAME and PALETTE to set the faces."
  `(let* ((class '((class color) (min-colors 24)))

          ;; set all colors from the master palette to "farm-<color>"
          ,@(mapcar (lambda (colordef)
                      (list (intern (concat "farm-" (symbol-name (nth 0 colordef))))
                            (nth 1 colordef)))
                    farmhouse-color-defs)
          ;; set all colors from the local palette to the symbol in the palette
          ,@(mapcar (lambda (colordef) (apply 'farmhouse-color-remap colordef)) palette)

          ;; common styles
          (diff-added `(:foreground ,green2))
          (diff-hunk-header `(:background ,base5))
          (diff-file-header `(:foreground ,purple2 :height 1.1 :weight bold))
          (diff-removed `(:foreground ,red2))
          (directory `(:foreground ,blue1))
          (error `(:foreground ,red2))
          (link `(:weight normal :underline t :foreground ,blue1))
          (ol1 `(:weight bold :height 1.2 :foreground ,green1))
          (ol2 `(:weight bold :height 1.1 :foreground ,cyan1))
          (ol3 `(:weight bold :foreground ,blue1))
          (ol4 `(:weight bold :foreground ,purple1))
          (ol5 `(:weight bold :foreground ,yellow1))
          (ol6 `(:weight bold :slant italic :foreground ,green1))
          (ol7 `(:weight bold :slant italic :foreground ,cyan1))
          (ol8 `(:weight bold :foreground ,blue1))
          (ol9 `(:weight bold :foreground ,purple1))
          (paren-matched `(:background ,cyan2))
          (paren-unmatched `(:background ,red2))
          )
     (custom-theme-set-faces
      ',name
      `(default ((,class (:background ,base6 :foreground ,base1))))
      `(bold ((,class (:weight bold))))

      `(font-lock-builtin-face ((,class (:foreground ,blue1 :weight bold))))
      `(font-lock-comment-face ((,class (:foreground ,base3))))
      `(font-lock-comment-delimiter-face ((,class (:foreground ,base3))))
      `(font-lock-constant-face ((,class (:foreground ,green2 :weight bold))))
      `(font-lock-function-name-face ((,class (:foreground ,cyan1))))
      `(font-lock-keyword-face ((,class (:foreground ,purple1))))
      `(font-lock-negation-char-face ((,class (:foreground ,purple2))))
      `(font-lock-preprocessor-face ((,class (:foreground ,blue2))))
      `(font-lock-regexp-grouping-construct ((,class (:foreground ,cyan2))))
      `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyan2))))
      `(font-lock-string-face ((,class (:foreground ,green1))))
      `(font-lock-type-face ((,class (:foreground ,red1))))
      `(font-lock-variable-name-face ((,class (:foreground ,yellow1))))
      `(font-lock-warning-face ((,class (:foreground ,purple2))))
      `(success ((,class (:foreground ,green2))))
      `(warning ((,class (:foreground ,yellow2))))
      `(error ((,class ,error)))


      `(match ((,class (:background ,yellow1 :foreground ,base1))))
      `(isearch ((,class (:background ,yellow1 :foreground ,base1 :weight bold))))
      `(isearch-fail ((,class (:background ,red1 :foreground ,base1))))
      `(lazy-highlight ((,class (:background ,yellow1))))
      `(trailing-whitespace ((,class (:background ,base5))))

      `(evil-search-highlight-persist-highlight-face ((,class (:background ,farm-yellow3 :foreground ,farm-gray0))))

      `(region ((,class (:background ,base5))))

      ;; ui chrome
      `(hi-line ((,class (:background ,base7))))
      `(highlight ((,class (:background ,base7))))
      `(cursor ((,class (:background ,blue2))))
      `(fringe ((,class (:background ,base6))))
      `(border ((,class (:background ,base6))))
      `(linum ((,class (:background ,base6 :foreground ,base3))))
      `(linum-highlight-face ((,class (:inverse-video nil :background ,base3))))
      `(vertical-border ((,class (:foreground ,base4))))
      `(minibuffer-prompt ((,class (:foreground ,blue1))))

      `(link ((,class ,link)))
      `(link-visited ((,class ,link)))

      ;; modeline and powerline
      `(mode-line ((,class (:background ,base7 :foreground ,base1))))
      `(powerline-active1 ((,class (:background ,base5 :foreground ,base2))))
      `(powerline-active2 ((,class (:background ,base5 :foreground ,base2))))

      `(mode-line-inactive ((,class (:inherit mode-line
                                              :foreground ,base2
                                              :background ,base5
                                              :box nil))))
      `(powerline-inactive1 ((,class (:background ,base4 :foreground ,base2))))
      `(powerline-inactive2 ((,class (:background ,base4 :foreground ,base2))))

      ;; term
      `(term-color-black ((,class (:foreground ,black))))
      `(term-color-red ((,class (:foreground ,red2))))
      `(term-color-green ((,class (:foreground ,green2))))
      `(term-color-yellow ((,class (:foreground ,yellow2))))
      `(term-color-blue ((,class (:foreground ,blue2))))
      `(term-color-purple ((,class (:foreground ,purple2))))
      `(term-color-cyan ((,class (:foreground ,cyan2))))
      `(term-color-white ((,class (:foreground ,white))))

      ;; auto-complete
      `(ac-candidate-face ((,class (:background ,base5 :foreground ,base2))))
      `(ac-selection-face ((,class (:background ,base7 :foreground ,base1))))
      `(popup-tip-face ((,class (:background ,base5 :foreground ,base2))))
      ;; company mode
      `(company-tooltip ((,class (:background ,base5 :foreground ,base2))))
      `(company-tooltip-selection ((,class (:background ,base7 :foreground ,base1))))
      `(company-tooltip-annotation ((,class (:background ,base7 :foreground ,base1))))
      `(company-tooltip-mouse ((,class (:background ,base5))))
      `(company-tooltip-common ((,class (:foreground ,blue1))))
      `(company-tooltip-common-selection ((,class (:foreground ,blue2))))
      `(company-scrollbar-fg ((,class (:background ,base3))))
      `(company-scrollbar-bg ((,class (:background ,base5))))
      `(company-preview ((,class (:background ,base5))))
      `(company-preview-common ((,class (:background ,base5 :foreground ,green2))))

      ;; diff
      `(diff-added ((,class ,diff-added)))
      `(diff-changed ((,class (:foreground ,yellow2))))
      `(diff-context ((,class (:foreground ,base2))))
      `(diff-file-header ((,class ,diff-file-header)))
      `(diff-hunk-header ((,class ,diff-hunk-header)))
      `(diff-removed ((,class ,diff-removed)))

      ;; eshell
      `(eshell-prompt ((,class (:foreground ,red2 :weight bold))))

      ;; helm
      `(helm-buffer-directory ((,class ,directory)))
      `(helm-buffer-file ((,class (:foreground ,base1))))
      `(helm-buffer-not-saved ((,class (:foreground ,red1))))
      `(helm-buffer-process ((,class (:foreground ,yellow1))))
      `(helm-buffer-saved-out ((,class (:background ,yellow2 :foreground ,black))))
      `(helm-buffer-size ((,class (:foreground ,base3))))
      `(helm-candidate-number ((,class (:foreground ,purple2))))
      `(helm-ff-directory ((,class ,directory)))
      `(helm-ff-dotted-directory ((,class (:foreground ,cyan1))))
      `(helm-ff-executable ((,class (:foreground ,purple1))))
      `(helm-ff-file ((,class (:foreground ,base1))))
      `(helm-ff-invalid-symlink ((,class ,error)))
      `(helm-ff-symlink ((,class (:foreground ,red1))))
      `(helm-selection ((,class (:background ,blue2))))
      `(helm-selection-line ((,class (:background ,blue2))))
      `(helm-separator ((,class (:foreground ,yellow2))))
      `(helm-source-header ((,class (:weight bold :height 1.3 :family "Sans Serif"))))

      ;; magit
      `(magit-branch ((,class (:weight bold :height 1.2 :foreground ,purple2))))
      `(magit-diff-add ((,class ,diff-added)))
      `(magit-diff-del ((,class ,diff-removed)))
      `(magit-diff-hunk-header ((,class ,diff-hunk-header)))
      `(magit-diff-file-header ((,class ,diff-file-header)))
      `(magit-item-highlight ((,class (:background ,base7))))
      `(magit-log-sha1 ((,class (:foreground ,cyan1))))
      `(magit-log-head-label ((,class (:foreground ,green1))))
      `(magit-log-head-label-head ((,class (:foreground ,purple1 :weight bold))))
      `(magit-log-head-label-local ((,class (:foreground ,green1 :weight bold))))
      `(magit-log-head-label-remote ((,class (:foreground ,cyan1 :weight bold))))
      `(magit-log-head-label-tags ((,class (:foreground ,yellow1 :weight bold))))
      `(magit-log-tag-label ((,class (:foreground ,yellow1))))

      ;; org-mode
      `(org-block ((,class (:background ,base5))))
      `(org-block-begin-line ((,class (:background ,base5 :foreground ,base3))))
      `(org-block-end-line ((,class (:background ,base5 :foreground ,base3))))
      `(org-date ((,class (:forefound ,cyan1))))
      `(org-footnote ((,class (:foreground ,cyan2 :slant italic :underline t))))
      `(org-hide ((,class (:foreground ,base5))))
      `(org-level-1 ((,class ,ol1)))
      `(org-level-2 ((,class ,ol2)))
      `(org-level-3 ((,class ,ol3)))
      `(org-level-4 ((,class ,ol4)))
      `(org-level-5 ((,class ,ol5)))
      `(org-level-6 ((,class ,ol6)))
      `(org-level-7 ((,class ,ol7)))
      `(org-level-8 ((,class ,ol8)))
      `(org-level-9 ((,class ,ol9)))
      `(org-link ((,class ,link)))
      `(org-list-dt ((,class (:weight bold :slant italic))))
      `(org-special-keyword ((,class (:foreground ,yellow1 :weight bold))))
      `(org-target ((,class (:foreground ,base2 :underline t :weight bold))))
      `(org-table ((,class (:foreground ,base2))))
      `(org-todo ((,class (:foreground ,red1 :weight bold))))
      `(outline-1 ((,class ,ol1)))
      `(outline-2 ((,class ,ol2)))
      `(outline-3 ((,class ,ol3)))
      `(outline-4 ((,class ,ol4)))
      `(outline-5 ((,class ,ol5)))
      `(outline-6 ((,class ,ol6)))
      `(outline-7 ((,class ,ol7)))
      `(outline-8 ((,class ,ol8)))

      ;; paren
      `(show-paren-match ((,class ,paren-matched)))
      `(show-paren-mismatch ((,class ,paren-unmatched)))
      `(paren-face-match ((,class ,paren-matched)))
      `(paren-face-mismatch ((,class ,paren-unmatched)))
      `(paren-face-no-match ((,class ,paren-unmatched)))

      ;; Prodigy
      `(prodigy-green-face ((,class (:foreground ,green1))))
      `(prodigy-red-face ((,class (:foreground ,red1))))
      `(prodigy-yellow-face ((,class (:foreground ,yellow1))))

      ;; rainbow-delimiters
      `(rainbow-delimiters-depth-1-face ((,class (:foreground ,base1))))
      `(rainbow-delimiters-depth-2-face ((,class (:foreground ,base2))))
      `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yellow1))))
      `(rainbow-delimiters-depth-4-face ((,class (:foreground ,green1))))
      `(rainbow-delimiters-depth-5-face ((,class (:foreground ,cyan1))))
      `(rainbow-delimiters-depth-6-face ((,class (:foreground ,blue1))))
      `(rainbow-delimiters-depth-9-face ((,class (:foreground ,purple1))))
      `(rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow2))))
      `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green2))))
      `(rainbow-delimiters-depth-10-face ((,class (:foreground ,cyan2))))
      `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue2))))
      `(rainbow-delimiters-depth-12-face ((,class (:foreground ,purple2))))
      `(rainbow-delimiters-mismatched-face ((,class ,paren-unmatched)))
      `(rainbow-delimiters-unmatched-face ((,class ,paren-unmatched)))

      )
     (custom-theme-set-variables
      ',name
      `(ansi-color-names-vector
        [,base6 ,red2 ,green2 ,yellow2 ,blue2 ,purple2 ,cyan2 ,base1])
      `(when (or (not (boundp 'ansi-term-color-vector))
                 (not (facep (aref ansi-term-color-vector 0))))
         (ansi-term-color-vector
          [unspecified ,base6 ,red2 ,green2 ,yellow2 ,blue2 ,purple2 ,base1]))
      )))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'farmhouse-theme-common)
;;; farmhouse-theme-common.el ends here






;;; farmhouse-light-theme.el --- Farmhouse Theme, Dark version

;; Copyright 2015 Matthew Lyon

;; Author: Matthew Lyon <matthew@lyonheart.us>
;; URL: https://github.com/mattly/emacs-farmhouse-theme
;; Package-Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Requires Emacs 24 or greater.
;;

;;; Code:

(eval-when-compile (require 'farmhouse-theme-common))
(deftheme farmhouse-light "The light version of the farmhouse theme.")

(farmhouse-theme--set-faces
 farmhouse-light
 ((base1 gray1)
  (base2 gray2)
  (base3 gray4)
  (base4 gray5)
  (base5 gray7)
  (base6 gray8)
  (base7 gray9)
  (black gray1)
  (white gray8)

  (red1 red1)
  (red2 red3)
  (purple1 purple1)
  (purple2 purple2)

  (blue1 blue1)
  (blue2 blue3)
  (cyan1 cyan1)
  (cyan2 cyan2)

  (green1 green1)
  (green2 green2)
  (yellow1 yellow1)
  (yellow2 yellow2)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'farmhouse-light)

;;; farmhouse-light-theme.el ends here




;;; farmhouse-dark-theme.el --- Farmhouse Theme, Dark version

;; Copyright 2015 Matthew Lyon

;; Author: Matthew Lyon <matthew@lyonheart.us>
;; URL: https://github.com/mattly/emacs-farmhouse-theme
;; Package-Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Requires Emacs 24 or greater.
;;

;;; Code:

(eval-when-compile (require 'farmhouse-theme-common))
(deftheme farmhouse-dark "The dark version of the farmhouse theme.")

(farmhouse-theme--set-faces
 farmhouse-dark
 ((base1 gray9)
  (base2 gray7)
  (base3 gray5)
  (base4 gray4)
  (base5 gray2)
  (base6 gray1)
  (base7 gray0)
  (black gray8)
  (white gray1)

  (red1 red3)
  (red2 red2)
  (purple1 purple3)
  (purple2 purple2)

  (blue1 blue3)
  (blue2 blue2)
  (cyan1 cyan3)
  (cyan2 cyan2)

  (green1 green3)
  (green2 green2)
  (yellow1 yellow3)
  (yellow2 yellow2)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'farmhouse-dark)

;;; farmhouse-dark-theme.el ends here
