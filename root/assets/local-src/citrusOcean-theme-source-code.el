;;; citrusOcean-theme.el --- citrusOcean theme

;;Copyright(C) 2022 int

;; Author: Ying Tzu Huang <huanginch329@gmail.com>
;; Version: 1.3

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


;;;Commentary:
;; you should install font Montserrat to your computer first before using this theme
;;url: https://fonts.google.com/specimen/Montserrat
;;To use rainbow-delimeters you should install rainbow-delimeters from melpha
;;url: https://melpa.org/#/rainbow-delimiters
;; or you can use M-x package-install to install

;;; Code:
(deftheme citrusOcean
  "A dark theme base on orange and blue")

;; set color and font variables
(let ((font-default "Montserrat")
      (font-arial "Arial")
      (font-serif "sans-serif")
      (primary "#6BC0EE")
      (secondary "#fff8dc")
      (string "#91F9E5")
      (light "#FAF36F")
      (keyword "#F3AB4D")
      (background "#000000")
      (line "#fff5ee")
      (shadow "#7a8b8b")
      (high-contrast "#b22222")
      (fun-name "#daa520")
      (builtin "#b0c4de")
      (type "#98fb98")
      (link "#87ceeb")) ;;set variable ends here

;;rainbow-delimeter variables
(custom-set-variables
 '(package-selected-packages '(rainbow-delimiters)))

;;custom face setting
(custom-set-faces
 `(rainbow-delimiters-depth-1-face ((t (:foreground ,keyword))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground ,link))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground "gold3"))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground ,primary))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground ,light))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground ,string))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground "orange"))))
 `(rainbow-delimiters-depth-8-face ((t (:foreground ,primary)))) )

   ;;;;Theme Faces
  (custom-theme-set-faces
   'citrusOcean

   ;;default setting
   `(default ((t (
                   :family ,font-default ,font-arial ,font-serif
                   :width normal
                   :weight normal
                   :slant normal
                   :foreground ,primary
                   :distant-foreground ,light
                   :background ,background
                   :underline nil
                   :overline nil
                   :strike-through nil
                   :box nil
                   :inverse-video nil
                   :inherit nil ))))

   ;; base setting
   `(cursor (( t (:background ,light )) ))
   `(show-paren-match ((t (:background "#191970")) ))

   ;;frame and window setting
   `(fringe (( t (:background ,shadow)) ))

   ;;line number
    `(line-number ((t (:foreground ,builtin))))
    `(line-number-current-line ((t (:foreground, secondary ))))

   ;; font-lock setting
   `(font-lock-comment-delimiter-face ((t (:foreground ,keyword ))))
   `(font-lock-comment-face ((t (:foreground ,secondary ))))
   `(font-lock-string-face ((t (:foreground ,string ))))
   `(font-lock-keyword-face ((t (:foreground ,keyword ))))
   `(font-lock-builtin-face ((t (:foreground ,builtin))))
   `(font-lock-function-name-face ((t (:foreground ,fun-name))))
   `(font-lock-type-face ((t (:foreground ,type))))

   ;;info
   `(success ((t (:foreground, "#66cdaa" :bold t)) ))
   `(warning ((t (:foreground, "#d2691e" ))))
    `(error ((t (:foreground ,high-contrast)) ))

   ) ;; custom-theme-set-faces ends here

  ) ;;let ends here




(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name) )))

(provide-theme 'citrusOcean)

;;; cirtusOcean-theme.el ens here
