;;; humanoid-themes.el --- Color themes with a dark and light variant -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Thomas Friese

;; Author: Thomas Friese
;; URL: https://github.com/humanoid-colors/emacs-humanoid-themes
;;
;; Version: 0.1
;; Keywords: faces, color, theme
;; Package-Requires: ((emacs "24.3"))

;; Initially forked from Spacemacs Theme <https://github.com/nashamri/spacemacs-theme>.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; This color scheme come with two variants - dark and light - and should work well
;; in a 256 color terminal.

;;; Code:

(defgroup humanoid nil
  "humanoid theme options."
  :group 'faces)

(defcustom humanoid-comment-bg nil
  "Use a background for comment lines."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-comment-italic nil
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-keyword-italic nil
  "Enable italics for keywords."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-org-agenda-height nil
  "If non-nil, use varying text heights for agenda items.
Note that if you change this to a non-nil value, you may want to
also adjust the value of `org-agenda-tags-column'. If that is set
to 'auto, tags may not be properly aligned."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-org-bold t
  "Inherit text bold for org headings."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-org-priority-bold t
  "Inherit text bold for priority items in agenda view."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'humanoid)

(defcustom humanoid-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'humanoid)

(defcustom humanoid-underline-parens t
  "If non-nil, underline matching parens when using command `show-paren-mode' or similar."
  :type 'boolean
  :group 'humanoid)

(defun humanoid-true-color ()
  "Ask if true colors."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun humanoid-create (variant theme-name)
  "Define colors with VARIANT and THEME-NAME."
  (let ((class '((class color) (min-colors 89))) ;;                        ~~ Dark ~~                                            ~~ Light ~~
        ;;                                                               GUI       TER                                          GUI       TER
        ;; colors
        (black         (if (eq variant 'dark) (if (humanoid-true-color) "#070708" "gray4")           (if (humanoid-true-color) "#070708" "gray4")))
        (red-light     (if (eq variant 'dark) (if (humanoid-true-color) "#fc466a" "IndianRed1")      (if (humanoid-true-color) "#f11235" "firebrick1")))
        (red           (if (eq variant 'dark) (if (humanoid-true-color) "#f11235" "firebrick1")      (if (humanoid-true-color) "#b0151a" "firebrick3")))
        (red-dark      (if (eq variant 'dark) (if (humanoid-true-color) "#b0151a" "firebrick")       (if (humanoid-true-color) "#8c0303" "firebrick")))
        (red-fg        (if (eq variant 'dark) (if (humanoid-true-color) "#ffe0e1" "MistyRose")       (if (humanoid-true-color) "#461218" "firebrick4")))
        (red-bg-b      (if (eq variant 'dark) (if (humanoid-true-color) "#83212d" "OrangeRed3")      (if (humanoid-true-color) "#ff8a8c" "pink")))
        (red-bg        (if (eq variant 'dark) (if (humanoid-true-color) "#5a171f" "OrangeRed4")      (if (humanoid-true-color) "#ffbdbe" "LightPink")))
        (red-bg-s      (if (eq variant 'dark) (if (humanoid-true-color) "#461218" "DarkRed")         (if (humanoid-true-color) "#ffe0e1" "MistyRose")))
        (green-light   (if (eq variant 'dark) (if (humanoid-true-color) "#3ee766" "green")           (if (humanoid-true-color) "#02d849" "green3")))
        (green         (if (eq variant 'dark) (if (humanoid-true-color) "#02d849" "green2")          (if (humanoid-true-color) "#22a54e" "green4")))
        (green-dark    (if (eq variant 'dark) (if (humanoid-true-color) "#22a54e" "green3")          (if (humanoid-true-color) "#096b38" "DarkOliveGreen")))
        (green-fg      (if (eq variant 'dark) (if (humanoid-true-color) "#e0ffd6" "honeydew2")       (if (humanoid-true-color) "#0d3912" "DarkGreen")))
        (green-bg-b    (if (eq variant 'dark) (if (humanoid-true-color) "#1c7725" "green4")          (if (humanoid-true-color) "#8aca83" "DarkSeaGreen3")))
        (green-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#124e18" "DarkGreen")       (if (humanoid-true-color) "#bde1b9" "DarkSeaGreen1")))
        (green-bg-s    (if (eq variant 'dark) (if (humanoid-true-color) "#0d3912" "DarkOliveGreen")  (if (humanoid-true-color) "#cee9cb" "honeydew2")))
        (yellow-light  (if (eq variant 'dark) (if (humanoid-true-color) "#fad417" "gold")            (if (humanoid-true-color) "#fad417" "gold")))
        (yellow        (if (eq variant 'dark) (if (humanoid-true-color) "#ffb627" "goldenrod1")      (if (humanoid-true-color) "#ffb627" "goldenrod1")))
        (yellow-dark   (if (eq variant 'dark) (if (humanoid-true-color) "#ff9505" "DarkGoldenrod2")  (if (humanoid-true-color) "#ff9505" "DarkGoldenrod2")))
        (yellow-bg     (if (eq variant 'dark) (if (humanoid-true-color) "#422b00" "wheat4")          (if (humanoid-true-color) "#ffeac1" "linen")))
        (orange-light  (if (eq variant 'dark) (if (humanoid-true-color) "#ff8108" "DarkOrange")      (if (humanoid-true-color) "#ff8108" "DarkOrange")))
        (orange        (if (eq variant 'dark) (if (humanoid-true-color) "#fb6107" "DarkOrange2")     (if (humanoid-true-color) "#fb6107" "DarkOrange2")))
        (orange-dark   (if (eq variant 'dark) (if (humanoid-true-color) "#bf5416" "DarkOrange3")     (if (humanoid-true-color) "#bf5416" "DarkOrange3")))
        (blue-light    (if (eq variant 'dark) (if (humanoid-true-color) "#02c6fc" "DeepSkyBlue")     (if (humanoid-true-color) "#00a6fb" "DeepSkyBlue2")))
        (blue          (if (eq variant 'dark) (if (humanoid-true-color) "#00a6fb" "DeepSkyBlue2")    (if (humanoid-true-color) "#0082c9" "DodgerBlue2")))
        (blue-dark     (if (eq variant 'dark) (if (humanoid-true-color) "#0082c9" "DeepSkyBlue3")    (if (humanoid-true-color) "#117396" "DodgerBlue3")))
        (blue-fg       (if (eq variant 'dark) (if (humanoid-true-color) "#b5e2fa" "azure2")          (if (humanoid-true-color) "#0f3e53" "DodgerBlue4")))
        (blue-bg-b     (if (eq variant 'dark) (if (humanoid-true-color) "#1b6e94" "RoyalBlue4")      (if (humanoid-true-color) "#8dc7e6" "LightSkyBlue1")))
        (blue-bg       (if (eq variant 'dark) (if (humanoid-true-color) "#134e69" "RoyalBlue4")      (if (humanoid-true-color) "#b7dcef" "LightSkyBlue1")))
        (blue-bg-s     (if (eq variant 'dark) (if (humanoid-true-color) "#0f3e53" "DodgerBlue4")     (if (humanoid-true-color) "#cce6f4" "azure2")))
        (magenta-light (if (eq variant 'dark) (if (humanoid-true-color) "#fa8cff" "violet")          (if (humanoid-true-color) "#e834f9" "magenta1")))
        (magenta       (if (eq variant 'dark) (if (humanoid-true-color) "#e834f9" "magenta1")        (if (humanoid-true-color) "#ba29eb" "magenta3")))
        (magenta-dark  (if (eq variant 'dark) (if (humanoid-true-color) "#ba29eb" "magenta3")        (if (humanoid-true-color) "#811cac" "magenta4")))
        (purple-light  (if (eq variant 'dark) (if (humanoid-true-color) "#b787ff" "MediumPurple1")   (if (humanoid-true-color) "#7518c4" "purple")))
        (purple        (if (eq variant 'dark) (if (humanoid-true-color) "#a359fe" "MediumPurple3")   (if (humanoid-true-color) "#4d10a5" "purple3")))
        (purple-dark   (if (eq variant 'dark) (if (humanoid-true-color) "#7518c4" "purple3")         (if (humanoid-true-color) "#2f1086" "purple4")))
        (aqua-light    (if (eq variant 'dark) (if (humanoid-true-color) "#60e2e4" "turquoise1")      (if (humanoid-true-color) "#0ed1d1" "cyan3")))
        (aqua          (if (eq variant 'dark) (if (humanoid-true-color) "#0ed1d1" "turquoise2")      (if (humanoid-true-color) "#09b8be" "cyan4")))
        (aqua-dark     (if (eq variant 'dark) (if (humanoid-true-color) "#09b8be" "turquoise3")      (if (humanoid-true-color) "#007784" "DarkSlateGray")))
        (aqua-bg       (if (eq variant 'dark) (if (humanoid-true-color) "#054948" "DarkSlateGrey")   (if (humanoid-true-color) "#c4e5e5" "azure1")))
        (cyan          (if (eq variant 'dark) (if (humanoid-true-color) "#1de9b6" "aquamarine2")     (if (humanoid-true-color) "#00bfa5" "aquamarine3")))
        (brown-light   (if (eq variant 'dark) (if (humanoid-true-color) "#cb8802" "tan1")            (if (humanoid-true-color) "#cb8802" "tan1")))
        (brown         (if (eq variant 'dark) (if (humanoid-true-color) "#b27701" "tan3")            (if (humanoid-true-color) "#b27701" "tan3")))
        (brown-dark    (if (eq variant 'dark) (if (humanoid-true-color) "#7f5501" "tan4")            (if (humanoid-true-color) "#7f5501" "tan4")))
        (brown-fg      (if (eq variant 'dark) (if (humanoid-true-color) "#fceac3" "bisque1")         (if (humanoid-true-color) "#46381d" "chocolate4")))
        (brown-bg-b    (if (eq variant 'dark) (if (humanoid-true-color) "#7c6433" "gray42")          (if (humanoid-true-color) "#d7b87f" "burlywood3")))
        (brown-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#584724" "gray31")          (if (humanoid-true-color) "#e9d8b9" "AntiqueWhite3")))
        (brown-bg-s    (if (eq variant 'dark) (if (humanoid-true-color) "#46381d" "gray23")          (if (humanoid-true-color) "#efe3cc" "AntiqueWhite2")))
        (white         (if (eq variant 'dark) (if (humanoid-true-color) "#fcfcfc" "grey99")          (if (humanoid-true-color) "#fcfcfc" "gray99")))
        (gray-light    (if (eq variant 'dark) (if (humanoid-true-color) "#f4f4ee" "gray90")          (if (humanoid-true-color) "#c0c0bd" "gray64")))
        (gray          (if (eq variant 'dark) (if (humanoid-true-color) "#c0c0bd" "gray64")          (if (humanoid-true-color) "#7a7b75" "gray42")))
        (gray-dark     (if (eq variant 'dark) (if (humanoid-true-color) "#7a7b75" "gray42")          (if (humanoid-true-color) "#2f3337" "gray23")))
        ;; generic
        (act1          (if (eq variant 'dark) (if (humanoid-true-color) "#3b4045" "gray28")          (if (humanoid-true-color) "#e8e8e2" "gray89")))
        (act2          (if (eq variant 'dark) (if (humanoid-true-color) "#484e54" "gray32")          (if (humanoid-true-color) "#deded8" "gray83")))
        (base          (if (eq variant 'dark) (if (humanoid-true-color) "#f8f8f2" "WhiteSmoke")      (if (humanoid-true-color) "#232629" "gray18")))
        (base-dim      (if (eq variant 'dark) (if (humanoid-true-color) "#60615d" "gray42")          (if (humanoid-true-color) "#c0c0bd" "gray64")))
        (bg0           (if (eq variant 'dark) (if (humanoid-true-color) "#151718" "gray12")          (if (humanoid-true-color) "#fcfcfc" "gray99")))
        (bg1           (if (eq variant 'dark) (if (humanoid-true-color) "#232629" "gray18")          (if (humanoid-true-color) "#f8f8f2" "WhiteSmoke")))
        (bg2           (if (eq variant 'dark) (if (humanoid-true-color) "#2f3337" "gray22")          (if (humanoid-true-color) "#f4f4ee" "gray93")))
        (bg3           (if (eq variant 'dark) (if (humanoid-true-color) "#3b4045" "gray28")          (if (humanoid-true-color) "#efefe9" "gray90")))
        (bg4           (if (eq variant 'dark) (if (humanoid-true-color) "#484e54" "gray32")          (if (humanoid-true-color) "#e8e8e2" "gray87")))
        (builtin       (if (eq variant 'dark) (if (humanoid-true-color) "#02c6fc" "DeepSkyBlue")     (if (humanoid-true-color) "#00a6fb" "DeepSkyBlue2")))
        (border        (if (eq variant 'dark) (if (humanoid-true-color) "#31363b" "gray20")          (if (humanoid-true-color) "#deded8" "gray83")))
        (cblk          (if (eq variant 'dark) (if (humanoid-true-color) "#f4f4ee" "gray93")          (if (humanoid-true-color) "#2f3337" "gray22")))
        (cblk-bg       (if (eq variant 'dark) (if (humanoid-true-color) "#151718" "gray12")          (if (humanoid-true-color) "#fcfcf6" "gray98")))
        (cblk-ln       (if (eq variant 'dark) (if (humanoid-true-color) "#5d6658" "cornsilk4")       (if (humanoid-true-color) "#98a890" "cornsilk4")))
        (cblk-ln-bg    (if (eq variant 'dark) (if (humanoid-true-color) "#2f3337" "gray22")          (if (humanoid-true-color) "#efefe9" "gray90")))
        (comment       (if (eq variant 'dark) (if (humanoid-true-color) "#6b7566" "LemonChiffon4")   (if (humanoid-true-color) "#8b9985" "LemonChiffon4")))
        (comment-bg    (if (eq variant 'dark) (if (humanoid-true-color) "#232629" "gray18")          (if (humanoid-true-color) "#f8f8f2" "gray96")))
        (comment-light (if (eq variant 'dark) (if (humanoid-true-color) "#75715e" "LightGoldenrod4") (if (humanoid-true-color) "#6b7566" "LightYellow4")))
        (comp          (if (eq variant 'dark) (if (humanoid-true-color) "#a359fe" "MediumPurple3")   (if (humanoid-true-color) "#4d10a5" "purple3")))
        (const         (if (eq variant 'dark) (if (humanoid-true-color) "#e834f9" "magenta1")        (if (humanoid-true-color) "#ba29eb" "magenta3")))
        (cursor        (if (eq variant 'dark) (if (humanoid-true-color) "#64dd17" "chartreuse2")     (if (humanoid-true-color) "#64dd17" "chartreuse2")))
        (err           (if (eq variant 'dark) (if (humanoid-true-color) "#f11235" "red2")            (if (humanoid-true-color) "#b0151a" "firebrick3")))
        (func          (if (eq variant 'dark) (if (humanoid-true-color) "#b27701" "tan3")            (if (humanoid-true-color) "#b27701" "tan3")))
        (head1         (if (eq variant 'dark) (if (humanoid-true-color) "#42a5f5" "DeepSkyBlue2")    (if (humanoid-true-color) "#2376ad" "DodgerBlue3")))
        (head1-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#293239" "gray17")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (head2         (if (eq variant 'dark) (if (humanoid-true-color) "#42cde8" "DeepSkyBlue1")    (if (humanoid-true-color) "#2595ab" "DeepSkyBlue4")))
        (head2-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#293235" "gray18")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (head3         (if (eq variant 'dark) (if (humanoid-true-color) "#42dbc8" "cyan3")           (if (humanoid-true-color) "#27a89e" "cyan4")))
        (head3-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#293235" "gray19")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (head4         (if (eq variant 'dark) (if (humanoid-true-color) "#42c96a" "MediumSeaGreen")  (if (humanoid-true-color) "#29a466" "MediumSeaGreen")))
        (head4-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#32322c" "gray20")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (head5         (if (eq variant 'dark) (if (humanoid-true-color) "#65bd44" "ForestGreen")     (if (humanoid-true-color) "#519f2a" "ForestGreen")))
        (head5-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#32322c" "gray26")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (head6         (if (eq variant 'dark) (if (humanoid-true-color) "#a5b646" "khaki3")          (if (humanoid-true-color) "#819e2a" "OliveDrab")))
        (head6-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#293235" "gray19")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (head7         (if (eq variant 'dark) (if (humanoid-true-color) "#b1854a" "DarkGoldenrod")   (if (humanoid-true-color) "#9e8b2b" "khaki4")))
        (head7-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#32322c" "gray20")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (head8         (if (eq variant 'dark) (if (humanoid-true-color) "#ad4d4d" "IndianRed3")      (if (humanoid-true-color) "#9e5b2b" "SaddleBrown")))
        (head8-bg      (if (eq variant 'dark) (if (humanoid-true-color) "#32322c" "gray26")          (if (humanoid-true-color) "#efefe9" "gray97")))
        (highlight     (if (eq variant 'dark) (if (humanoid-true-color) "#425051" "gray30")          (if (humanoid-true-color) "#b1bfbb" "gray89")))
        (highlight-dim (if (eq variant 'dark) (if (humanoid-true-color) "#333b3d" "gray36")          (if (humanoid-true-color) "#e9f0e5" "gray85")))
        (keyword       (if (eq variant 'dark) (if (humanoid-true-color) "#00a6fb" "DeepSkyBlue2")    (if (humanoid-true-color) "#0082c9" "DodgerBlue2")))
        (lnum          (if (eq variant 'dark) (if (humanoid-true-color) "#5d6658" "SlateGrey")       (if (humanoid-true-color) "#98a890" "SlateGrey")))
        (mat           (if (eq variant 'dark) (if (humanoid-true-color) "#ced8a2" "gray82")          (if (humanoid-true-color) "#29422d" "DarkSlateGrey")))
        (meta          (if (eq variant 'dark) (if (humanoid-true-color) "#c0c0bd" "gray71")          (if (humanoid-true-color) "#60615d" "gray20")))
        (num           (if (eq variant 'dark) (if (humanoid-true-color) "#ffb627" "goldenrod1")      (if (humanoid-true-color) "#811cac" "magenta4")))
        (str           (if (eq variant 'dark) (if (humanoid-true-color) "#3ee766" "green")           (if (humanoid-true-color) "#096b38" "DarkOliveGreen")))
        (suc           (if (eq variant 'dark) (if (humanoid-true-color) "#02d849" "green2")          (if (humanoid-true-color) "#22a54e" "green4")))
        (ttip          (if (eq variant 'dark) (if (humanoid-true-color) "#75715e" "wheat4")          (if (humanoid-true-color) "#6b7566" "LightYellow4")))
        (ttip-bg       (if (eq variant 'dark) (if (humanoid-true-color) "#484e54" "gray25")          (if (humanoid-true-color) "#e8e8e2" "gray88")))
        (ttip-sl       (if (eq variant 'dark) (if (humanoid-true-color) "#545b62" "gray28")          (if (humanoid-true-color) "#deded8" "gray84")))
        (type          (if (eq variant 'dark) (if (humanoid-true-color) "#09b8be" "turquoise3")      (if (humanoid-true-color) "#007c91" "cyan4")))
        (var           (if (eq variant 'dark) (if (humanoid-true-color) "#0ed1d1" "turquoise2")      (if (humanoid-true-color) "#0d47a1" "DeepSkyBlue4")))
        (war           (if (eq variant 'dark) (if (humanoid-true-color) "#ff9505" "DarkOrange")      (if (humanoid-true-color) "#ff3d00" "OrangeRed"))))

    (cl-loop for (cvar . val) in humanoid-custom-colors
             do (set cvar val))

    (custom-theme-set-faces
     theme-name

;;;;; custom colors
     `(custom-black                        ((,class (:foreground ,black))))
     `(custom-red                          ((,class (:foreground ,red))))
     `(custom-green                        ((,class (:foreground ,green))))
     `(custom-yellow                       ((,class (:foreground ,yellow))))
     `(custom-orange                       ((,class (:foreground ,orange))))
     `(custom-blue                         ((,class (:foreground ,blue))))
     `(custom-magenta                      ((,class (:foreground ,magenta))))
     `(custom-aqua                         ((,class (:foreground ,aqua))))
     `(custom-cyan                         ((,class (:foreground ,cyan))))
     `(custom-white                        ((,class (:foreground ,white))))
     `(custom-gray                         ((,class (:foreground ,base-dim))))

;;;;; basics
     `(cursor                              ((,class (:background ,cursor))))
     `(custom-button                       ((,class (:background ,bg2 :foreground ,base :box (:line-width 2 :style released-button)))))
     `(default                             ((,class (:background ,bg1 :foreground ,base))))
     `(default-italic                      ((,class (:italic t))))
     `(error                               ((,class (:foreground ,err))))
     `(eval-sexp-fu-flash                  ((,class (:background ,suc :foreground ,act1))))
     `(eval-sexp-fu-flash-error            ((,class (:background ,err :foreground ,act1))))
     `(font-lock-builtin-face              ((,class (:foreground ,builtin))))
     `(font-lock-comment-face              ((,class (:foreground ,
                                                     (if humanoid-comment-italic comment-light comment) :background ,
                                                     (when humanoid-comment-bg comment-bg) :slant ,
                                                     (if humanoid-comment-italic 'italic 'normal)))))
     `(font-lock-constant-face             ((,class (:foreground ,const))))
     `(font-lock-doc-face                  ((,class (:foreground ,comment-light))))
     `(font-lock-function-name-face        ((,class (:foreground ,func :bold nil))))
     `(font-lock-keyword-face              ((,class (:foreground ,keyword :bold nil))))
     `(font-lock-negation-char-face        ((,class (:foreground ,const))))
     `(font-lock-preprocessor-face         ((,class (:foreground ,comp))))
     `(font-lock-reference-face            ((,class (:foreground ,const))))
     `(font-lock-regexp-grouping-backslash ((,class (:foreground ,magenta))))
     `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow))))
     `(font-lock-string-face               ((,class (:foreground ,str))))
     `(font-lock-type-face                 ((,class (:foreground ,type))))
     `(font-lock-variable-name-face        ((,class (:foreground ,var))))
     `(font-lock-warning-face              ((,class (:foreground ,war :background ,bg1))))
     `(fringe                              ((,class (:background ,bg1 :foreground ,lnum))))
     `(header-line                         ((,class (:background ,bg2))))
     `(highlight                           ((,class (:foreground ,base :background ,highlight))))
     `(hl-line                             ((,class (:background ,bg3 :extend t))))
     `(hl-todo                             ((,class (:foreground ,magenta))))
     `(isearch                             ((,class (:foreground ,mat :background ,highlight))))
     `(lazy-highlight                      ((,class (:background ,highlight-dim :weight normal))))
     `(link                                ((,class (:foreground unspecified :underline t))))
     `(link-visited                        ((,class (:inherit link))))
     `(match                               ((,class (:background ,highlight :foreground ,mat))))
     `(minibuffer-prompt                   ((,class (:inherit bold :foreground ,keyword))))
     `(mouse                               ((,class (:foreground ,act1 :background ,base :inverse-video t))))
     `(page-break-lines                    ((,class (:foreground ,act2))))
     `(region                              ((,class (:background ,highlight-dim :extend t))))
     `(secondary-selection                 ((,class (:background ,bg3 :extend t))))
     `(shadow                              ((,class (:foreground ,base-dim))))
     `(success                             ((,class (:foreground ,suc :inherit bold))))
     `(tooltip                             ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
     `(vertical-border                     ((,class (:foreground ,border))))
     `(warning                             ((,class (:foreground ,war :imherit bold))))
     `(window-divider                      ((,class (:foreground ,border))))

;;;;; outline
     `(outline-1                     ((,class (:inherit bold :foreground ,head1 :height ,(if humanoid-org-height 1.3 1.0) :background ,(when humanoid-org-highlight head1-bg)))))
     `(outline-2                     ((,class (:inherit bold :foreground ,head2 :height ,(if humanoid-org-height 1.2 1.0) :background ,(when humanoid-org-highlight head2-bg)))))
     `(outline-3                     ((,class (:bold nil :foreground ,head3 :height ,(if humanoid-org-height 1.1 1.0) :background ,(when humanoid-org-highlight head3-bg)))))
     `(outline-4                     ((,class (:bold nil :foreground ,head4 :background ,(when humanoid-org-highlight head4-bg)))))
     `(outline-5                     ((,class (:bold nil :foreground ,head5 :background ,(when humanoid-org-highlight head5-bg)))))
     `(outline-6                     ((,class (:bold nil :foreground ,head6 :background ,(when humanoid-org-highlight head6-bg)))))
     `(outline-7                     ((,class (:bold nil :foreground ,head7 :background ,(when humanoid-org-highlight head7-bg)))))
     `(outline-8                     ((,class (:bold nil :foreground ,head8 :background ,(when humanoid-org-highlight head8-bg)))))

;;;;; ace-window
     `(aw-leading-char-face ((,class (:foreground ,func :weight bold :height 2.0 :box (:line-width 1 :color ,keyword :style released-button)))))

;;;;; ahs
     `(ahs-face                     ((,class (:background ,highlight))))
     `(ahs-plugin-whole-buffer-face ((,class (:background ,mat :foreground ,act1))))

;;;;; all-the-icons
     `(all-the-icons-red       ((,class (:foreground ,red))))
     `(all-the-icons-lred      ((,class (:foreground ,red-light))))
     `(all-the-icons-dred      ((,class (:foreground ,red-dark))))
     `(all-the-icons-green     ((,class (:foreground ,green))))
     `(all-the-icons-lgreen    ((,class (:foreground ,green-light))))
     `(all-the-icons-dgreen    ((,class (:foreground ,green-dark))))
     `(all-the-icons-yellow    ((,class (:foreground ,yellow))))
     `(all-the-icons-lyellow   ((,class (:foreground ,yellow-light))))
     `(all-the-icons-dyellow   ((,class (:foreground ,yellow-dark))))
     `(all-the-icons-blue      ((,class (:foreground ,blue))))
     `(all-the-icons-blue-alt  ((,class (:foreground ,builtin))))
     `(all-the-icons-lblue     ((,class (:foreground ,blue-light))))
     `(all-the-icons-dblue     ((,class (:foreground ,blue-dark))))
     `(all-the-icons-maroon    ((,class (:foreground ,brown))))
     `(all-the-icons-lmaroon   ((,class (:foreground ,brown-light))))
     `(all-the-icons-dmaroon   ((,class (:foreground ,brown-dark))))
     `(all-the-icons-orange    ((,class (:foreground ,orange))))
     `(all-the-icons-lorange   ((,class (:foreground ,orange-light))))
     `(all-the-icons-dorange   ((,class (:foreground ,orange-dark))))
     `(all-the-icons-purple    ((,class (:foreground ,purple))))
     `(all-the-icons-lpurple   ((,class (:foreground ,purple-light))))
     `(all-the-icons-dpurple   ((,class (:foreground ,purple-dark))))
     `(all-the-icons-cyan      ((,class (:foreground ,aqua))))
     `(all-the-icons-cyan-alt  ((,class (:foreground ,cyan))))
     `(all-the-icons-lcyan     ((,class (:foreground ,aqua-light))))
     `(all-the-icons-dcyan     ((,class (:foreground ,aqua-dark))))
     `(all-the-icons-pink      ((,class (:foreground ,magenta))))
     `(all-the-icons-lpink     ((,class (:foreground ,magenta-light))))
     `(all-the-icons-dpink     ((,class (:foreground ,magenta-dark))))
     `(all-the-icons-silver    ((,class (:foreground ,gray))))
     `(all-the-icons-lsilver   ((,class (:foreground ,gray-light))))
     `(all-the-icons-dsilver   ((,class (:foreground ,gray-dark))))

;;;;; anzu-mode
     `(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

;;;;; auto-complete
     `(ac-completion-face ((,class (:background ,ttip-bg :foreground ,ttip))))

;;;;; avy
     `(avy-lead-face   ((,class (:background ,green-bg :foreground ,green))))
     `(avy-lead-face-0 ((,class (:background ,green-bg :foreground ,yellow))))
     `(avy-lead-face-1 ((,class (:background ,green-bg :foreground ,magenta))))
     `(avy-lead-face-2 ((,class (:background ,green-bg :foreground ,blue))))

;;;;; c
     `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

;;;;; calendar
     `(holiday ((,class (:background ,war))))

;;;;; calfw
     `(cfw:face-title               ((,class (:foreground ,head1 :height 2.0 :weight bold :inherit variable-pitch))))
     `(cfw:face-header              ((,class (:foreground ,base :weight bold))))
     `(cfw:face-saturday            ((,class (:foreground ,base :weight bold))))
     `(cfw:face-sunday              ((,class (:foreground ,base :weight bold))))
     `(cfw:face-holiday             ((,class (:foreground ,head1 :weight bold))))
     `(cfw:face-grid                ((,class (:foreground ,border))))
     `(cfw:face-default-content     ((,class (:foreground ,green))))
     `(cfw:face-periods             ((,class (:foreground ,cyan))))
     `(cfw:face-day-title           ((,class (:background ,blue-bg))))
     `(cfw:face-default-day         ((,class (:foreground ,base :weight bold))))
     `(cfw:face-annotation          ((,class (:foreground ,aqua))))
     `(cfw:face-disable             ((,class (:foreground ,base-dim))))
     `(cfw:face-today-title         ((,class (:background ,blue :weight bold))))
     `(cfw:face-today               ((,class (:background ,blue-bg :weight bold))))
     `(cfw:face-select              ((,class (:background ,magenta :weight bold))))
     `(cfw:face-toolbar             ((,class (:foreground ,base :background ,bg1))))
     `(cfw:face-toolbar-button-off  ((,class (:foreground ,base :weight bold))))
     `(cfw:face-toolbar-button-on   ((,class (:foreground ,base :weight bold))))

;;;;; centaur-tabs
     `(centaur-tabs-default                    ((,class (:background ,bg1 :foreground ,base-dim))))
     `(centaur-tabs-selected                   ((,class (:background ,bg1 :foreground ,base :weight normal))))
     `(centaur-tabs-unselected                 ((,class (:background ,bg2 :foreground ,comment-light :weight light))))
     `(centaur-tabs-selected-modified          ((,class (:background ,bg1 :foreground ,war :weight normal))))
     `(centaur-tabs-unselected-modified        ((,class (:background ,bg2 :weight light :foreground ,war))))
     `(centaur-tabs-active-bar-face            ((,class (:background ,blue))))
     `(centaur-tabs-modified-marker-selected   ((,class (:inherit centaur-tabs-selected :foreground,keyword))))
     `(centaur-tabs-modified-marker-unselected ((,class (:inherit centaur-tabs-unselected :foreground,keyword))))

;;;;; cider
     `(cider-enlightened         ((,class      (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
     `(cider-enlightened-local   ((,class      (:foreground ,yellow))))
     `(cider-instrumented-face   ((,class      (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
     `(cider-result-overlay-face ((,class      (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
     `(cider-test-error-face     ((,class      (:background ,war :foreground ,act1))))
     `(cider-test-failure-face   ((,class      (:background ,err :foreground ,act1))))
     `(cider-test-success-face   ((,class      (:background ,suc :foreground ,act1))))
     `(cider-traced-face         ((,class      (:box (:color ,cyan :line-width -1 :style nil)))))

;;;;; company
     `(company-echo-common              ((,class (:background ,base :foreground ,act1))))
     `(company-preview                  ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-preview-common           ((,class (:background ,ttip-bg :foreground ,base))))
     `(company-preview-search           ((,class (:inherit match))))
     `(company-scrollbar-bg             ((,class (:background ,bg2))))
     `(company-scrollbar-fg             ((,class (:background ,act2))))
     `(company-template-field           ((,class (:inherit region))))
     `(company-tooltip                  ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-tooltip-annotation       ((,class (:foreground ,type))))
     `(company-tooltip-common           ((,class (:background ,ttip-bg :foreground ,keyword))))
     `(company-tooltip-common-selection ((,class (:foreground ,base))))
     `(company-tooltip-mouse            ((,class (:inherit highlight))))
     `(company-tooltip-search           ((,class (:inherit match))))
     `(company-tooltip-selection        ((,class (:background ,ttip-sl :foreground ,base))))

;;;;; diff
     `(diff-added             ((,class (:background ,green-bg-s :foreground ,green-fg :extend t))))
     `(diff-changed           ((,class (:background ,blue-bg-s  :foreground ,blue-fg  :extend t))))
     `(diff-header            ((,class (:background ,cblk-ln-bg :foreground ,func     :extend t))))
     `(diff-file-header       ((,class (:background ,cblk-ln-bg :foreground ,cblk     :extend t))))
     `(diff-indicator-added   ((,class (:background nil         :foreground ,green    :extend t))))
     `(diff-indicator-changed ((,class (:background nil         :foreground ,blue))))
     `(diff-indicator-removed ((,class (:background nil         :foreground ,red))))
     `(diff-refine-added      ((,class (:background ,green-bg-b :foreground ,green-fg))))
     `(diff-refine-changed    ((,class (:background ,blue-bg-b  :foreground ,blue-fg))))
     `(diff-refine-removed    ((,class (:background ,red-bg-b   :foreground ,red-fg))))
     `(diff-removed           ((,class (:background ,red-bg-s   :foreground ,red-fg   :extend t))))

;;;;; diff-hl
     `(diff-hl-change ((,class (:background ,blue-bg  :foreground ,blue-fg))))
     `(diff-hl-delete ((,class (:background ,red-bg   :foreground ,red-fg))))
     `(diff-hl-insert ((,class (:background ,green-bg :foreground ,green-fg))))

;;;;; dired
     `(dired-directory  ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(dired-flagged    ((,class (:foreground ,red))))
     `(dired-header     ((,class (:foreground ,comp :inherit bold))))
     `(dired-ignored    ((,class (:inherit shadow))))
     `(dired-mark       ((,class (:foreground ,comp :inherit bold))))
     `(dired-marked     ((,class (:foreground ,magenta :inherit bold))))
     `(dired-perm-write ((,class (:foreground ,base :underline t))))
     `(dired-symlink    ((,class (:foreground ,var :background ,bg1 :inherit bold))))
     `(dired-warning    ((,class (:foreground ,war))))

;;;;; doom-modeline
     `(doom-modeline-bar                ((,class (:background ,bg2))))
     `(doom-modeline-bar-inactive       ((,class (:background ,comment))))
     `(doom-modeline-buffer-major-mode  ((,class (:inherit font-lock-type-face))))
     `(doom-modeline-buffer-modified    ((,class (:inherit warning))))
     `(doom-modeline-buffer-path        ((,class (:inherit font-lock-keyword-face))))
     `(doom-modeline-evil-emacs-state   ((,class (:foreground ,blue))))
     `(doom-modeline-evil-insert-state  ((,class (:foreground ,green))))
     `(doom-modeline-evil-motion-state  ((,class (:foreground ,purple))))
     `(doom-modeline-evil-normal-state  ((,class (:foreground ,yellow))))
     `(doom-modeline-evil-replace-state ((,class (:foreground ,orange))))
     `(doom-modeline-evil-visual-state  ((,class (:foreground ,base-dim))))
     `(doom-modeline-info               ((,class (:inherit success))))
     `(doom-modeline-project-dir        ((,class (:inherit font-lock-keyword-face))))
     `(doom-modeline-warning            ((,class (:inherit warning))))

;;;;; ediff
     `(ediff-current-diff-A        ((,class (:background ,red-bg-s   :extend t))))
     `(ediff-current-diff-Ancestor ((,class (:background ,blue-bg-s  :extend t))))
     `(ediff-current-diff-B        ((,class (:background ,green-bg-s :extend t))))
     `(ediff-current-diff-C        ((,class (:background ,brown-bg-s :extend t))))
     `(ediff-even-diff-A           ((,class (:background ,bg4        :extend t))))
     `(ediff-even-diff-Ancestor    ((,class (:background ,bg3        :extend t))))
     `(ediff-even-diff-B           ((,class (:background ,bg3        :extend t))))
     `(ediff-even-diff-C           ((,class (:background ,bg4        :extend t))))
     `(ediff-fine-diff-A           ((,class (:background ,red-bg     :extend t))))
     `(ediff-fine-diff-Ancestor    ((,class (:background ,blue-bg    :extend t))))
     `(ediff-fine-diff-B           ((,class (:background ,green-bg   :extend t))))
     `(ediff-fine-diff-C           ((,class (:background ,brown-bg   :extend t))))
     `(ediff-odd-diff-A            ((,class (:background ,bg3        :extend t))))
     `(ediff-odd-diff-Ancestor     ((,class (:background ,bg2        :extend t))))
     `(ediff-odd-diff-B            ((,class (:background ,bg4        :extend t))))
     `(ediff-odd-diff-C            ((,class (:background ,bg3        :extend t))))

;;;;; ein
     `(ein:cell-input-area           ((,class (:background ,bg2))))
     `(ein:cell-input-prompt         ((,class (:foreground ,suc))))
     `(ein:cell-output-prompt        ((,class (:foreground ,err))))
     `(ein:notification-tab-normal   ((,class (:foreground ,keyword))))
     `(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

;;;;; eldoc
     `(eldoc-highlight-function-argument ((,class (:foreground ,mat :inherit bold))))

;;;;; elfeed
     `(elfeed-search-date-face         ((,class (:foreground ,head2))))
     `(elfeed-search-feed-face         ((,class (:foreground ,blue))))
     `(elfeed-search-tag-face          ((,class (:foreground ,func))))
     `(elfeed-search-title-face        ((,class (:foreground ,var))))
     `(elfeed-search-unread-title-face ((,class (:foreground ,base))))

;;;;; enh-ruby
     `(enh-ruby-op-face               ((,class (:background ,bg1 :foreground ,base))))
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))

;;;;; erc
     `(erc-input-face        ((,class (:foreground ,func))))
     `(erc-my-nick-face      ((,class (:foreground ,keyword))))
     `(erc-nick-default-face ((,class (:foreground ,keyword))))
     `(erc-nick-prefix-face  ((,class (:foreground ,yellow))))
     `(erc-notice-face       ((,class (:foreground ,str))))
     `(erc-prompt-face       ((,class (:foreground ,mat :inherit bold))))
     `(erc-timestamp-face    ((,class (:foreground ,keyword))))

;;;;; eshell
     `(eshell-ls-archive    ((,class (:foreground ,red :inherit bold))))
     `(eshell-ls-backup     ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter    ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory  ((,class (:foreground ,keyword :inherit bold))))
     `(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
     `(eshell-ls-missing    ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product    ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special    ((,class (:foreground ,yellow :inherit bold))))
     `(eshell-ls-symlink    ((,class (:foreground ,var :inherit bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,base))))
     `(eshell-prompt        ((,class (:foreground ,keyword :inherit bold))))


;;;;; ESS
     `(ess-assignment-face             ((,class (:foreground ,type :inherit bold))))
     `(ess-backquoted-face             ((,class (:foreground ,var))))
     `(ess-constant-face               ((,class (:inherit font-lock-constant-face))))
     `(ess-f-t-face                    ((,class (:inherit font-lock-constant-face))))
     `(ess-function-call-face          ((,class (:foreground ,func))))
     `(ess-keyword-face                ((,class (:inherit font-lock-keyword-face))))
     `(ess-matrix-face                 ((,class (:foreground ,base-dim))))
     `(ess-modifiers-face              ((,class (:foreground ,keyword))))
     `(ess-numbers-face                ((,class (:inherit font-lock-constant-face))))
     `(ess-operator-face               ((,class (:foreground ,var))))
     `(ess-paren-face                  ((,class (:foreground ,blue))))
     `(ess-r-control-flow-keyword-face ((,class (:foreground ,keyword))))
     `(ess-r-signal-keyword-face       ((,class (:foreground ,war))))

;;;;; evil
     `(evil-ex-lazy-highlight         ((,class (:background ,comment :foreground ,comment-bg))))
     `(evil-ex-substitute-matches     ((,class (:background ,red-bg :foreground ,red))))
     `(evil-ex-substitute-replacement ((,class (:background ,green-bg :foreground ,green))))

;;;;; evil-goggles
     `(evil-goggles--pulse-face                ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(evil-goggles-change-face                ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-commentary-face            ((,class (:background ,aqua-bg :foreground ,aqua))))
     `(evil-goggles-delete-face                ((,class (:background ,red-bg-s :foreground ,red))))
     `(evil-goggles-fill-and-move-face         ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-indent-face                ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-join-face                  ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-nerd-commenter-face        ((,class (:background ,aqua-bg :foreground ,aqua))))
     `(evil-goggles-paste-face                 ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-record-macro-face          ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-replace-with-register-face ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(evil-goggles-set-marker-face            ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-shift-face                 ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-surround-face              ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-yank-face                  ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-undo-redo-add-face         ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-undo-redo-change-face      ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-undo-redo-remove-face      ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; evil-mc
     `(evil-mc-cursor-bar-face     ((,class (:foreground ,aqua))))
     `(evil-mc-cursor-default-face ((,class (:background ,aqua :foreground ,act2))))
     `(evil-mc-cursor-hbar-face    ((,class (:foreground ,aqua))))
     `(evil-mc-region-face         ((,class (:inherit highlight))))

;;;;; flycheck
     `(flycheck-error
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,err)))
        (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
     `(flycheck-fringe-error            ((,class (:foreground ,err :inherit bold))))
     `(flycheck-fringe-info             ((,class (:foreground ,keyword :inherit bold))))
     `(flycheck-fringe-warning          ((,class (:foreground ,war :inherit bold))))
     `(flycheck-info
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,comment)))
        (,class (:foreground ,base :background ,comment :underline t))))
     `(flycheck-warning
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,comment-light)))
        (,class (:foreground ,base :background ,comment-light :underline t))))

;;;;; flymake
     `(flymake-error ((,(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color ,err)))
                      (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     `(flymake-note ((,(append '((supports :underline (:style line))) class)
                      (:underline (:style wave :color ,keyword)))
                     (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     `(flymake-warning ((,(append '((supports :underline (:style line))) class)
                         (:underline (:style line :color ,war)))
                        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; flyspell
     `(flyspell-incorrect ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,base-dim)))
                           (,class (:foreground ,base :background ,base-dim :inherit bold :underline t))))
     `(flyspell-duplicate ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,comment)))
                           (,class (:foreground ,base :background ,comment :inherit bold :underline t))))

;;;;; jabber
     `(jabber-activity-face          ((,class (:inherit bold :foreground ,red))))
     `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-error             ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-foreign    ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-local      ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-prompt-system     ((,class (:inherit bold :foreground ,green))))
     `(jabber-chat-text-foreign      ((,class (:foreground ,base))))
     `(jabber-chat-text-local        ((,class (:foreground ,base))))
     `(jabber-rare-time-face         ((,class (:foreground ,green))))
     `(jabber-roster-user-away       ((,class (:foreground ,yellow))))
     `(jabber-roster-user-chatty     ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-dnd        ((,class (:foreground ,red))))
     `(jabber-roster-user-error      ((,class (:foreground ,err))))
     `(jabber-roster-user-offline    ((,class (:foreground ,base))))
     `(jabber-roster-user-online     ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-xa         ((,class (:foreground ,aqua))))

;;;;; git
     `(git-commit-summary ((,class (:foreground ,base :inherit bold))))

;;;;; git-gutter
     `(git-gutter:added        ((,class (:foreground ,green :inherit bold))))
     `(git-gutter:deleted      ((,class (:foreground ,red :inherit bold))))
     `(git-gutter:modified     ((,class (:foreground ,purple :inherit bold))))
     `(git-gutter:separator    ((,class (:foreground ,aqua :inherit bold))))
     `(git-gutter:unchanged    ((,class (:background ,yellow))))
     `(git-gutter+:added       ((,class (:inherit git-gutter:added))))
     `(git-gutter+:deleted     ((,class (:inherit git-gutter:deleted))))
     `(git-gutter+:modified    ((,class (:inherit git-gutter:modified))))
     `(git-gutter+:separator   ((,class (:inherit git-gutter:separator))))
     `(git-gutter+:unchanged   ((,class (:inherit git-gutter:unchanged))))
     `(git-gutter-fr:added     ((,class (:inherit git-gutter:added))))
     `(git-gutter-fr:deleted   ((,class (:inherit git-gutter:deleted))))
     `(git-gutter-fr:modified  ((,class (:inherit git-gutter:modified))))
     `(git-gutter-fr+:added    ((,class (:inherit git-gutter:added))))
     `(git-gutter-fr+:deleted  ((,class (:inherit git-gutter:deleted))))
     `(git-gutter-fr+:modified ((,class (:inherit git-gutter:modified))))

;;;;; git-timemachine
     `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; gnus
     `(gnus-cite-1                   ((,class (:foreground ,green :background ,bg2))))
     `(gnus-cite-2                   ((,class (:foreground ,blue :background ,bg2))))
     `(gnus-cite-3                   ((,class (:foreground ,magenta :background ,bg2))))
     `(gnus-cite-4                   ((,class (:foreground ,yellow :background ,bg2))))
     `(gnus-cite-5                   ((,class (:foreground ,cyan :background ,bg2))))
     `(gnus-cite-6                   ((,class (:foreground ,orange :background ,bg2))))
     `(gnus-cite-7                   ((,class (:foreground ,aqua :background ,bg2))))
     `(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,act1))))
     `(gnus-header-content           ((,class (:foreground ,str))))
     `(gnus-header-from              ((,class (:foreground ,var))))
     `(gnus-header-name              ((,class (:foreground ,comp))))
     `(gnus-header-subject           ((,class (:foreground ,base :inherit bold))))
     `(gnus-summary-cancelled        ((,class (:background ,war :foreground ,act1))))
     `(message-header-content        ((,class (:foreground ,str))))
     `(message-header-from           ((,class (:foreground ,var))))
     `(message-header-to             ((,class (:foreground ,var))))
     `(message-header-name           ((,class (:foreground ,comp))))
     `(message-header-subject        ((,class (:foreground ,base :inherit bold))))
     `(message-summary-cancelled     ((,class (:background ,war :foreground ,act1))))
     `(message-cited-text            ((,class (:foreground ,green :background ,bg2))))

;;;;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,base))))
     `(guide-key/key-face               ((,class (:foreground ,keyword))))
     `(guide-key/prefix-command-face    ((,class (:foreground ,keyword :inherit bold))))

;;;;; helm
     `(helm-bookmark-directory          ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file               ((,class (:foreground ,base))))
     `(helm-bookmark-gnus               ((,class (:foreground ,comp))))
     `(helm-bookmark-info               ((,class (:foreground ,comp))))
     `(helm-bookmark-man                ((,class (:foreground ,comp))))
     `(helm-bookmark-w3m                ((,class (:foreground ,comp))))
     `(helm-buffer-directory            ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-file                 ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-not-saved            ((,class (:foreground ,comp :background ,bg1))))
     `(helm-buffer-process              ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-buffer-saved-out            ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-size                 ((,class (:foreground ,base :background ,bg1))))
     `(helm-candidate-number            ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
     `(helm-ff-directory                ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(helm-ff-dotted-directory         ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(helm-ff-dotted-symlink-directory ((,class (:foreground ,var :background ,bg1 :inherit bold))))
     `(helm-ff-executable               ((,class (:foreground ,suc :background ,bg1 :weight normal))))
     `(helm-ff-file                     ((,class (:foreground ,base :background ,bg1 :weight normal))))
     `(helm-ff-invalid-symlink          ((,class (:foreground ,err :background ,bg1 :inherit bold))))
     `(helm-ff-prefix                   ((,class (:foreground ,act1 :background ,keyword :weight normal))))
     `(helm-ff-symlink                  ((,class (:foreground ,var :background ,bg1 :inherit bold))))
     `(helm-grep-cmd-line               ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-file                   ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-finish                 ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-lineno                 ((,class (:foreground ,type :background ,bg1 :inherit bold))))
     `(helm-grep-match                  ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-header                      ((,class (:foreground ,base :background ,bg1 :underline nil :box nil))))
     `(helm-header-line-left-margin     ((,class (:foreground ,comp :background ,nil))))
     `(helm-match                       ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-match-item                  ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-moccur-buffer               ((,class (:foreground ,var :background ,bg1))))
     `(helm-selection                   ((,class (:background ,highlight))))
     `(helm-selection-line              ((,class (:background ,bg3))))
     `(helm-separator                   ((,class (:foreground ,comp :background ,bg1))))
     `(helm-source-header               ((,class (:background ,cblk-ln-bg :foreground ,cblk :inherit bold))))
     `(helm-time-zone-current           ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-time-zone-home              ((,class (:foreground ,comp :background ,bg1))))
     `(helm-visible-mark                ((,class (:foreground ,func :background ,bg3))))

;;;;; helm-swoop
     `(helm-swoop-target-line-block-face ((,class (:foreground ,base :background ,highlight))))
     `(helm-swoop-target-line-face       ((,class (:background ,highlight))))
     `(helm-swoop-target-word-face       ((,class (:background ,highlight :foreground ,mat))))

;;;;; highlights
     `(hi-green                   ((,class (:foreground ,green :background ,green-bg))))
     `(hi-yellow                  ((,class (:foreground ,yellow :background ,yellow-bg))))
     `(highlight-indentation-face ((,class (:background ,comment-bg))))
     `(highlight-numbers-number   ((,class (:foreground ,num))))
     `(highlight-symbol-face      ((,class (:background ,bg2))))

;;;;; hydra
     `(hydra-face-blue ((,class (:foreground ,blue))))
     `(hydra-face-red  ((,class (:foreground ,red))))

;;;;; ido
     `(ido-first-match         ((,class (:foreground ,comp :inherit bold))))
     `(ido-only-match          ((,class (:foreground ,mat :inherit bold))))
     `(ido-subdir              ((,class (:foreground ,keyword))))
     `(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; info
     `(info-header-xref    ((,class (:foreground ,func :underline t))))
     `(info-menu           ((,class (:foreground ,suc))))
     `(info-node           ((,class (:foreground ,func :inherit bold))))
     `(info-quoted-name    ((,class (:foreground ,keyword))))
     `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
     `(info-string         ((,class (:foreground ,str))))
     `(info-title-1        ((,class (:height 1.4 :inherit bold))))
     `(info-title-2        ((,class (:height 1.3 :inherit bold))))
     `(info-title-3        ((,class (:height 1.3))))
     `(info-title-4        ((,class (:height 1.2))))

;;;;; ivy
     `(ivy-current-match           ((,class (:background ,highlight :inherit bold))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
     `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
     `(ivy-remote                  ((,class (:foreground ,cyan))))

;;;;; ivy-posframe
     `(ivy-posframe ((,class (:background ,bg3))))

;;;;; latex
     `(font-latex-bold-face                ((,class (:foreground ,comp))))
     `(font-latex-italic-face              ((,class (:foreground ,keyword :italic t))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords  ((,class (:foreground ,var))))
     `(font-latex-sectioning-0-face        ((,class (:inherit bold :foreground ,head3 :height ,(if humanoid-org-height 1.3 1.0) :background ,(when humanoid-org-highlight head3-bg)))))
     `(font-latex-sectioning-1-face        ((,class (:inherit bold :foreground ,head4 :height ,(if humanoid-org-height 1.3 1.0) :background ,(when humanoid-org-highlight head4-bg)))))
     `(font-latex-sectioning-2-face        ((,class (:inherit bold :foreground ,head1 :height ,(if humanoid-org-height 1.3 1.0) :background ,(when humanoid-org-highlight head1-bg)))))
     `(font-latex-sectioning-3-face        ((,class (:inherit bold :foreground ,head2 :height ,(if humanoid-org-height 1.2 1.0) :background ,(when humanoid-org-highlight head2-bg)))))
     `(font-latex-sectioning-4-face        ((,class (:bold nil :foreground ,head3 :height ,(if humanoid-org-height 1.1 1.0) :background ,(when humanoid-org-highlight head3-bg)))))
     `(font-latex-sectioning-5-face        ((,class (:bold nil :foreground ,head4 :background ,(when humanoid-org-highlight head4-bg)))))
     `(font-latex-string-face              ((,class (:foreground ,str))))
     `(font-latex-warning-face             ((,class (:foreground ,war))))

;;;;; ledger-mode
     `(ledger-font-directive-face      ((,class (:foreground ,meta))))
     `(ledger-font-posting-amount-face ((,class (:foreground ,yellow))))
     `(ledger-font-posting-date-face   ((,class (:foreground ,head1))))
     `(ledger-occur-xact-face          ((,class (:background ,bg2))))

;;;;; linum-mode
     `(linum ((,class (:foreground ,lnum :background ,bg2 :inherit default))))

;;;;; line-numbers
     `(line-number              ((,class (:foreground ,lnum :background ,bg2 :inherit default))))
     `(line-number-current-line ((,class (:foreground ,comment :background ,bg0 :inherit line-number))))

;;;;; linum-relative
     `(linum-relative-current-face ((,class (:foreground ,base))))

;;;;; magit
     `(magit-bisect-bad                  ((,class (:foreground ,war))))
     `(magit-bisect-good                 ((,class (:foreground ,suc))))
     `(magit-bisect-skip                 ((,class (:foreground ,yellow))))
     `(magit-blame-culprit               ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(magit-blame-date                  ((,class (:background ,yellow-bg :foreground ,green))))
     `(magit-blame-hash                  ((,class (:background ,yellow-bg :foreground ,func))))
     `(magit-blame-heading               ((,class (:background ,yellow-bg :foreground ,green :extend t))))
     `(magit-blame-highlight             ((,class (:background ,highlight-dim))))
     `(magit-blame-name                  ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(magit-blame-sha1                  ((,class (:background ,yellow-bg :foreground ,func))))
     `(magit-blame-subject               ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(magit-blame-summary               ((,class (:background ,yellow-bg :foreground ,yellow :extend t))))
     `(magit-blame-time                  ((,class (:background ,yellow-bg :foreground ,green))))
     `(magit-branch                      ((,class (:foreground ,const :inherit bold))))
     `(magit-branch-local                ((,class (:background ,blue-bg :foreground ,blue :inherit bold))))
     `(magit-branch-remote               ((,class (:background ,aqua-bg :foreground ,aqua :inherit bold))))
     `(magit-cherry-equivalent           ((,class (:foreground ,magenta))))
     `(magit-cherry-unmatched            ((,class (:foreground ,cyan))))
     `(magit-diff-added                  ((,class (:background ,green-bg-s :foreground ,green-fg :extend t))))
     `(magit-diff-added-highlight        ((,class (:background ,green-bg   :foreground ,green-fg :extend t))))
     `(magit-diff-base                   ((,class (:background ,brown-bg-s :foreground ,brown-fg :extend t))))
     `(magit-diff-base-highlight         ((,class (:background ,brown-bg   :foreground ,brown-fg :extend t))))
     `(magit-diff-removed                ((,class (:background ,red-bg-s   :foreground ,red-fg   :extend t))))
     `(magit-diff-removed-highlight      ((,class (:background ,red-bg     :foreground ,red-fg   :extend t))))
     `(magit-diff-context                ((,class (:background ,bg1 :foreground ,base :extend t))))
     `(magit-diff-context-highlight      ((,class (:background ,bg0 :foreground ,base))))
     `(magit-diff-file-heading-selection ((,class (:inherit magit-diff-file-heading-highlight :foreground ,meta :extend t))))
     `(magit-diff-hunk-heading           ((,class (:background ,ttip-bg :foreground ,cblk :extend t))))
     `(magit-diff-hunk-heading-highlight ((,class (:background ,ttip-sl :foreground ,base :extend t))))
     `(magit-diff-hunk-heading-selection ((,class (:inherit magit-diff-hunk-heading-highlight :foreground ,meta :extend t))))
     `(magit-diff-hunk-region            ((,class (:inherit bold :extend t))))
     `(magit-diff-lines-heading          ((,class (:inherit magit-diff-hunk-heading-highlight :foreground ,meta :extend t))))
     `(magit-diffstat-added              ((,class (:foreground ,green))))
     `(magit-diffstat-removed            ((,class (:foreground ,red))))
     `(magit-dimmed                      ((,class (:foreground ,base-dim))))
     `(magit-hash                        ((,class (:foreground ,var))))
     `(magit-hunk-heading                ((,class (:background ,bg3 :extend t))))
     `(magit-hunk-heading-highlight      ((,class (:background ,bg3 :extend t))))
     `(magit-item-highlight              ((,class (:background ,bg2))))
     `(magit-log-author                  ((,class (:foreground ,func))))
     `(magit-log-head-label-head         ((,class (:background ,yellow :foreground ,act1 :inherit bold))))
     `(magit-log-head-label-local        ((,class (:background ,keyword :foreground ,act1 :inherit bold))))
     `(magit-log-head-label-remote       ((,class (:background ,suc :foreground ,act1 :inherit bold))))
     `(magit-log-head-label-tags         ((,class (:background ,magenta :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-wip          ((,class (:background ,cyan :foreground ,act1 :inherit bold))))
     `(magit-log-sha1                    ((,class (:foreground ,str))))
     `(magit-process-ng                  ((,class (:foreground ,war :inherit bold))))
     `(magit-process-ok                  ((,class (:foreground ,func :inherit bold))))
     `(magit-reflog-amend                ((,class (:foreground ,magenta))))
     `(magit-reflog-checkout             ((,class (:foreground ,blue))))
     `(magit-reflog-cherry-pick          ((,class (:foreground ,green))))
     `(magit-reflog-commit               ((,class (:foreground ,green))))
     `(magit-reflog-merge                ((,class (:foreground ,green))))
     `(magit-reflog-other                ((,class (:foreground ,cyan))))
     `(magit-reflog-rebase               ((,class (:foreground ,magenta))))
     `(magit-reflog-remote               ((,class (:foreground ,cyan))))
     `(magit-reflog-reset                ((,class (:foreground ,red))))
     `(magit-section-heading             ((,class (:foreground ,keyword :inherit bold :extend t))))
     `(magit-section-heading-selection   ((,class (:inherit magit-section-heading :foreground ,meta))))
     `(magit-section-highlight           ((,class (:background ,bg2 :extend t))))
     `(magit-section-title               ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
     `(magit-sequence-drop               ((,class (:foreground ,war))))
     `(magit-sequence-head               ((,class (:foreground ,keyword))))
     `(magit-sequence-part               ((,class (:foreground ,type))))
     `(magit-sequence-stop               ((,class (:foreground ,suc))))
     `(magit-signature-bad               ((,class (:foreground ,war))))
     `(magit-signature-error             ((,class (:foreground ,err))))
     `(magit-signature-expired           ((,class (:foreground ,yellow))))
     `(magit-signature-expired-key       ((,class (:foreground ,yellow))))
     `(magit-signature-good              ((,class (:foreground ,suc))))
     `(magit-signature-revoked           ((,class (:foreground ,const))))
     `(magit-signature-untrusted         ((,class (:foreground ,cyan))))
     `(magit-tag                         ((,class (:foreground ,meta))))

;;;;; man
     `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
     `(Man-reverse    ((,class (:foreground ,highlight))))
     `(Man-underline  ((,class (:foreground ,comp :underline t))))

;;;;; markdown
     `(markdown-header-face-1   ((,class (:inherit outline-1))))
     `(markdown-header-face-2   ((,class (:inherit outline-2))))
     `(markdown-header-face-3   ((,class (:inherit outline-3))))
     `(markdown-header-face-4   ((,class (:inherit outline-4))))
     `(markdown-header-face-5   ((,class (:inherit outline-5))))
     `(markdown-header-face-6   ((,class (:inherit outline-6))))
     `(markdown-table-face      ((,class (:inherit org-table))))
     `(mmm-default-submode-face ((,class (:background ,bg2))))

;;;;; mode-line
     `(mode-line           ((,class (:foreground ,base :background ,bg2 :box nil :underline nil))))
     `(mode-line-inactive  ((,class (:foreground ,base-dim :background ,bg2  :box nil :underline nil))))
     `(mode-line-buffer-id ((,class (:foreground ,base :weight normal))))
     `(mode-line-highlight ((,class (:foreground ,base :background ,bg3 :box nil :underline nil))))

;;;;; mu4e
     `(mu4e-attach-number-face        ((,class (:foreground ,var))))
     `(mu4e-cited-1-face              ((,class (:foreground ,green :background ,bg2))))
     `(mu4e-cited-2-face              ((,class (:foreground ,blue :background ,bg2))))
     `(mu4e-cited-3-face              ((,class (:foreground ,magenta :background ,bg2))))
     `(mu4e-cited-4-face              ((,class (:foreground ,cyan :background ,bg2))))
     `(mu4e-cited-5-face              ((,class (:foreground ,orange :background ,bg2))))
     `(mu4e-cited-6-face              ((,class (:foreground ,aqua :background ,bg2))))
     `(mu4e-cited-7-face              ((,class (:foreground ,red :background ,bg2))))
     `(mu4e-contact-face              ((,class (:foreground ,str))))
     `(mu4e-draft-face                ((,class (:foreground ,comp))))
     `(mu4e-flagged-face              ((,class (:foreground ,keyword :inherit bold))))
     `(mu4e-footer-face               ((,class (:inherit (font-lock-comment-face fixed-pitch)))))
     `(mu4e-forwarded-face            ((,class (:foreground ,const))))
     `(mu4e-header-highlight-face     ((,class (:background ,act2))))
     `(mu4e-header-key-face           ((,class (:foreground ,keyword :inherit bold))))
     `(mu4e-header-marks-face         ((,class (:foreground ,var))))
     `(mu4e-header-title-face         ((,class (:foreground ,base :inherit bold))))
     `(mu4e-header-value-face         ((,class (:foreground ,str :inherit bold))))
     `(mu4e-highlight-face            ((,class (:foreground ,comp))))
     `(mu4e-modeline-face             ((,class (:foreground ,base))))
     `(mu4e-replied-face              ((,class (:foreground ,suc))))
     `(mu4e-special-header-value-face ((,class (:foreground ,mat))))
     `(mu4e-title-face                ((,class (:foreground ,head1 :inherit bold))))
     `(mu4e-unread-face               ((,class (:foreground ,war :inherit bold))))
     `(mu4e-view-url-number-face      ((,class (:foreground ,comment))))

;;;;; mu4e-maildirs
     `(mu4e-maildirs-extension-maildir-hl-face ((,class (:foreground ,head1 :inherit bold))))

;;;;; notmuch
     `(notmuch-hello-logo-background ((,class (:background ,bg1))))
     `(notmuch-search-date           ((,class (:foreground ,str))))
     `(notmuch-search-flagged-face   ((,class (:foreground unspecified :underline nil))))
     `(notmuch-search-unread-face    ((,class (:background ,cblk-bg :inherit bold :underline t))))
     `(notmuch-tag-face              ((,class (:foreground ,comment))))
     `(notmuch-tag-flagged           ((,class (:foreground ,war))))
     `(notmuch-tag-unread            ((,class (:foreground ,func))))
     `(notmuch-tag-red               ((,class (:foreground ,red))))
     `(notmuch-tag-green             ((,class (:foreground ,green))))
     `(notmuch-tag-yellow            ((,class (:foreground ,yellow))))
     `(notmuch-tag-orange            ((,class (:foreground ,orange))))
     `(notmuch-tag-blue              ((,class (:foreground ,blue))))
     `(notmuch-tag-magenta           ((,class (:foreground ,magenta))))
     `(notmuch-tag-aqua              ((,class (:foreground ,aqua))))
     `(notmuch-tag-cyan              ((,class (:foreground ,cyan))))
     `(notmuch-tag-gray              ((,class (:foreground ,base-dim))))

;;;;; neotree
     `(neo-dir-link-face   ((,class (:foreground ,var :inherit bold))))
     `(neo-expand-btn-face ((,class (:foreground ,base))))
     `(neo-file-link-face  ((,class (:foreground ,keyword))))
     `(neo-root-dir-face   ((,class (:foreground ,func :inherit bold))))

;;;;; org
     `(org-default                   ((,class (:inherit 'variable-pitch))))
     `(org-agenda-clocking           ((,class (:background ,highlight :foreground ,comp))))
     `(org-agenda-date               ((,class (:foreground ,var :height ,(if humanoid-org-agenda-height 1.1 1.0)))))
     `(org-agenda-date-today         ((,class (:foreground ,keyword :inherit bold :height ,(if humanoid-org-agenda-height 1.3 1.0)))))
     `(org-agenda-date-weekend       ((,class (:inherit bold :foreground ,var))))
     `(org-agenda-dimmed-todo-face   ((,class (:foreground ,comment))))
     `(org-agenda-done               ((,class (:foreground ,suc :height ,(if humanoid-org-agenda-height 1.2 1.0)))))
     `(org-agenda-structure          ((,class (:inherit bold :foreground ,comp))))
     `(org-block                     ((,class (:background ,cblk-bg :foreground ,cblk :extend t))))
     `(org-block-begin-line          ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln :extend t))))
     `(org-block-end-line            ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln :extend t))))
     `(org-clock-overlay             ((,class (:foreground ,comp))))
     `(org-code                      ((,class (:foreground ,cyan))))
     `(org-column                    ((,class (:background ,highlight))))
     `(org-column-title              ((,class (:background ,highlight))))
     `(org-date                      ((,class (:underline t :foreground ,var))))
     `(org-date-selected             ((,class (:background ,func :foreground ,act1))))
     `(org-document-info             ((,class (:foreground ,builtin))))
     `(org-document-info-keyword     ((,class (:foreground ,meta))))
     `(org-document-title            ((,class (:foreground ,func :inherit bold :height ,(if humanoid-org-height 1.4 1.0) :underline t))))
     `(org-done                      ((,class (:foreground ,suc :inherit bold :background ,green-bg))))
     `(org-ellipsis                  ((,class (:foreground ,keyword))))
     `(org-footnote                  ((,class (:underline t :foreground ,base))))
     `(org-formula                   ((,class (:foreground ,type))))
     `(org-headline-done             ((,class (:foreground ,base-dim))))
     `(org-hide                      ((,class (:foreground ,highlight-dim))))
     `(org-kbd                       ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
     `(org-latex-and-related         ((,class (:foreground ,mat))))
     `(org-link                      ((,class (:inherit link))))
     `(org-meta-line                 ((,class (:foreground ,meta))))
     `(org-mode-line-clock-overrun   ((,class (:foreground ,err))))
     `(org-priority                  ((,class (:foreground ,war :inherit bold))))
     `(org-quote                     ((,class (:inherit org-block :slant italic :extend t))))
     `(org-scheduled                 ((,class (:foreground ,comp))))
     `(org-scheduled-today           ((,class (:foreground ,func :height ,(if humanoid-org-agenda-height 1.2 1.0)))))
     `(org-scheduled-previously      ((,class (:foreground ,base :slant italic))))
     `(org-sexp-date                 ((,class (:foreground ,base))))
     `(org-special-keyword           ((,class (:foreground ,func))))
     `(org-table                     ((,class (:foreground ,base :background ,head1-bg))))
     `(org-tag                       ((,class (:foreground ,meta))))
     `(org-time-grid                 ((,class (:foreground ,str))))
     `(org-todo                      ((,class (:foreground ,war :inherit bold :background ,yellow-bg))))
     `(org-upcoming-deadline         ((,class (:foreground ,war :inherit org-priority))))
     `(org-upcoming-distant-deadline ((,class (:foreground ,suc :inherit org-priority))))
     `(org-verbatim                  ((,class (:foreground ,keyword))))
     `(org-verse                     ((,class (:inherit org-block :slant italic))))
     `(org-warning                   ((,class (:foreground ,err :inherit org-priority))))

;;;;; perspective
     `(persp-selected-face ((,class (:inherit bold :foreground ,func))))

;;;;; popup
     `(popup-enu-selection-face         ((,class (:background ,ttip-sl :foreground ,base))))
     `(popup-face                       ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(popup-isearch-match              ((,class (:inherit match))))
     `(popup-menu-face                  ((,class (:background ,ttip-bg :foreground ,base))))
     `(popup-menu-mouse-face            ((,class (:inherit highlight))))
     `(popup-scroll-bar-background-face ((,class (:background ,bg2))))
     `(popup-scroll-bar-foreground-face ((,class (:background ,act2))))
     `(popup-tip-face                   ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))

;;;;; powerline
     `(powerline-active0   ((,class (:background ,bg1 :foreground ,base))))
     `(powerline-active1   ((,class (:background ,bg3 :foreground ,base))))
     `(powerline-active2   ((,class (:background ,bg2 :foreground ,base))))
     `(powerline-inactive0 ((,class (:background ,bg1 :foreground ,base-dim))))
     `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base-dim))))
     `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,base-dim))))

;;;;; rainbow-blocks
     `(rainbow-blocks-depth-1-face   ((,class (:foreground ,cyan))))
     `(rainbow-blocks-depth-2-face   ((,class (:foreground ,yellow))))
     `(rainbow-blocks-depth-3-face   ((,class (:foreground ,blue))))
     `(rainbow-blocks-depth-4-face   ((,class (:foreground ,magenta))))
     `(rainbow-blocks-depth-5-face   ((,class (:foreground ,green))))
     `(rainbow-blocks-depth-6-face   ((,class (:foreground ,yellow))))
     `(rainbow-blocks-depth-7-face   ((,class (:foreground ,blue))))
     `(rainbow-blocks-depth-8-face   ((,class (:foreground ,magenta))))
     `(rainbow-blocks-depth-9-face   ((,class (:foreground ,green))))
     `(rainbow-blocks-unmatched-face ((,class (:foreground ,red))))

;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face    ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-2-face    ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-3-face    ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face    ((,class (:foreground ,magenta))))
     `(rainbow-delimiters-depth-5-face    ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-6-face    ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-7-face    ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-8-face    ((,class (:foreground ,magenta))))
     `(rainbow-delimiters-depth-9-face    ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-10-face   ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-11-face   ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-12-face   ((,class (:foreground ,magenta))))
     `(rainbow-delimiters-mismatched-face ((,class (:foreground ,err :overline t :inverse-video t))))
     `(rainbow-delimiters-unmatched-face  ((,class (:foreground ,err :overline t))))

;;;;; rcirc
     `(rcirc-bright-nick   ((,class (:background ,aqua-bg :foreground ,cyan))))
     `(rcirc-dim-nick      ((,class (:foreground ,base-dim))))
     `(rcirc-keyword       ((,class (:background ,green-bg-s :foreground ,green))))
     `(rcirc-timestamp     ((,class (:foreground ,keyword))))
     `(rcirc-track-keyword ((,class (:background ,green :foreground ,act1))))
     `(rcirc-url           ((,class (:inherit link))))

;;;;; shell script
     `(sh-quoted-exec ((,class (:foreground ,comp))))
     `(sh-heredoc     ((,class (:foreground ,war))))

;;;;; shm
     `(shm-current-face    ((,class (:background ,green-bg-s))))
     `(shm-quarantine-face ((,class (:background ,red-bg-s))))

;;;;; show-paren
     `(show-paren-match            ((,class (:foreground ,mat :inherit bold  :underline ,(when humanoid-underline-parens t)))))
     `(show-paren-match-expression ((,class (:background ,green-bg-s))))
     `(show-paren-mismatch         ((,class (:foreground ,err :inherit bold :underline ,(when humanoid-underline-parens t)))))

;;;;; smartparens
     `(sp-pair-overlay-face    ((,class (:background ,highlight :foreground nil))))
     `(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold  :underline ,(when humanoid-underline-parens t)))))

;;;;; smerge
     `(smerge-base            ((,class (:background ,yellow-bg :extend t))))
     `(smerge-markers         ((,class (:background ,ttip-bg :foreground ,ttip :extend t))))
     `(smerge-mine            ((,class (:background ,red-bg))))
     `(smerge-other           ((,class (:background ,green-bg))))
     `(smerge-refined-added   ((,class (:background ,green-bg-s :foreground ,green))))
     `(smerge-refined-changed ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(smerge-refined-removed ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; solaire-mode
     `(solaire-default-face    ((,class (:inherit default :background ,bg1))))
     `(solaire-hl-line-face    ((,class (:inherit hl-line :background ,bg3 :extend t))))
     `(solaire-minibuffer-face ((,class (:inherit solaire-default-face :background ,bg3))))
     `(solaire-org-hide-face   ((,class (:foreground ,bg1))))

;;;;; spaceline
     `(spaceline-evil-emacs       ((,class (:background ,blue))))
     `(spaceline-evil-insert      ((,class (:background ,green))))
     `(spaceline-evil-motion      ((,class (:background ,purple))))
     `(spaceline-evil-normal      ((,class (:foreground ,black :background ,yellow))))
     `(spaceline-evil-replace     ((,class (:background ,orange))))
     `(spaceline-evil-visual      ((,class (:background ,base-dim))))
     `(spaceline-flycheck-error   ((,class (:foreground ,err))))
     `(spaceline-flycheck-info    ((,class (:foreground ,keyword))))
     `(spaceline-flycheck-warning ((,class (:foreground ,war))))
     `(spaceline-highlight-face   ((,class (:foreground ,black :background ,yellow))))
     `(spaceline-modified         ((,class (:foreground ,act1 :background ,keyword))))
     `(spaceline-python-venv      ((,class (:foreground ,comp))))
     `(spaceline-read-only        ((,class (:background ,aqua))))
     `(spaceline-unmodified       ((,class (:foreground ,base :background ,lnum))))
     `(spacemacs-emacs-face       ((,class (:foreground ,act1 :background ,blue))))
     `(spacemacs-evilified-face   ((,class (:foreground ,act1 :background ,type))))
     `(spacemacs-hybrid-face      ((,class (:foreground ,act1 :background ,cyan))))
     `(spacemacs-insert-face      ((,class (:foreground ,act1 :background ,green))))
     `(spacemacs-lisp-face        ((,class (:foreground ,act1 :background ,magenta))))
     `(spacemacs-motion-face      ((,class (:foreground ,act1 :background ,purple))))
     `(spacemacs-normal-face      ((,class (:foreground ,black :background ,yellow))))
     `(spacemacs-replace-face     ((,class (:foreground ,act1 :background ,orange))))
     `(spacemacs-visual-face      ((,class (:foreground ,act1 :background ,base-dim))))

;;;;; spacemacs-specific
     `(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; swiper
     `(swiper-line-face    ((,class (:background ,highlight :inherit bold))))
     `(swiper-match-face-1 ((,class (:inherit bold))))
     `(swiper-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; tabbar
     `(tabbar-default             ((,class (:background ,bg1 :foreground ,head1 :height 0.9))))
     `(tabbar-button              ((,class (:inherit tabbar-default))))
     `(tabbar-button-highlight    ((,class (:inherit tabbar-default))))
     `(tabbar-highlight           ((,class (:underline t))))
     `(tabbar-selected            ((,class (:inherit tabbar-default :foreground ,func :weight bold))))
     `(tabbar-selected-modified   ((,class (:inherit tabbar-default :foreground ,red :weight bold))))
     `(tabbar-separator           ((,class (:inherit tabbar-default))))
     `(tabbar-unselected          ((,class (:inherit tabbar-default :background ,bg1 :slant italic :weight light))))
     `(tabbar-unselected-modified ((,class (:inherit tabbar-unselected :background ,bg1 :foreground ,red))))

;;;;; term
     `(term               ((,class (:foreground ,base    :background ,bg1))))
     `(term-color-black   ((,class (:foreground ,bg4     :background ,bg4))))
     `(term-color-blue    ((,class (:foreground ,keyword :background ,keyword))))
     `(term-color-cyan    ((,class (:foreground ,cyan    :background ,cyan))))
     `(term-color-green   ((,class (:foreground ,green   :background ,green))))
     `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
     `(term-color-red     ((,class (:foreground ,red     :background ,red))))
     `(term-color-white   ((,class (:foreground ,base    :background ,base))))
     `(term-color-yellow  ((,class (:foreground ,yellow  :background ,yellow))))

;;;;; vterm
     `(vterm-color-default ((,class (:foreground ,base :background ,bg1))))
     ;; vterm-color-black used to render black color code.
     ;; The foreground color is used as ANSI color 0 and the background
     ;; color is used as ANSI color 8.
     `(vterm-color-black   ((,class (:foreground ,bg4     :background ,bg4))))
     `(vterm-color-blue    ((,class (:foreground ,blue    :background ,blue))))
     `(vterm-color-cyan    ((,class (:foreground ,cyan    :background ,cyan))))
     `(vterm-color-green   ((,class (:foreground ,green   :background ,green))))
     `(vterm-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
     `(vterm-color-red     ((,class (:foreground ,red     :background ,red))))
     `(vterm-color-white   ((,class (:foreground ,base    :background ,base))))
     `(vterm-color-yellow  ((,class (:foreground ,yellow  :background ,yellow))))

;;;;; tide
     `(tide-hl-identifier-face ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; treemacs
     `(treemacs-git-added-face     ((,class (:foreground ,green :background ,green-bg))))
     `(treemacs-git-conflict-face  ((,class (:foreground ,red :background ,red-bg))))
     `(treemacs-git-ignored-face   ((,class (:foreground ,yellow))))
     `(treemacs-git-modified-face  ((,class (:foreground ,blue :background ,blue-bg))))
     `(treemacs-git-untracked-face ((,class (:foreground ,aqua :background ,aqua-bg))))

;;;;; web-mode
     `(web-mode-builtin-face                   ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-comment-face                   ((,class (:inherit font-lock-comment-face))))
     `(web-mode-constant-face                  ((,class (:inherit font-lock-constant-face))))
     `(web-mode-current-element-highlight-face ((,class (:background ,bg3))))
     `(web-mode-doctype-face                   ((,class (:inherit font-lock-comment-face))))
     `(web-mode-function-name-face             ((,class (:inherit font-lock-function-name-face))))
     `(web-mode-html-attr-name-face            ((,class (:foreground ,func))))
     `(web-mode-html-attr-value-face           ((,class (:foreground ,keyword))))
     `(web-mode-html-tag-face                  ((,class (:foreground ,keyword))))
     `(web-mode-keyword-face                   ((,class (:foreground ,keyword))))
     `(web-mode-string-face                    ((,class (:foreground ,str))))
     `(web-mode-symbol-face                    ((,class (:foreground ,type))))
     `(web-mode-type-face                      ((,class (:inherit font-lock-type-face))))
     `(web-mode-warning-face                   ((,class (:inherit font-lock-warning-face))))

;;;;; which-key
     `(which-key-command-description-face ((,class (:foreground ,base))))
     `(which-key-group-description-face   ((,class (:foreground ,keyword))))
     `(which-key-key-face                 ((,class (:foreground ,func :inherit bold))))
     `(which-key-separator-face           ((,class (:background nil :foreground ,str))))
     `(which-key-special-key-face         ((,class (:background ,func :foreground ,act1))))

;;;;; which-function-mode
     `(which-func ((,class (:foreground ,func))))

;;;;; whitespace-mode
     `(whitespace-empty            ((,class (:background nil :foreground ,yellow))))
     `(whitespace-indentation      ((,class (:background nil :foreground ,war))))
     `(whitespace-line             ((,class (:background nil :foreground ,comp))))
     `(whitespace-newline          ((,class (:background nil :foreground ,comp))))
     `(whitespace-space            ((,class (:background nil :foreground ,act2))))
     `(whitespace-space-after-tab  ((,class (:background nil :foreground ,yellow))))
     `(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
     `(whitespace-tab              ((,class (:background nil :foreground ,act2))))
     `(whitespace-trailing         ((,class (:background ,red-bg-s :foreground ,war))))

;;;;; other, need more work
     `(ac-completion-face                   ((,class (:underline t :foreground ,keyword))))
     `(escape-glyph                         ((,class (:foreground ,comment))))
     `(escape-glyph-face                    ((,class (:foreground ,comment))))
     `(ffap                                 ((,class (:foreground ,base))))
     `(flx-highlight-face                   ((,class (:foreground ,comp :underline nil))))
     `(icompletep-determined                ((,class (:foreground ,keyword))))
     `(js2-external-variable                ((,class (:foreground ,comp))))
     `(js2-function-param                   ((,class (:foreground ,const))))
     `(js2-jsdoc-html-tag-delimiter         ((,class (:foreground ,str))))
     `(js2-jsdoc-html-tag-name              ((,class (:foreground ,keyword))))
     `(js2-jsdoc-value                      ((,class (:foreground ,str))))
     `(js2-private-function-call            ((,class (:foreground ,const))))
     `(js2-private-member                   ((,class (:foreground ,base))))
     `(js3-error-face                       ((,class (:underline ,war))))
     `(js3-external-variable-face           ((,class (:foreground ,var))))
     `(js3-function-param-face              ((,class (:foreground ,keyword))))
     `(js3-instance-member-face             ((,class (:foreground ,const))))
     `(js3-jsdoc-tag-face                   ((,class (:foreground ,keyword))))
     `(js3-warning-face                     ((,class (:underline ,keyword))))
     `(slime-repl-inputed-output-face       ((,class (:foreground ,comp))))
     `(trailing-whitespace                  ((,class (:foreground nil :background ,err))))
     `(undo-tree-visualizer-current-face    ((,class (:foreground ,keyword))))
     `(undo-tree-visualizer-default-face    ((,class (:foreground ,base))))
     `(undo-tree-visualizer-register-face   ((,class (:foreground ,comp))))
     `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,var)))))

    (custom-theme-set-variables
     theme-name

;;;;; ansi-color-names
     `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,base])

;;;;; hl-todo
     `(hl-todo-keyword-faces '(("TODO"       . ,magenta)
                               ("NEXT"       . ,magenta)
                               ("THEM"       . ,aqua)
                               ("PROG"       . ,cyan)
                               ("OKAY"       . ,blue)
                               ("DONT"       . ,war)
                               ("FAIL"       . ,err)
                               ("DONE"       . ,suc)
                               ("NOTE"       . ,yellow)
                               ("KLUDGE"     . ,orange)
                               ("HACK"       . ,purple)
                               ("TEMP"       . ,gray)
                               ("FIXME"      . ,magenta)
                               ("XXX"        . ,var)
                               ("XXXX"       . ,var)
                               ("\\?\\?\\?+" . ,var)))

;;;;; pdf-tools
     `(pdf-view-midnight-colors '(,base . ,bg1)))))

(deftheme humanoid-dark "Humanoid theme, the dark version")

(humanoid-create 'dark 'humanoid-dark)

(provide-theme 'humanoid-dark)

;;; humanoid-dark-theme.el ends here
