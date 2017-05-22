;;; lenlen-theme.el --- a solarized-based kawaii light theme

;; Copyright (C) 2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0
;; Package-Requires: ((color-theme-solarized "20150110"))

;;; Commentary:

;; a solarized-based kawaii light theme.
;;
;;   (load-theme 'lenlen-theme t)

;;; Change Log:

;;; Code:

(require 'solarized-definitions
         (locate-file "solarized-definitions.el" custom-theme-load-path '("c" "")))

(defconst lenlen-theme-version "1.0.0")

(create-solarized-theme
 lenlen-theme "a solarized-based kawaii light theme"
 (let ((solarized-colors
        '((base03  "#281e03") (base02  "#2f250a") (base01  "#736c59") (base00  "#7d7665")
          (base0   "#979182") (base1   "#a49f91") (base2   "#f8f6f1") (base3   "#fffdf9")
          (yellow  "#db8d2e") (orange  "#f77e96") (red     "#f47166") (magenta "#b04d99")
          (violet  "#51981b") (blue    "#fda700") (cyan    "#34bd7d") (green   "#59a9d2"))))
   (solarized-color-definitions)))

(custom-theme-set-variables 'lenlen-theme '(frame-background-mode 'light))

(provide 'lenlen-theme)

;;; lenlen-theme.el ends here
