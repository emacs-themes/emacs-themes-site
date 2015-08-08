;;; rippedcasts-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011 Matt Woodward.

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

(deftheme rippedcasts
  "Similar to the railscasts theme by Oleg Shaldybin")

(custom-theme-set-faces
 'rippedcasts
 '(default ((t (:background "#232323" :foreground "#e6e1de"))))
 '(cursor ((t (:background "#232323" :foreground "#5a647e"))))
 '(region ((t (:background "#555577" :foreground "#e6e1de" :weight bold))))
 '(mode-line ((t (:background "#000000" :foreground "#a5baf1"))))
 '(mode-line-inactive ((t (:background "#525c78" :foreground "#91a3d4"))))
 '(fringe ((t (:background "#232323"))))
 '(minibuffer-prompt ((t (:foreground "#ff6600" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#d0d0ff"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#bc9458"))))
 '(font-lock-constant-face ((t (:foreground "#6d9cbe"))))
 '(font-lock-function-name-face ((t (:foreground "#ffc66d"))))
 '(font-lock-keyword-face ((t (:foreground "#cc7833"))))
 '(font-lock-string-face ((t (:foreground "#a5c261"))))
 '(font-lock-type-face ((t (:foreground "#ffffff"))))
 '(font-lock-variable-name-face ((t (:foreground "#b0c4de"))))
 '(font-lock-warning-face ((t (:foreground "#cd6889" :weight bold))))
 '(isearch ((t (:background "#555555" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#556b2f"))))
 '(link ((t (:foreground "#0000ff" :underline t))))
 '(link-visited ((t (:foreground "#8b008b" :underline t))))
 '(button ((t (:underline t))))
 '(header-line ((t (:background "#5a647e" :foreground "#e6e1de")))))

(provide-theme 'rippedcasts)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; rippedcasts-theme.el  ends here
