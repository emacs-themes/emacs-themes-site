;;; nofrils-sepia-theme.el --- Port of "No Frils Sepia" Vim theme.

;; Copyright (c) 2018 Eric Sessoms
;; See COPYING for details.

;; Author: Eric Sessoms <esessoms@protonmail.com>
;; Package-Requires: ((emacs "24"))
;; URL: https://gitlab.com/esessoms/nofrils-theme
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Minimal syntax highlighting to reduce distractions.  Only
;; highlights comments and errors by default.

;; (require 'nofrils-sepia-theme)
;; (load-theme 'nofrils-sepia t)

;;; Credits:

;; This theme was ported from No Frils Sepia by Robert Melton.
;; https://github.com/robertmeta/nofrils

;;; Code:

(deftheme nofrils-sepia
  "Port of No Frils Sepia by Robert Melton.")

(let ((background "#FFDFAF")
      (foreground "#000000")
      (comment "#AF8700")
      (error "#FF5555")
      (fringe "#AF7800")
      (search "#00CDCD"))

  (custom-theme-set-faces
   'nofrils-sepia

   `(default ((t :background ,background :foreground ,foreground)))

   ;; Highlight only comments and errors.
   `(error ((t :background "white" :foreground ,error)))
   `(font-lock-builtin-face ((t nil)))
   `(font-lock-comment-face ((t :foreground ,comment)))
   `(font-lock-constant-face ((t nil)))
   `(font-lock-function-name-face ((t nil)))
   `(font-lock-keyword-face ((t nil)))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-regexp-grouping-backslash ((t nil)))
   `(font-lock-regexp-grouping-construct ((t nil)))
   `(font-lock-string-face ((t nil)))
   `(font-lock-type-face ((t nil)))
   `(font-lock-variable-name-face ((t nil)))

   ;; Show searches and selections.
   `(isearch ((t :background ,search :foreground "white")))
   `(lazy-highlight ((t :background "black" :foreground "white")))
   `(region ((t :background ,foreground :foreground ,background)))

   ;; Parenthesis matching is never wrong.
   `(show-paren-match ((t :weight bold)))
   `(show-paren-mismatch ((t :background ,error :weight bold)))

   `(fringe ((t :background ,fringe)))
   `(minibuffer-prompt ((t :foreground ,foreground)))
   `(mode-line ((t :background "black" :foreground "white")))
   `(mode-line-inactive ((t :background ,fringe)))

   ;; Org mode needs to chill.
   `(org-done ((t :weight bold)))
   `(org-todo ((t :weight bold)))))

;;; Footer:

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nofrils-sepia)

(provide 'nofrils-sepia-theme)

;;; nofrils-sepia-theme.el ends here
