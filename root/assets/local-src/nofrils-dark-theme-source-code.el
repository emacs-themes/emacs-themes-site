;;; nofrils-dark-theme.el --- Port of "No Frils Dark" Vim theme.

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

;; (require 'nofrils-dark-theme)
;; (load-theme 'nofrils-dark t)

;;; Credits:

;; This theme was ported from No Frils Dark by Robert Melton.
;; https://github.com/robertmeta/nofrils

;;; Code:

(deftheme nofrils-dark
  "Port of No Frils Dark by Robert Melton.")

(let ((background "#262626")
      (foreground "#EEEEEE")
      (comment "#6C6C6C")
      (error "#5F0000")
      (fringe "#6C6C6C")
      (search "#00CDCD"))

  (custom-theme-set-faces
   'nofrils-dark

   `(default ((t :background ,background :foreground ,foreground)))

   ;; Highlight only comments and errors.
   `(error ((t :background ,error)))
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
   `(isearch ((t :background ,search :foreground "black")))
   `(lazy-highlight ((t :background "green" :foreground "black")))
   `(region ((t :background ,foreground :foreground ,background)))

   ;; Parenthesis matching is never wrong.
   `(show-paren-match ((t :weight bold)))
   `(show-paren-mismatch ((t :background ,error :weight bold)))

   `(fringe ((t :background ,fringe)))
   `(minibuffer-prompt ((t :foreground ,foreground)))
   `(mode-line ((t :background ,foreground :foreground ,background)))
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

(provide-theme 'nofrils-dark)

(provide 'nofrils-dark-theme)

;;; nofrils-dark-theme.el ends here
