;;; purp-common.el --- simple themes with few colors

;; Author: Vincent Foley <vfoley@gmail.com>
;; URL: https://github.com/gnuvince/purp
;; Version: 1.0
;; Keywords: faces

;;; Commentary:
;;; - mktheme: a function that abstracts away the actual colors

;;; Code:

(defun purp-common-mktheme (theme-name &rest colors)
  "Create a purp theme.
THEME-NAME is the of the variant, COLORS is a plist of the colors to use."
  (let* (
         ;; The four main text colors (excluding special handling)
         (default-fg  (plist-get colors :default-fg))
         (string-fg   (plist-get colors :string-fg))
         (function-fg (plist-get colors :function-fg))
         (comment-fg  (plist-get colors :comment-fg))

         (default-bg  (plist-get colors :default-bg))
         (bright-bg   (plist-get colors :bright-bg))
         (inactive-bg (plist-get colors :inactive-bg))

         (error-fg    (plist-get colors :error-fg))
         (error-bg    (plist-get colors :error-bg))

         ;; special cases
         (hl-line-bg  (plist-get colors :hl-line-bg))
         )
    (custom-theme-set-faces
     theme-name
     `(default                             ((t (:background ,default-bg :foreground ,default-fg))))
     `(region                              ((t (:background ,bright-bg))))
     `(cursor                              ((t (:background ,default-fg))))

     `(font-lock-builtin-face              ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-comment-delimiter-face    ((t (:background ,default-bg :foreground ,comment-fg))))
     `(font-lock-comment-face              ((t (:background ,default-bg :foreground ,comment-fg))))
     `(font-lock-constant-face             ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-doc-face                  ((t (:background ,default-bg :foreground ,comment-fg))))
     `(font-lock-function-name-face        ((t (:background ,default-bg :foreground ,function-fg))))
     `(font-lock-keyword-face              ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-negation-char-face        ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-preprocessor-face         ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-regexp-grouping-backslash ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-regexp-grouping-construct ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-string-face               ((t (:background ,default-bg :foreground ,string-fg))))
     `(font-lock-type-face                 ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-variable-name-face        ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-warning-face              ((t (:weight bold :background ,error-bg :foreground ,error-fg))))

     `(mode-line                           ((t (:foreground ,default-fg :background ,bright-bg))))
     `(mode-line-inactive                  ((t (:foreground ,default-fg :background ,inactive-bg))))

     `(fringe                              ((t (:background ,default-bg))))

     ;; isearch
     `(lazy-highlight                      ((t (:background ,bright-bg))))

     `(hl-line                             ((t (:background ,hl-line-bg))))

     `(show-paren-mismatch                 ((t (:background ,error-bg))))
     `(show-paren-match                    ((t (:background ,bright-bg))))

     ;; markdown-mode
     `(markdown-blockquote-face       ((t (:foreground ,default-fg))))
     `(markdown-bold-face             ((t (:foreground ,default-fg :weight bold))))
     `(markdown-code-face             ((t (:foreground ,default-fg))))
     `(markdown-comment-face          ((t (:foreground ,comment-fg))))
     `(markdown-footnote-marker-face  ((t (:foreground ,default-fg))))
     `(markdown-footnote-text-face    ((t (:foreground ,default-fg))))
     `(markdown-gfm-checkbox-face     ((t (:foreground ,default-fg))))
     `(markdown-header-delimiter-face ((t (:foreground ,default-fg))))
     `(markdown-header-face           ((t (:foreground ,default-fg))))
     `(markdown-header-face-1         ((t (:foreground ,function-fg))))
     `(markdown-header-face-2         ((t (:foreground ,function-fg))))
     `(markdown-header-face-3         ((t (:foreground ,function-fg))))
     `(markdown-header-face-4         ((t (:foreground ,function-fg))))
     `(markdown-header-face-5         ((t (:foreground ,function-fg))))
     `(markdown-header-face-6         ((t (:foreground ,function-fg))))
     `(markdown-header-rule-face      ((t (:foreground ,default-fg))))
     `(markdown-highlight-face        ((t (:foreground ,default-fg))))
     `(markdown-hr-face               ((t (:foreground ,default-fg))))
     `(markdown-inline-code-face      ((t (:foreground ,default-fg))))
     `(markdown-italic-face           ((t (:foreground ,default-fg :slant italic))))
     `(markdown-language-info-face    ((t (:foreground ,default-fg))))
     `(markdown-language-keyword-face ((t (:foreground ,default-fg))))
     `(markdown-line-break-face       ((t (:foreground ,default-fg))))
     `(markdown-link-face             ((t (:foreground ,default-fg))))
     `(markdown-link-title-face       ((t (:foreground ,default-fg))))
     `(markdown-list-face             ((t (:foreground ,default-fg))))
     `(markdown-markup-face           ((t (:foreground ,default-fg))))
     `(markdown-math-face             ((t (:foreground ,default-fg))))
     `(markdown-metadata-key-face     ((t (:foreground ,comment-fg))))
     `(markdown-metadata-value-face   ((t (:foreground ,comment-fg))))
     `(markdown-missing-link-face     ((t (:foreground ,default-fg))))
     `(markdown-plain-url-face        ((t (:foreground ,default-fg))))
     `(markdown-pre-face              ((t (:foreground ,default-fg))))
     `(markdown-reference-face        ((t (:foreground ,default-fg))))
     `(markdown-strike-through-face   ((t (:foreground ,default-fg :strike-through t))))
     `(markdown-url-face              ((t (:foreground ,default-fg))))

     ;; org-mode
     `(org-agenda-calendar-event    ((t (:foreground ,default-fg))))
     `(org-agenda-calendar-sexp     ((t (:foreground ,default-fg))))
     `(org-agenda-clocking          ((t (:foreground ,default-fg))))
     `(org-agenda-column-dateline   ((t (:foreground ,default-fg))))
     `(org-agenda-current-time      ((t (:foreground ,default-fg))))
     `(org-agenda-date              ((t (:foreground ,default-fg))))
     `(org-agenda-date-today        ((t (:foreground ,default-fg))))
     `(org-agenda-date-weekend      ((t (:foreground ,default-fg))))
     `(org-agenda-diary             ((t (:foreground ,default-fg))))
     `(org-agenda-dimmed-todo-face  ((t (:foreground ,default-fg))))
     `(org-agenda-done              ((t (:foreground ,default-fg))))
     `(org-agenda-filter-category   ((t (:foreground ,default-fg))))
     `(org-agenda-filter-effort     ((t (:foreground ,default-fg))))
     `(org-agenda-filter-regexp     ((t (:foreground ,default-fg))))
     `(org-agenda-filter-tags       ((t (:foreground ,default-fg))))
     `(org-agenda-restriction-lock  ((t (:foreground ,default-fg))))
     `(org-agenda-structure         ((t (:foreground ,default-fg))))
     `(org-archived                 ((t (:foreground ,default-fg))))
     `(org-block                    ((t (:foreground ,default-fg))))
     `(org-block-begin-line         ((t (:foreground ,default-fg))))
     `(org-block-end-line           ((t (:foreground ,default-fg))))
     `(org-checkbox                 ((t (:foreground ,default-fg))))
     `(org-checkbox-statistics-done ((t (:foreground ,default-fg))))
     `(org-checkbox-statistics-todo ((t (:foreground ,default-fg))))
     `(org-clock-overlay            ((t (:foreground ,default-fg))))
     `(org-code                     ((t (:foreground ,default-fg))))
     `(org-column                   ((t (:foreground ,default-fg))))
     `(org-column-title             ((t (:foreground ,default-fg))))
     `(org-date                     ((t (:foreground ,default-fg))))
     `(org-date-selected            ((t (:foreground ,default-fg))))
     `(org-default                  ((t (:foreground ,default-fg))))
     `(org-document-info            ((t (:foreground ,default-fg))))
     `(org-document-info-keyword    ((t (:foreground ,default-fg))))
     `(org-document-title           ((t (:foreground ,default-fg))))
     `(org-done                     ((t (:foreground ,comment-fg))))
     `(org-drawer                   ((t (:foreground ,default-fg))))
     `(org-ellipsis                 ((t (:foreground ,default-fg))))
     `(org-footnote                 ((t (:foreground ,default-fg))))
     `(org-formula                  ((t (:foreground ,default-fg))))
     `(org-headline-done            ((t (:foreground ,default-fg))))
     `(org-hide                     ((t (:foreground ,default-bg))))
     `(org-indent                   ((t (:foreground ,default-bg))))
     `(org-latex-and-related        ((t (:foreground ,default-fg))))
     `(org-level-1                  ((t (:foreground ,function-fg))))
     `(org-level-2                  ((t (:foreground ,function-fg))))
     `(org-level-3                  ((t (:foreground ,function-fg))))
     `(org-level-4                  ((t (:foreground ,function-fg))))
     `(org-level-5                  ((t (:foreground ,function-fg))))
     `(org-level-6                  ((t (:foreground ,function-fg))))
     `(org-level-7                  ((t (:foreground ,function-fg))))
     `(org-level-8                  ((t (:foreground ,function-fg))))
     `(org-link                     ((t (:foreground ,default-fg))))
     `(org-list-dt                  ((t (:foreground ,default-fg :weight bold))))
     `(org-macro                    ((t (:foreground ,default-fg))))
     `(org-meta-line                ((t (:foreground ,default-fg))))
     `(org-mode-line-clock          ((t (:foreground ,default-fg))))
     `(org-mode-line-clock-overrun  ((t (:foreground ,default-fg))))
     `(org-priority                 ((t (:foreground ,default-fg))))
     `(org-property-value           ((t (:foreground ,default-fg))))
     `(org-quote                    ((t (:foreground ,default-fg))))
     `(org-scheduled                ((t (:foreground ,default-fg))))
     `(org-scheduled-previously     ((t (:foreground ,default-fg))))
     `(org-scheduled-today          ((t (:foreground ,default-fg))))
     `(org-sexp-date                ((t (:foreground ,default-fg))))
     `(org-special-keyword          ((t (:foreground ,default-fg))))
     `(org-table                    ((t (:foreground ,default-fg))))
     `(org-tag                      ((t (:foreground ,default-fg))))
     `(org-tag-group                ((t (:foreground ,default-fg))))
     `(org-target                   ((t (:foreground ,default-fg))))
     `(org-time-grid                ((t (:foreground ,default-fg))))
     `(org-todo                     ((t (:foreground ,error-fg))))
     `(org-upcoming-deadline        ((t (:foreground ,default-fg))))
     `(org-verbatim                 ((t (:foreground ,default-fg))))
     `(org-verse                    ((t (:foreground ,default-fg))))
     `(org-warning                  ((t (:foreground ,default-fg))))

     `(ediff-current-diff-A        ((t (:foreground ,default-fg :background "#aa3300"))))
     `(ediff-current-diff-Ancestor ((t (:foreground ,default-fg :background ,default-bg))))
     `(ediff-current-diff-B        ((t (:foreground ,default-fg :background "#33aa00"))))
     `(ediff-current-diff-C        ((t (:foreground ,default-fg :background "#0033aa"))))
     `(ediff-even-diff-A           ((t (:foreground ,default-fg :background "#444444"))))
     `(ediff-even-diff-Ancestor    ((t (:foreground ,default-fg :background ,default-bg))))
     `(ediff-even-diff-B           ((t (:foreground ,default-fg :background "#444444"))))
     `(ediff-even-diff-C           ((t (:foreground ,default-fg :background "#444444"))))
     `(ediff-fine-diff-A           ((t (:foreground ,default-fg :background "#551100"))))
     `(ediff-fine-diff-Ancestor    ((t (:foreground ,default-fg :background ,default-bg))))
     `(ediff-fine-diff-B           ((t (:foreground ,default-fg :background "#115500"))))
     `(ediff-fine-diff-C           ((t (:foreground ,default-fg :background "#001155"))))
     `(ediff-odd-diff-A            ((t (:foreground ,default-fg :background "#444444"))))
     `(ediff-odd-diff-Ancestor     ((t (:foreground ,default-fg :background ,default-bg))))
     `(ediff-odd-diff-B            ((t (:foreground ,default-fg :background "#444444"))))
     `(ediff-odd-diff-C            ((t (:foreground ,default-fg :background "#444444"))))

     )))

(provide 'purp-common)

;;; purp-common.el ends here
;;; purp-light-theme.el --- A dark color theme with few colors

;;; Copyright (C) 2018 Vincent Foley

;; Author: Vincent Foley <vfoley@gmail.com>
;; URL: https://github.com/gnuvince/purp
;; Version: 1.0
;; Keywords: faces

;;; Commentary:
;;; This is the dark variant of purp.

;;; Code:

(require 'purp-common)

(deftheme purp
  "a theme that highlights only a few constructs")

(purp-common-mktheme
 'purp
 :default-fg          "#cccccc"
 :string-fg           "#ffcc55"
 :function-fg         "#ff55ff"
 :comment-fg          "#55ff55"

 :default-bg          "#222222"
 :bright-bg           "#550055"
 :inactive-bg         "#555555"

 :error-fg            "#ffff00"
 :error-bg            "#aa0000"

 ;; special cases
 :hl-line-bg          "#303030"
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'purp)

;;; purp-theme.el ends here
