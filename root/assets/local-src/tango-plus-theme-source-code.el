;;; tango-plus-theme.el --- A color theme based on the tango palette

;; Copyright (C) 2013 Titus von der Malsburg &lt;malsburg@posteo.de&gt;

;; Author: Titus von der Malsburg &lt;malsburg@posteo.de&gt;
;; Maintainer: Titus von der Malsburg &lt;malsburg@posteo.de&gt;
;; URL: https://github.com/tmalsburg/tango-plus-theme
;; Version: 1.0.0

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

;; This theme is based on the tango theme that is part of GNU Emacs.
;;
;; Most colors in this theme come from the Tango palette, which is in
;; the public domain: http://tango.freedesktop.org/
;; Some colors were added to improve contrast.
;;
;; For details and screenshots visit the project page on Github:
;;
;;     https://github.com/tmalsburg/tango-plus-theme
;;
;; To use this theme put the following in your startup file:
;;
;;     (load-theme 'tango-plus t)
;;

;;; Install:

;; Put this file on your Emacs-Lisp load path and add the following in
;; your Emacs startup file:
;;
;;     (load-theme 'tango-plus t)

;;; Code:

(deftheme tango-plus
  "Face colors using the Tango palette (light background).
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#5f615c") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (white "#ffffff") (black "#000000") (plum-0 "#edc4e2")
      (red-0 "#ffe6e6") (cham-0 "#e6ffc2") (cham-4 "#346604")
      (blue-0 "#8cc4ff") (orange-4 "#b35000"))

  (custom-theme-set-faces
   'tango-plus
   `(default                        ((,class (:foreground ,black :background ,white))))
   `(cursor                         ((,class (:inverse-video t))))
   ;; Highlighting faces
   `(fringe                         ((,class (:foreground ,alum-2 :background ,white))))
   `(highlight                      ((,class (:background ,alum-1))))
   `(sentence-highlight-face        ((,class (:background ,alum-1))))
   `(show-paren-match               ((,class (:background ,alum-1))))
   `(region                         ((,class (:background ,butter-1))))
   `(secondary-selection            ((,class (:background ,blue-0))))
   `(isearch                        ((,class (:foreground ,white :background ,cham-1))))
   `(lazy-highlight                 ((,class (:background ,cham-0))))
   `(evil-ex-substitute-matches     ((,class (:background ,red-0 :strike-through ,red-1))))
   `(evil-ex-substitute-replacement ((,class (:background ,cham-0))))
   `(helm-swoop-target-word-face    ((,class (:background ,cham-1))))
   `(helm-swoop-target-line-face    ((,class (:background ,alum-1))))
   `(helm-swoop-target-line-block-face ((,class (:background ,alum-1))))
   `(trailing-whitespace            ((,class (:background ,red-1))))
   ;; Mode line faces
   `(mode-line                      ((,class (:background ,alum-5 :foreground ,alum-1))))
   `(mode-line-inactive             ((,class (:background ,alum-4 :foreground ,alum-3))))
   ;; Escape and prompt faces
   `(minibuffer-prompt              ((,class (:weight bold :foreground ,blue-3))))
   `(escape-glyph                   ((,class (:foreground ,red-3))))
   `(error                          ((,class (:foreground ,red-3))))
   `(warning                        ((,class (:foreground ,orange-3))))
   `(success                        ((,class (:foreground ,cham-3))))
   ;; Font lock faces
   `(font-lock-builtin-face         ((,class (:foreground ,plum-2))))
   `(font-lock-comment-face         ((,class (:slant italic :foreground ,alum-4))))
   `(font-lock-constant-face        ((,class (:weight light :foreground ,blue-3))))
   `(font-lock-function-name-face   ((,class (:foreground ,red-3))))
   `(font-lock-keyword-face         ((,class (:foreground ,blue-3 :weight light))))
   `(font-lock-string-face          ((,class (:foreground ,choc-3 :slant italic))))
   `(font-lock-type-face            ((,class (:foreground ,blue-3))))
   `(font-lock-variable-name-face   ((,class (:foreground ,red-3))))
   ;; Button and link faces
   `(link                           ((,class (:underline t :foreground ,blue-3))))
   `(link-visited                   ((,class (:underline t :foreground ,blue-2))))
   ;; Gnus faces
   `(gnus-group-news-1              ((,class (:weight bold :foreground ,plum-3))))
   `(gnus-group-news-1-low          ((,class (:foreground ,plum-3))))
   `(gnus-group-news-2              ((,class (:weight bold :foreground ,blue-3))))
   `(gnus-group-news-2-low          ((,class (:foreground ,blue-3))))
   `(gnus-group-news-3              ((,class (:weight bold :foreground ,red-3))))
   `(gnus-group-news-3-low          ((,class (:foreground ,red-3))))
   `(gnus-group-news-4              ((,class (:weight bold :foreground ,"#7a4c02"))))
   `(gnus-group-news-4-low          ((,class (:foreground ,"#7a4c02"))))
   `(gnus-group-news-5              ((,class (:weight bold :foreground ,orange-3))))
   `(gnus-group-news-5-low          ((,class (:foreground ,orange-3))))
   `(gnus-group-news-low            ((,class (:foreground ,alum-4))))
   `(gnus-group-mail-1              ((,class (:weight bold :foreground ,plum-3))))
   `(gnus-group-mail-1-low          ((,class (:foreground ,plum-3))))
   `(gnus-group-mail-2              ((,class (:weight bold :foreground ,blue-3))))
   `(gnus-group-mail-2-low          ((,class (:foreground ,blue-3))))
   `(gnus-group-mail-3              ((,class (:weight bold :foreground ,cham-3))))
   `(gnus-group-mail-3-low          ((,class (:foreground ,cham-3))))
   `(gnus-group-mail-low            ((,class (:foreground ,alum-4))))
   `(gnus-header-content            ((,class (:foreground ,cham-3))))
   `(gnus-header-from               ((,class (:weight bold :foreground ,butter-3))))
   `(gnus-header-subject            ((,class (:foreground ,red-3))))
   `(gnus-header-name               ((,class (:foreground ,blue-3))))
   `(gnus-header-newsgroups         ((,class (:foreground ,alum-4))))
   ;; Message faces
   `(message-header-name            ((,class (:foreground ,blue-3))))
   `(message-header-cc              ((,class (:foreground ,butter-3))))
   `(message-header-othe            ((,class (:foreground ,choc-2))))
   `(message-header-subj            ((,class (:foreground ,red-3))))
   `(message-header-to              ((,class (:weight bold :foreground ,butter-3))))
   `(message-cited-text             ((,class (:slant italic :foreground ,alum-5))))
   `(message-separator              ((,class (:weight bold :foreground ,cham-3))))
   ;; SMerge
   `(smerge-refined-change          ((,class (:background ,plum-1))))
   ;; Ediff
   `(ediff-current-diff-A           ((,class (:foreground ,black :background ,red-0))))
   `(ediff-fine-diff-A              ((,class (:foreground ,black :background ,red-1))))
   `(ediff-current-diff-B           ((,class (:foreground ,black :background ,cham-0))))
   `(ediff-fine-diff-B              ((,class (:foreground ,black :background ,cham-1))))
   `(ediff-even-diff-A              ((,class (:foreground ,black :background ,alum-1))))
   `(ediff-even-diff-B              ((,class (:foreground ,black :background ,alum-1))))
   `(ediff-odd-diff-A               ((,class (:foreground ,black :background ,alum-1))))
   `(ediff-odd-diff-B               ((,class (:foreground ,black :background ,alum-1))))
   ;; Flyspell
   `(flyspell-duplicate             ((,class (:underline ,orange-1))))
   `(flyspell-incorrect             ((,class (:underline ,red-1 :background ,red-0))))
   ;; Org mode
   `(org-level-1                    ((,class (:foreground ,blue-3 :weight bold))))
   `(org-level-2                    ((,class (:foreground ,blue-3))))
   `(org-level-3                    ((,class (:foreground ,blue-3))))
   `(org-level-4                    ((,class (:foreground ,blue-3))))
   `(org-level-5                    ((,class (:foreground ,blue-3))))
   `(org-level-6                    ((,class (:foreground ,blue-3))))
   `(org-level-7                    ((,class (:foreground ,blue-3))))
   `(org-level-8                    ((,class (:foreground ,blue-3))))
   `(org-todo                       ((,class (:foreground ,red-2 :weight bold))))
   `(org-done                       ((,class (:foreground ,cham-3))))
   `(org-table                      ((,class (:foreground ,blue-3))))
   `(org-date                       ((,class (:foreground ,plum-1))))
   `(org-footnote                   ((,class (:foreground ,alum-5))))
   `(org-hide                       ((,class (:foreground ,alum-1))))
   ;; Mu4e
   `(mu4e-flagged-face              ((,class (:foreground ,red-3 :weight bold))))
   `(mu4e-unread-face               ((,class (:foreground ,blue-3 :weight bold))))
   `(mu4e-replied-face              ((,class (:foreground ,alum-4))))
   `(mu4e-header-highlight-face     ((,class (:background ,alum-1))))
   ;; Helm
   `(helm-source-header             ((,class (:background ,butter-2 :foreground ,alum-5
                                              :weight bold :height 1.3
                                              :family "Sans Serif"))))
   `(helm-selection                 ((,class (:background ,alum-1))))
   `(helm-action                    ((,class ())))
   `(helm-candidate-number          ((,class (:background ,alum-5
                                              :foreground ,butter-1))))
   ;; Markdown mode
   `(markdown-italic-face           ((,class (:slant italic))))
   `(markdown-bold-face             ((,class (:weight bold))))
   `(markdown-header-rule-face      ((,class (:foreground ,blue-3 :weight bold))))
   `(markdown-header-delimiter-face ((,class (:foreground ,blue-3))))
   `(markdown-header-face           ((,class (:foreground ,blue-3))))
   `(markdown-header-face-1         ((,class (:inherit markdown-header-face
                                              :weight bold))))
   `(markdown-header-face           ((,class (:foreground ,blue-3))))
   `(markdown-inline-code-face      ((,class (:foreground ,choc-3))))
   `(markdown-list-face             ((,class (:weight bold))))
   `(markdown-blockquote-face       ((,class (:foreground ,choc-3 :slant italic))))
   `(markdown-pre-face              ((,class (:foreground ,choc-3))))
   `(markdown-language-keyword-face ((,class (:foreground ,blue-3))))
   `(markdown-link-face             ((,class (:foreground ,blue-3))))
   `(markdown-missing-link-face     ((,class (:foreground ,blue-3))))
   `(markdown-reference-face        ((,class (:foreground ,blue-3))))
   `(markdown-footnote-face         ((,class (:foreground ,blue-3))))
   `(markdown-url-face              ((,class (:foreground ,blue-3))))
   `(markdown-link-title-face       ((,class (:foreground ,blue-3))))
   `(markdown-link-break-face       ((,class (:foreground ,blue-3))))
   `(markdown-comment-face          ((,class (:foreground ,alum-4))))
   `(markdown-math-face             ((,class (:foreground ,blue-3))))
   ;; Semantic faces
   `(semantic-decoration-on-includes
                                    ((,class (:underline  ,cham-4))))
   `(semantic-decoration-on-private-members-face
                                    ((,class (:background ,alum-2))))
   `(semantic-decoration-on-protected-members-face
                                    ((,class (:background ,alum-2))))
   `(semantic-decoration-on-unknown-includes
                                    ((,class (:background ,choc-3))))
   `(semantic-decoration-on-unparsed-includes
                                    ((,class (:underline  ,orange-3))))
   `(semantic-tag-boundary-face
                                    ((,class (:overline   ,blue-1))))
   `(semantic-unmatched-syntax-face
                                    ((,class (:underline  ,red-1)))))

  (custom-theme-set-variables
   'tango-plus
   `(ansi-color-names-vector [,alum-6 ,red-3 ,cham-3 ,butter-3
				      ,blue-3 ,plum-3 ,blue-1 ,alum-1])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tango-plus)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tango-plus-theme.el ends here
