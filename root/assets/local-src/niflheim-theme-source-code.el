;;; niflheim-theme.el --- A port of the Nifleim theme to Emacs

;; Copyright (C) 2014  Nicolas Petton

;; Author: Nicolas Petton &lt;petton.nicolas@gmail.com&gt;, Benjamin Van Ryseghem &lt;benjamin.vanryseghem@gmail.com&gt;
;; Keywords: themes
;; URL: https://github.com/niflheim-theme/emacs
;; Version: 0.1

;; This file is NOT part of GNU Emacs

;;; License:
;;
;; Niflheim (https://github.com/niflheim-theme) is licensed under a
;; Creative Commons Attribution-ShareAlike 4.0 International License.

;;; Commentary:
;;
;; A port the the Niflheim dark theme for Emacs
;; (https://github.com/niflheim-theme)
;;

;;; Code:

(deftheme niflheim
  "A dark medium contrast theme")

(let ((class '((class color) (min-colors 89)))
          (background-color "#303030")
          (orange "#ffcd8e")
          (purple "#cbaaf5")
          (default-color "#b8c4cf")
          (cursor-background "#b6c4cf"))
  (custom-theme-set-faces
   'niflheim
   `(default ((,class (:background ,background-color :foreground ,default-color))))
   `(cursor ((,class (:background ,cursor-background))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#202020"))))
   `(highlight ((,class (:background "#454545" :foreground "#ffffff" :underline t))))
   `(region ((,class (:background "#666666" :foreground "#f6f3e8"))))
   `(secondary-selection ((,class (:background "#252525" :foreground "#f6f3e8"))))
   `(isearch ((,class (:background "#aaeeab" :foreground "#202020"))))
   `(lazy-highlight ((,class (:background "#789771" :foreground "#444444"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#202020" :foreground "#cccccc"))))
   `(mode-line-inactive ((,class (:background "#444444" :foreground "#aaaaaa"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,orange :weight bold))))
   `(escape-glyph ((,class (:foreground ,orange :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#ff6c6b" :weight bold))))
   `(font-lock-comment-face ((,class (:foreground "#929283"))))
   `(font-lock-constant-face ((,class (:foreground "#da8548" :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,purple))))
   `(font-lock-keyword-face ((,class (:foreground "#f7af75" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#789771"))))
   `(font-lock-doc-face ((,class (:foreground "#70a56f"))))
   `(font-lock-type-face ((,class (:foreground "#92a65e" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#aaccff"))))
   `(font-lock-warning-face ((,class (:foreground "#ff6c6b"))))

   ;; Button and link faces
   `(link ((,class (:foreground "#7ac1ff" :underline t))))
   `(link-visited ((,class (:foreground "#aaccff" :underline t))))
   `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   `(header-line ((,class (:background "#202020" :foreground "#f6f3e8"))))
   ;; compilation
   `(compilation-info ((,class (:foreground ,purple :weight bold))))
   `(compilation-line-number ((,class (:foreground ,orange :weight bold))))

   ;; dired
   `(dired-header ((,class (:foreground ,orange :weight bold))))
   `(dired-directory ((,class (:foreground ,purple :weight bold))))

   ;; magit
   `(magit-section-title ((,class (:foreground "#92a65e" :weight bold))))
   `(magit-branch ((,class (:foreground ,orange :weight bold))))
   `(magit-log-sha1 ((,class (:foreground ,purple :weight bold))))
   `(magit-tag ((,class (:foreground "#7ac1ff" :weight bold))))
   `(magit-log-head-label-local ((,class (:foreground ,orange :weight bold :box 1 :background "#202020"))))
   `(magit-log-head-label-remote ((,class (:foreground "#92a65e" :weight bold :box 1 :background "#202020"))))
   `(magit-log-head-label-head ((,class (:foreground "#ff6c6b" :weight bold :box 1 :background "#202020"))))

   ;; ido faces
   `(ido-first-match ((,class (:foreground ,purple :weight bold))))
   `(ido-only-match ((,class (:foreground ,purple :weight bold))))
   `(ido-subdir ((,class (:foreground "#f7af75"))))

   ;; js2-mode
   `(js2-function-param ((,class (:foreground "#7db1cf" :weight bold))))

   ;; org-mode todo WORK IN PROGRESS
   `(org-hide ((,class (:foreground ,background-color))))
   `(org-todo ((,class (:foreground "#ff5b66" :weight bold))))
   `(org-done ((,class (:foreground "#7ac1ff" :weight bold))))
   `(org-headline-done ((,class (:foreground ,default-color))))
   `(outline-1 ((,class (:foreground ,orange))))
   `(outline-2 ((,class (:foreground ,default-color))))
   `(outline-3 ((,class (:foreground "#92a65e"))))
   `(outline-4 ((,class (:foreground "#7db1cf"))))
   `(outline-5 ((,class (:foreground ,orange))))
   `(outline-6 ((,class (:foreground ,default-color))))
   `(outline-7 ((,class (:foreground "#92a65e"))))
   `(outline-8 ((,class (:foreground "#7db1cf"))))
;;   `(org-agenda-date ((,class (:foreground ,purple :background "#202020" :weight bold :box 1))))
   `(org-agenda-date ((,class (:foreground ,purple :weight bold))))
   `(org-agenda-structure ((,class (:foreground ,orange :weight bold))))
   `(org-scheduled-today ((,class (:foreground ,default-color :weight bold))))
   `(org-agenda-done ((,class (:foreground "#929283"))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,orange :weight bold))))
   `(message-header-cc ((,class (:foreground ,purple))))
   `(message-header-other ((,class (:foreground ,purple))))
   `(message-header-subject ((,class (:foreground "#83e1b2"))))
   `(message-header-to ((,class (:foreground ,purple))))
   `(message-cited-text ((,class (:foreground "#99968b"))))
   `(message-separator ((,class (:foreground "#ff6c6b" :weight bold))))

        ;; flyspell
   `(flyspell-incorrect ((,class (:underline (:color "#ff6c6b" :style wave) :weight bold))))
   `(flyspell-duplicate ((,class (:underline (:color "#ff6c6b" :style wave) :weight bold))))

   ;; Gnus faces -- from wombat, feel free to improve :)
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((,class (:foreground "#99968b"))))
   `(gnus-header-content ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground "#cae682"))))
   `(gnus-header-name ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'niflheim)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; niflheim-theme.el ends here
