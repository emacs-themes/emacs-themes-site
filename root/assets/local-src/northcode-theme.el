(deftheme northcode
  "Dark theme focused on blue and orange colors.")

(custom-theme-set-variables
 'northcode
 )

(custom-theme-set-faces
 'northcode
 '(default ((t (:inherit nil :stipple nil :background "#1c1c1c" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :family "Inconsolata"))))
 '(font-lock-constant-face ((t (:foreground "#5971B2"))))
 '(font-lock-keyword-face ((t (:foreground "#6E8EE5"))))
 '(font-lock-variable-name-face ((t (:foreground "#CBD4EA"))))
 '(fringe ((t (:background "#1c1c1c" :foreground "#D8DEE9"))))
 '(widget-field ((t (:background "#303030" :foreground "#D8DEE9"))))
 '(font-lock-string-face ((t (:foreground "#C27127"))))
 '(font-lock-type-face ((t (:foreground "#7392E5"))))
 '(font-lock-function-name-face ((t (:foreground "#E9D2AF"))))
 '(custom-variable-tag ((t (:inherit font-lock-keyword-face :weight bold))))
 '(helm-match ((t (:inherit font-lock-variable-name-face))))
 '(helm-candidate-number ((t (:background "#212736" :foreground "#C27127"))))
 '(helm-selection ((t (:background "#E9D2AF" :distant-foreground "black"))))
 '(helm-source-header ((t (:background "#212736" :foreground "white" :weight bold :height 1.3))))
 '(header-line ((t (:inherit mode-line :background "#26314C" :foreground "grey90" :box nil))))
 '(helm-M-x-key ((t (:underline t))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#1e1e1e" :weight light))))
 '(mode-line ((t (:background "#2A2E38" :foreground "#88C0D0"))))
 '(font-lock-comment-face ((t (:foreground "#656C7D"))))
 '(error ((t (:foreground "#993937" :weight bold))))
 '(highlight ((t (:background "#333" :weight bold))))
 '(region ((t (:background "#E9D2AF" :foreground "black"))))
 '(eshell-prompt ((t (:inherit font-lock-keyword-face :weight bold))))
 '(org-todo ((t (:foreground "#B0412A" :weight bold))))
 '(org-done ((t (:foreground "#377D32" :weight bold))))
 '(company-scrollbar-bg ((t (:background "#1e1e1e"))))
 '(company-template-field ((t (:inherit company-tooltip))))
 '(company-tooltip ((t (:background "#2A2E38" :foreground "white"))))
 '(link ((t (:foreground "#4C97E0" :underline t))))
 '(minibuffer-prompt ((t (:foreground "#55C2BE")))))

(provide-theme 'northcode)
