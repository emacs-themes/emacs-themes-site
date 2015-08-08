;;   "Color theme by dngpng, created 2007-09-11."
;; Originally, designed for color-theme.el, this is a port to
;; emacs deftheme framework.  Although the colors are the same,
;; it has been heavily reorganized
(deftheme sunburst "The Sunburst Color Theme")

(custom-theme-set-faces
 'sunburst

 ;; emacs
 '(cursor ((t (:background "yellow"))))
 '(default ((t (:background "#111" :foreground "#ddd"))))
 '(mode-line ((t (:background "#e6e5e4" :foreground "black"))))
 '(highlight ((t (:bold t :slant italic))))
 '(gui-element ((t (:background "#0e2231" :foreground "black"))))
 '(blue ((t (:foreground "blue"))))
 '(italic ((t (nil))))
 '(left-margin ((t (nil))))
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:bold t :slant italic))))
 '(border-glyph ((t (nil))))
 '(underline ((nil (:underline nil))))
 '(primary-selection ((t (:background "#222"))))
 '(border-coler ((t (:background "#111"))))
 '(region ((t (:background "#4a410d"))))

 ;; font-lock
 '(font-lock-builtin-face ((t (:foreground "#dd7b3b"))))
 '(font-lock-builtin-face ((t (:foreground "#dd7b3b"))))
 '(font-lock-comment-face ((t (:foreground "#666" ))))
 '(font-lock-constant-face ((t (:foreground "#99cf50"))))
 '(font-lock-doc-string-face ((t (:foreground "#9b859d"))))
 '(font-lock-function-name-face ((t (:foreground "#e9c062" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "#cf6a4c" :bold t))))
 '(font-lock-preprocessor-face ((t (:foreground "#aeaeae"))))
 '(font-lock-reference-face ((t (:foreground "#8b98ab"))))
 '(font-lock-string-face ((t (:foreground "#65b042"))))
 '(font-lock-type-face ((t (:foreground "#c5af75"))))
 '(font-lock-variable-name-face ((t (:foreground "#3387cc"))))
 '(font-lock-warning-face ((t (:bold t :background "#420e09" :foreground "#eeeeee"))))

 ;; erc
 '(erc-current-nick-face ((t (:foreground "#aeaeae"))))
 '(erc-default-face ((t (:foreground "#ddd"))))
 '(erc-keyword-face ((t (:foreground "#cf6a4c"))))
 '(erc-notice-face ((t (:foreground "#666"))))
 '(erc-timestamp-face ((t (:foreground "#65b042"))))
 '(erc-underline-face ((t (:foreground "#c5af75"))))

 ;; nxml
 '(nxml-attribute-local-name-face ((t (:foreground "#3387cc"))))
 '(nxml-attribute-colon-face ((t (:foreground "#e28964"))))
 '(nxml-attribute-prefix-face ((t (:foreground "#cf6a4c"))))
 '(nxml-attribute-value-face ((t (:foreground "#65b042"))))
 '(nxml-attribute-value-delimiter-face ((t (:foreground "#99cf50"))))
 '(nxml-namespace-attribute-prefix-face ((t (:foreground "#9b859d"))))
 '(nxml-comment-content-face ((t (:foreground "#666"))))
 '(nxml-comment-delimiter-face ((t (:foreground "#333"))))
 '(nxml-element-local-name-face ((t (:foreground "#e9c062"))))
 '(nxml-markup-declaration-delimiter-face ((t (:foreground "#aeaeae"))))
 '(nxml-namespace-attribute-xmlns-face ((t (:foreground "#8b98ab"))))
 '(nxml-prolog-keyword-face ((t (:foreground "#c5af75"))))
 '(nxml-prolog-literal-content-face ((t (:foreground "#dad085"))))
 '(nxml-tag-delimiter-face ((t (:foreground "#cda869"))))
 '(nxml-tag-slash-face ((t (:foreground "#cda869"))))
 '(nxml-text-face ((t (:foreground "#ddd"))))

 ;; jabber
 '(jabber-chat-prompt-local ((t (:foreground "#65b042"))))
 '(jabber-chat-prompt-foreign ((t(:foreground "#3387cc"))))
 '(jabber-roster-user-xa ((t (:foreground "#e28964"))))
 '(jabber-roster-user-online ((t (:foreground "#3387cc"))))
 '(jabber-roster-user-away ((t (:foreground "#9b859d"))))

 ;; magit
 '(magit-log-sha1 ((t (:foreground "#cf6a4c"))))
 '(magit-log-head-label-local ((t (:foreground "#3387cc"))))
 '(magit-log-head-label-remote ((t (:foreground "#65b042"))))
 '(magit-branch ((t (:foreground "#e9c062"))))
 '(magit-section-title ((t (:foreground "#9b859d"))))
 '(magit-item-highlight ((t (:background "#1f1f1f"))))

 ;; highline
 '(highline-face ((t (:background "#4a410d"))))

 ;; MultiMode
 '(mmm-default-submode-face ((t (:background "#111"))))
 )

(provide-theme 'sunburst)
