;;; nova-theme.el --- A dark, pastel color theme
;;
;; Copyright (C) 2018 Muir Manders
;;
;; Author: Muir Manders <muir+emacs@mnd.rs>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: theme dark nova pastel faces
;; URL: https://github.com/muirmanders/emacs-nova-theme
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Nova is an Emacs color theme using Trevor Miller's Nova color scheme
;; <https://trevordmiller.com/projects/nova>.
;;
;;; Credits:
;;
;; Trevor Miller came up with the color scheme.
;;
;;; Code:

(require 'color)
(require 'cl-lib)

(deftheme nova
  "A dark theme using Trevor Miller's Nova color scheme.
See <https://trevordmiller.com/projects/nova>.")

(defvar nova-base-colors
  '((cyan "#7FC1CA")
    (blue "#83AFE5")
    (purple "#9A93E1")
    (pink "#D18EC2")
    (red "#DF8C8C")
    (orange "#F2C38F")
    (yellow "#DADA93")
    (green "#A8CE93")
    (gray0 "#1E272C")
    (gray1 "#3C4C55")
    (gray2 "#556873")
    (gray3 "#6A7D89")
    (gray4 "#899BA6")
    (gray5 "#C5D4DD")
    (gray6 "#E6EEF3")

    (bg gray1)
    (fg gray5)
    (constant cyan)
    (identifier blue)
    (statement yellow)
    (type green)
    (global purple)
    (emphasis pink)
    (special orange)
    (trivial gray4)

    (user-action-needed red)
    (user-current-state cyan)
    (background-shade gray0)

    (added green)
    (modified orange)
    (removed red)
    (renamed blue)

    ;; non-standard additions
    (variable pink)
    (black (nova-darken bg 0.2))
    (white (nova-lighten fg 0.5))))

;;;###autoload
(defun nova--build-face (face)
  "Internal helper to turn FACE into proper face spec."
  (let ((name (car face)) (attrs (cdr face)))
    `(list ',name (list (list t ,@attrs)))))

;;;###autoload
(defun nova-darken (color alpha)
  "Darken given rgb string COLOR by ALPHA (0-1)."
  (nova-blend "#000000" color alpha))

;;;###autoload
(defmacro nova-with-colors (&rest body)
  "Macro to make color variables available to BODY."
  (declare (indent defun))
  `(let* ,nova-base-colors ,@body))

;;;###autoload
(defun nova-blend (c1 c2 a)
  "Combine A C1 with (1-a) C2."
  (apply
   'color-rgb-to-hex
   (cl-mapcar
    (lambda (c1 c2) (+ (* a c1) (* (- 1 a) c2)))
    (color-name-to-rgb c1)
    (color-name-to-rgb c2))))

;;;###autoload
(defun nova-lighten (color alpha)
  "Lighten given rgb string COLOR by ALPHA (0-1)."
  (nova-blend "#FFFFFF" color alpha))

(defmacro nova-set-faces (&rest faces)
  "Macro for conveniently setting nova faces.
Makes color variables available, and reduces face spec clutter.
FACES is a list of faces of the form (name :attr value) such as:

\(some-important-face :foreground red)"
  (declare (indent defun))
  `(nova-with-colors
     (custom-theme-set-faces
      'nova
      ,@(mapcar #'nova--build-face faces))))

(nova-set-faces
  ;; basic faces (faces.el)
  (default :foreground fg :background bg)
  (cursor :background user-current-state)
  (region :background gray3 :distant-foreground nil)
  (highlight :background user-current-state :foreground bg)
  (fringe :foreground gray3)
  (success :foreground green)
  (warning :foreground yellow)
  (error :foreground user-action-needed)
  (escape-glyph :foreground special)
  (button :foreground constant :inherit 'underline)
  (minibuffer-prompt :foreground cyan)
  (trailing-whitespace :background user-action-needed)
  (show-paren-match :background gray3)
  (show-paren-mismatch :background (nova-darken red 0.4) :foreground red)
  (header-line :background bg)
  (mode-line :box nil :background black :foreground cyan)
  (mode-line-inactive :box nil :background (nova-darken black -0.2) :foreground (nova-darken cyan 0.2))
  (mode-line-buffer-id :weight 'unspecified :foreground white)
  (mode-line-highlight :inherit 'highlight)
  (link :foreground cyan :underline t)
  (vertical-border :foreground trivial)
  (window-divider :inherit 'vertical-border)
  (window-divider-first-pixel :inherit 'vertical-border)
  (window-divider-last-pixel :inherit 'vertical-border)
  (shadow :foreground trivial)

  ;; font lock faces
  (font-lock-function-name-face :foreground identifier)
  (font-lock-constant-face :foreground constant)
  (font-lock-string-face :foreground constant)
  (font-lock-keyword-face :foreground global)
  (font-lock-builtin-face :foreground global)
  (font-lock-variable-name-face :foreground variable)
  (font-lock-type-face :foreground type)
  (font-lock-warning-face :foreground yellow)
  (font-lock-comment-face :foreground trivial)
  (font-lock-negation-char-face :foreground emphasis)

  ;; powerline faces
  (powerline-active0 :background (nova-blend purple bg 0.4) :foreground cyan)
  (powerline-active1 :background black :foreground cyan)
  (powerline-active2 :background gray0 :foreground cyan)
  (powerline-inactive0 :background (nova-blend purple bg 0.2) :foreground (nova-darken cyan 0.2))
  (powerline-inactive1 :background (nova-darken black -0.2) :foreground (nova-darken cyan 0.2))
  (powerline-inactive2 :background (nova-darken gray0 -0.3) :foreground (nova-darken fg 0.2))
  (mode-line-buffer-id-inactive :foreground (nova-darken fg 0.2)) ; doesn't seem to work

  ;; search faces
  (match :background emphasis :foreground bg :distant-foreground bg)
  (isearch :inherit 'match)
  (lazy-highlight :background (nova-darken emphasis 0.3) :foreground bg)

  ;; ivy faces
  (ivy-current-match :background user-current-state :foreground bg)
  (ivy-remote :foreground yellow)
  (ivy-modified-buffer :foreground modified)
  (ivy-highlight-face :foreground emphasis)
  (ivy-match-required-face :foreground user-action-needed)
  (ivy-confirm-face :foreground green)
  (ivy-minibuffer-match-face-2 :background emphasis :foreground bg)
  (ivy-minibuffer-match-face-3 :background orange :foreground bg)
  (ivy-minibuffer-match-face-4 :background green :foreground bg)

  ;; swiper faces
  (swiper-line-face :background user-current-state :foreground bg)
  (swiper-match-face-2 :background emphasis :foreground bg)
  (swiper-match-face-3 :background orange :foreground bg)
  (swiper-match-face-4 :background green :foreground bg)

  ;; hydra faces
  (hydra-face-red :foreground red)
  (hydra-face-amaranth :foreground purple)
  (hydra-face-blue :foreground blue)
  (hydra-face-pink :foreground pink)
  (hydra-face-teal :foreground cyan)

  ;; ido faces
  (ido-first-match :foreground emphasis)
  (ido-indicator :foreground red :background bg)
  (ido-only-match :foreground green)
  (ido-subdir :foreground blue)
  (ido-virtual :foreground trivial)

  ;; magit faces
  (magit-tag :foreground yellow)
  (magit-filename :foreground fg)
  (magit-branch-current :foreground user-current-state :box t)
  (magit-branch-local :foreground user-current-state)
  (magit-branch-remote :foreground green)
  (magit-dimmed :foreground trivial)
  (magit-hash :foreground trivial)
  (magit-process-ng :inherit 'error)
  (magit-process-ok :inherit 'success)
  (magit-section-highlight :background gray2)
  (magit-section-heading :foreground blue)
  (magit-section-heading-selection :foreground cyan :background gray2)
  (magit-section-secondary-heading :foreground purple :weight 'bold)
  (magit-diff-file-heading-selection :foreground cyan :background gray2)
  (magit-diff-hunk-heading :foreground (nova-darken blue 0.1) :background gray2)
  (magit-diff-hunk-heading-highlight :foreground blue :background gray2)
  (magit-diff-lines-heading :background orange :foreground bg)
  (magit-diff-added :foreground added :background (nova-blend added bg 0.1))
  (magit-diff-added-highlight :foreground added :background (nova-blend added bg 0.2))
  (magit-diff-removed :foreground removed :background (nova-blend removed bg 0.1))
  (magit-diff-removed-highlight :foreground removed :background (nova-blend removed bg 0.2))
  (magit-diff-base :foreground yellow :background (nova-blend yellow bg 0.1))
  (magit-diff-base-highlight :foreground yellow :background (nova-blend yellow bg 0.2))
  (magit-diff-context :foreground trivial :background bg)
  (magit-diff-context-highlight :foreground trivial :background bg)
  (magit-diffstat-added :foreground added)
  (magit-diffstat-removed :foreground removed)
  (magit-log-author :foreground pink)
  (magit-log-date :foreground blue)
  (magit-log-graph :foreground trivial)
  (magit-bisect-bad :foreground red)
  (magit-bisect-good :foreground green)
  (magit-bisect-skip :foreground orange)
  (magit-blame-heading  :background black)
  (magit-blame-name :foreground pink :background black)
  (magit-blame-date :foreground blue :background black)
  (magit-blame-summary :foreground cyan :background black)
  (magit-blame-hash :foreground trivial :background black)
  (magit-cherry-equivalent :foreground purple)
  (magit-cherry-unmatched :foreground cyan)
  (magit-reflog-amend :foreground pink)
  (magit-reflog-checkout :foreground blue)
  (magit-reflog-cherry-pick :foreground green)
  (magit-reflog-commit :foreground green)
  (magit-reflog-merge :foreground green)
  (magit-reflog-other :foreground cyan)
  (magit-reflog-rebase :foreground pink)
  (magit-reflog-remote :foreground cyan)
  (magit-reflog-reset :inherit 'error)
  (magit-refname :foreground trivial)
  (magit-sequence-drop :foreground red)
  (magit-sequence-head :foreground (nova-lighten blue 0.2))
  (magit-sequence-part :foreground yellow)
  (magit-sequence-stop :foreground (nova-darken green 0.2))
  (magit-signature-bad :inherit 'error)
  (magit-signature-error :inherit 'error)
  (magit-signature-expired :inherit 'warning)
  (magit-signature-good :inherit 'success)
  (magit-signature-revoked :foreground pink)
  (magit-signature-untrusted :foreground orange)

  ;; vc faces
  (diff-context :inherit 'magit-diff-context)
  (diff-added :inherit 'magit-diff-added-highlight)
  (diff-removed :inherit 'magit-diff-removed-highlight)
  (diff-file-header :inherit 'magit-diff-file-heading)
  (diff-header :inherit 'magit-section)
  (diff-function :foreground identifier)
  (diff-hunk-header :foreground purple)

  ;; ediff faces
  (ediff-current-diff-A :background (nova-blend removed bg 0.3))
  (ediff-current-diff-B :background (nova-blend added bg 0.3))
  (ediff-current-diff-C :background (nova-blend yellow bg 0.3))
  (ediff-current-diff-Ancestor :background (nova-blend blue bg 0.3))
  (ediff-fine-diff-A :background (nova-blend removed bg 0.6))
  (ediff-fine-diff-B :background (nova-blend added bg 0.6))
  (ediff-fine-diff-C :background (nova-blend yellow bg 0.6))
  (ediff-fine-diff-Ancestor :background (nova-blend blue bg 0.6))
  (ediff-even-diff-A :background (nova-blend removed bg 0.2))
  (ediff-even-diff-B :background (nova-blend added bg 0.2))
  (ediff-even-diff-C :background (nova-blend yellow bg 0.2))
  (ediff-even-diff-Ancestor :background (nova-blend blue bg 0.2))
  (ediff-odd-diff-A :background (nova-blend removed bg 0.2))
  (ediff-odd-diff-B :background (nova-blend added bg 0.2))
  (ediff-odd-diff-C :background (nova-blend yellow bg 0.2))
  (ediff-odd-diff-Ancestor :background (nova-blend blue bg 0.2))

  ;; smerge faces
  (smerge-lower :background (nova-blend added bg 0.3))
  (smerge-refined-added :background (nova-blend added bg 0.6))
  (smerge-upper :background (nova-blend removed bg 0.3))
  (smerge-refined-removed :background (nova-blend removed bg 0.6))
  (smerge-base :background (nova-blend yellow bg 0.3))
  (smerge-markers :background gray2)

  ;; rainbow-delimiters faces
  (rainbow-delimiters-depth-1-face :foreground blue)
  (rainbow-delimiters-depth-2-face :foreground pink)
  (rainbow-delimiters-depth-3-face :foreground green)
  (rainbow-delimiters-depth-4-face :foreground orange)
  (rainbow-delimiters-depth-5-face :foreground purple)
  (rainbow-delimiters-depth-6-face :foreground yellow)
  (rainbow-delimiters-depth-7-face :foreground cyan)
  (rainbow-delimiters-unmatched-face :foreground red :background (nova-blend red bg 0.2))

  ;; wgrep faces
  (wgrep-face :background gray2 :foreground 'unspecified)
  (wgrep-delete-face :foreground red :background (nova-blend red bg 0.2))
  (wgrep-done-face :foreground blue)
  (wgrep-file-face :foreground 'unspecified :background gray2)
  (wgrep-reject-face :foreground red :weight 'bold)

  ;; outline faces
  (outline-1 :foreground blue)
  (outline-2 :foreground orange)
  (outline-3 :foreground green)
  (outline-4 :foreground cyan)
  (outline-5 :foreground red )
  (outline-6 :foreground pink)
  (outline-7 :foreground yellow)
  (outline-8 :foreground purple)

  ;; org-mode faces
  (org-hide :foreground bg)
  (org-code :foreground blue :background (nova-blend blue bg 0.2))
  (org-block :inherit 'unspecified :background (nova-blend blue bg 0.2))
  (org-date :inherit 'link)
  (org-footnote :inherit 'link)
  (org-todo :foreground yellow :weight 'bold :box '(:line-width 1) :background (nova-blend yellow bg 0.1))
  (org-done :foreground green :weight 'bold :box '(:line-width 1) :background (nova-blend green bg 0.1))
  (org-table :foreground yellow :background (nova-blend yellow bg 0.1))
  (org-checkbox :background gray2 :box '(:line-width 1 :style released-button))
  (org-formula :foreground yellow)
  (org-sexp-date :foreground cyan)
  (org-scheduled :foreground green)

  (compilation-mode-line-fail :foreground red)
  (compilation-mode-line-exit :foreground green)

  (company-tooltip :foreground bg :background blue)
  (company-tooltip-selection :background purple)
  (company-tooltip-common :background 'unspecified)
  (company-preview :foreground bg)
  (company-preview-common :background gray3 :foreground fg)
  (company-scrollbar-bg :background (nova-darken blue 0.2))
  (company-scrollbar-fg :background (nova-darken blue 0.4))
  (company-template-field :background orange :foreground bg)

  (web-mode-doctype-face :foreground trivial)
  (web-mode-html-tag-face :foreground global)
  (web-mode-html-tag-bracket-face :foreground global)
  (web-mode-html-attr-name-face :foreground identifier)
  (web-mode-block-attr-name-face :foreground constant)
  (web-mode-html-entity-face :foreground cyan :inherit 'italic)
  (web-mode-block-control-face :foreground emphasis)

  (enh-ruby-op-face :inherit 'default)
  (enh-ruby-string-delimiter-face :inherit 'font-lock-string-face)
  (enh-ruby-heredoc-delimiter-face :inherit 'font-lock-string-face)
  (enh-ruby-regexp-delimiter-face :foreground special)
  (enh-ruby-regexp-face :foreground special)

  ;; widget faces
  (widget-field :background gray2)
  (widget-single-line-field :background gray2)
  (widget-documentation :foreground cyan)

  ;; info faces
  (info-menu-star :foreground red)

  ;; customize faces
  (custom-variable-button :foreground green :underline t)
  (custom-variable-tag :foreground pink)
  (custom-group-tag :foreground purple)
  (custom-group-tag-1 :foreground blue)
  (custom-state :foreground yellow :background (nova-blend yellow bg 0.2))
  (custom-saved :foreground green :background (nova-blend green bg 0.2))
  (custom-button :foreground bg :background gray5 :box '(:line-width 2 :style released-button))
  (custom-button-mouse :inherit 'custom-button :background gray6)
  (custom-button-pressed :inherit 'custom-button :box '(:line-width 2 :style pressed-button))
  (custom-button-pressed-unraised :foreground purple)
  (custom-comment :foreground fg :background gray3)
  (custom-comment-tag :foreground trivial)
  (custom-modified :foreground modified :background (nova-blend modified bg 0.2))
  (custom-group-subtitle :foreground orange)
  (custom-variable-obsolete :foreground trivial)
  (custom-set :foreground green :background bg)
  (custom-themed :foreground blue :background bg)
  (custom-invalid :foreground red :background (nova-blend red bg 0.2))
  (custom-changed :foreground orange :background bg)

  ;; dired faces
  (dired-directory :foreground blue)
  (dired-ignored :foreground trivial)
  (dired-flagged :foreground red :background (nova-blend red bg 0.2))
  (dired-header :foreground pink)
  (dired-mark :foreground blue)
  (dired-marked :background gray3)
  (dired-perm-write :foreground red)
  (dired-symlink :foreground orange)
  (dired-warning :inherit 'warning)

  ;; dired+
  ;;
  ;; file faces
  (diredp-dir-heading :foreground pink)
  (diredp-dir-name :foreground blue)
  (diredp-file-name :inherit 'default)
  (diredp-file-suffix :inherit 'default)
  (diredp-compressed-file-name :foreground yellow)
  (diredp-compressed-file-suffix :foreground yellow)
  (diredp-symlink :foreground orange)
  (diredp-autofile-name :foreground pink)
  (diredp-ignored-file-name :foreground (nova-darken red 0.2))

  ;; other column faces
  (diredp-number :foreground yellow)
  (diredp-date-time :foreground cyan)

  ;; priv faces
  (diredp-dir-priv :foreground blue)
  (diredp-read-priv :foreground green)
  (diredp-write-priv :foreground orange)
  (diredp-exec-priv :foreground red)
  (diredp-link-priv :foreground blue)
  (diredp-other-priv :foreground yellow)
  (diredp-rare-priv :foreground pink)
  (diredp-no-priv :foreground fg)

  ;; mark/tag/flag faces
  (diredp-deletion :foreground red :background (nova-blend red bg 0.2))
  (diredp-deletion-file-name :foreground red :background (nova-blend red bg 0.2))
  (diredp-executable-tag :foreground red)
  (diredp-flag-mark :foreground blue :background gray2)
  (diredp-flag-mark-line :background gray2)
  (diredp-mode-line-marked :foreground blue)
  (diredp-mode-line-flagged :foreground red)

  ;; eshell faces
  (eshell-prompt :foreground cyan)
  (eshell-ls-archive :inherit 'diredp-compressed-file-name)
  (eshell-ls-backup :foreground trivial)
  (eshell-ls-clutter :foreground trivial)
  (eshell-ls-directory :inherit 'diredp-dir-name)
  (eshell-ls-executable :foreground red)
  (eshell-ls-missing :foreground red)
  (eshell-ls-product :foreground orange)
  (eshell-ls-readonly :foreground yellow)
  (eshell-ls-special :foreground pink)
  (eshell-ls-symlink :foreground cyan)
  (eshell-ls-unreadable :foreground orange)

  ;; term faces
  (term :foreground fg)
  (term-bold :weight 'bold)
  (term-color-black :background gray0 :foreground gray0)
  (term-color-red :background red :foreground red)
  (term-color-green :background green :foreground green)
  (term-color-yellow :background yellow :foreground yellow)
  (term-color-blue :background blue :foreground blue)
  (term-color-magenta :background pink :foreground pink)
  (term-color-cyan :background cyan :foreground cyan)
  (term-color-white :background white :foreground white)

  ;; sh-mode faces
  (sh-heredoc :inherit 'font-lock-string-face :weight 'unspecified)
  (sh-quoted-exec :foreground yellow)
  (sh-escaped-newline :foreground special)

  ;; flyspell faces
  (flyspell-incorrect :underline `(:style wave :color ,red))
  (flyspell-duplicate :underline `(:style wave :color ,orange))

  ;; cperl-mode faces
  (cperl-hash-face :foreground red :background 'unspecified)
  (cperl-array-face :foreground yellow :background 'unspecified)
  (cperl-nonoverridable-face :inherit 'font-lock-builtin-face)

  (js2-warning :underline yellow)
  (js2-error :underline user-action-needed)
  (js2-function-param :foreground variable)
  (js2-function-call :foreground identifier)
  (js2-jsdoc-tag :foreground trivial)
  (js2-jdsoc-type :foreground type)
  (js2-jdoc-value :foreground identifier)
  (js2-external-variable :foreground special))

(nova-with-colors
  (custom-theme-set-variables
   'nova
   `(frame-background-mode 'dark)
   `(vc-annotate-background ,bg)
   `(vc-annotate-very-old-color ,(nova-darken purple 0.2))
   `(vc-annotate-color-map
     `((20 . ,,red)
       (40 . ,,(nova-blend red orange 0.8))
       (60 . ,,(nova-blend red orange 0.6))
       (80 . ,,(nova-blend red orange 0.4))
       (100 . ,,(nova-blend red orange 0.2))
       (120 . ,,orange)
       (140 . ,,(nova-blend orange yellow 0.8))
       (160 . ,,(nova-blend orange yellow 0.6))
       (180 . ,,(nova-blend orange yellow 0.4))
       (200 . ,,(nova-blend orange yellow 0.2))
       (220 . ,,yellow)
       (240 . ,,(nova-blend yellow green 0.8))
       (260 . ,,(nova-blend yellow green 0.6))
       (280 . ,,(nova-blend yellow green 0.4))
       (300 . ,,(nova-blend yellow green 0.2))
       (320 . ,,green)
       (340 . ,,(nova-blend green blue 0.8))
       (360 . ,,(nova-blend green blue 0.6))
       (380 . ,,(nova-blend green blue 0.4))
       (400 . ,,(nova-blend green blue 0.2))
       (420 . ,,blue)
       (440 . ,,(nova-blend blue purple 0.8))
       (460 . ,,(nova-blend blue purple 0.6))
       (480 . ,,(nova-blend blue purple 0.4))
       (500 . ,,(nova-blend blue purple 0.2))
       (520 . ,,purple)))))

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list
   'custom-theme-load-path
   (file-name-directory load-file-name)))

(provide-theme 'nova)

;;; nova-theme.el ends here
