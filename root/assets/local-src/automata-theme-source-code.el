(require 'autothemer)

(autothemer-deftheme
 automata "A refined personal theme for Emacs/automata/vim/dwm/bemenu roughly based on NieR:Automata's colors."

 ((((class color) (min-colors #xFFFFFF)))  ;; only use in graphical Emacs

  ;; define our color palette
  (automata-grey "#bab5a1")
  (automata-orange "#ce664d")
  (automata-dark-grey "#898776")
  (automata-brown "#877861")
  (automata-dark-brown "#454138")
  (automata-indigo "#2a0d83")
  (automata-second-grey "#7a766f")
  (automata-light-yellow "#ece2b1")
  (automata-gold "#af5f00")
  (white "#ffffff")
  )

 ;; customize faces
 ((default (:foreground automata-dark-brown :background automata-grey))
  (cursor (:background automata-dark-brown))
  (font-lock-preprocessor-face (:foreground automata-dark-brown))
  (font-lock-string-face (:foreground automata-orange))
  (font-lock-type-face (:foreground automata-dark-brown))
  (font-lock-function-name-face (:foreground automata-dark-brown))
  (font-lock-keyword-face (:foreground automata-gold))
  (font-lock-variable-name-face (:foreground automata-dark-brown))
  (font-lock-constant-face (:foreground automata-orange))
  (font-lock-comment-face (:foreground automata-second-grey))
  (mode-line (:foreground automata-grey :background automata-dark-brown))
  (mode-line-inactive (:foreground automata-dark-brown :background automata-second-grey))
  (fringe (:background automata-grey))
  (help-key-binding (:foreground automata-grey :background automata-dark-brown))
  (region (:foreground automata-grey :background automata-dark-brown))
  (line-number (:foreground automata-gold))
  (line-number-current-line (:foreground automata-gold))
  (minibuffer-prompt (:foreground automata-indigo))
  (isearch (:foreground white :background automata-gold))
  (ivy-current-match (:foreground white :background automata-indigo))
  (ivy-prompt-match (:foreground white :background automata-indigo))
  (ivy-minibuffer-match-face-2 (:foreground white :background automata-gold))
  (ivy-minibuffer-match-face-4 (:foreground white :background automata-gold))
  (ivy-subdir (:foreground white :background automata-indigo))
  (swiper-line-face (:foreground white :background automata-indigo))
  (swiper-match-face-1 (:foreground automata-dark-brown :background automata-light-yellow))
  (swiper-match-face-3 (:foreground white :background automata-orange))
  ))

(provide-theme 'automata)
