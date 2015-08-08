;;; stekene-theme-common.el --- Common stuff for the stekene themes -*- lexical-binding: t -*-

;; Author: Fanael Linithien &lt;fanael4@gmail.com&gt;
;; URL: https://github.com/Fanael/stekene-theme

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(defmacro stekene-theme--set-faces (name palette)
  `(let ,(cons '(class '((class color) (min-colors 89))) palette)
     (custom-theme-set-faces
      ',name
      `(default ((,class (:background ,background :foreground ,foreground))))
      `(cursor ((,class (:background ,foreground))))
      `(region ((,class (:background ,region))))
      `(highlight ((,class (:background ,highlight))))
      `(font-lock-builtin-face ((,class (:foreground ,blue3))))
      `(font-lock-preprocessor-face ((,class (:foreground ,dullred))))
      `(font-lock-comment-face ((,class (:foreground ,gray1))))
      `(font-lock-constant-face ((,class (:foreground ,dullyellow))))
      `(font-lock-function-name-face ((,class (:foreground ,blue1))))
      `(font-lock-keyword-face ((,class (:foreground ,gray2))))
      `(font-lock-string-face ((,class (:foreground ,red))))
      `(font-lock-regexp-grouping-backslash ((,class (:foreground ,orange2))))
      `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow2))))
      `(font-lock-doc-face ((,class (:foreground ,orange1))))
      `(font-lock-type-face ((,class (:foreground ,dullgreen))))
      `(font-lock-variable-name-face ((,class (:foreground ,blue2))))
      `(font-lock-negation-char-face ((,class (:foreground ,orange2))))

      `(hl-line ((,class (:background ,hlline))))
      `(show-paren-match-face ((,class (:background ,region))))

      `(whitespace-line ((,class (:background ,whitespaceline :foreground nil))))
      `(whitespace-trailing ((,class (:background ,whitespacetrailing :foreground nil))))

      `(fringe ((,class (:background ,fringebg))))
      `(linum ((,class (:background ,background :foreground ,gray1))))

      `(mode-line ((,class
                    (:background ,modelinebg :foreground ,foreground :box nil))))

      `(minibuffer-prompt ((,class (:foreground ,orange1))))

      `(ido-subdir ((,class (:foreground ,yellow1))))
      `(ido-only-match ((,class (:foreground ,blue2))))

      `(evil-ex-info ((,class (:foreground ,red :weight bold))))
      `(evil-ex-substitute-replacement ((,class
                                         (:foreground ,red :weight bold :underline t))))

      `(rainbow-identifiers-identifier-1 ((,class (:foreground ,symbol1))))
      `(rainbow-identifiers-identifier-2 ((,class (:foreground ,symbol2))))
      `(rainbow-identifiers-identifier-3 ((,class (:foreground ,symbol3))))
      `(rainbow-identifiers-identifier-4 ((,class (:foreground ,symbol4))))
      `(rainbow-identifiers-identifier-5 ((,class (:foreground ,symbol5))))
      `(rainbow-identifiers-identifier-6 ((,class (:foreground ,symbol6))))
      `(rainbow-identifiers-identifier-7 ((,class (:foreground ,symbol7))))
      `(rainbow-identifiers-identifier-8 ((,class (:foreground ,symbol8))))
      `(rainbow-identifiers-identifier-9 ((,class (:foreground ,symbol9))))
      `(rainbow-identifiers-identifier-10 ((,class (:foreground ,symbol10))))
      `(rainbow-identifiers-identifier-11 ((,class (:foreground ,symbol11))))
      `(rainbow-identifiers-identifier-12 ((,class (:foreground ,symbol12))))
      `(rainbow-identifiers-identifier-13 ((,class (:foreground ,symbol13))))
      `(rainbow-identifiers-identifier-14 ((,class (:foreground ,symbol14))))
      `(rainbow-identifiers-identifier-15 ((,class (:foreground ,symbol15))))

      `(rainbow-delimiters-depth-1-face ((,class (:foreground ,delim1))))
      `(rainbow-delimiters-depth-2-face ((,class (:foreground ,delim2))))
      `(rainbow-delimiters-depth-3-face ((,class (:foreground ,delim3))))
      `(rainbow-delimiters-depth-4-face ((,class (:foreground ,delim4))))
      `(rainbow-delimiters-depth-5-face ((,class (:foreground ,delim5))))
      `(rainbow-delimiters-depth-6-face ((,class (:foreground ,delim6))))
      `(rainbow-delimiters-depth-7-face ((,class (:foreground ,delim7))))
      `(rainbow-delimiters-depth-8-face ((,class (:foreground ,delim8))))
      `(rainbow-delimiters-depth-9-face ((,class (:foreground ,delim9))))

      `(highlight-blocks-depth-1-face ((,class (:background ,block1))))
      `(highlight-blocks-depth-2-face ((,class (:background ,block2))))
      `(highlight-blocks-depth-3-face ((,class (:background ,block3))))
      `(highlight-blocks-depth-4-face ((,class (:background ,block4))))
      `(highlight-blocks-depth-5-face ((,class (:background ,block5))))
      `(highlight-blocks-depth-6-face ((,class (:background ,block6))))
      `(highlight-blocks-depth-7-face ((,class (:background ,block7))))
      `(highlight-blocks-depth-8-face ((,class (:background ,block8))))
      `(highlight-blocks-depth-9-face ((,class (:background ,block9)))))
     (custom-theme-set-variables
      ',name
      `(rainbow-identifiers-cie-l*a*b*-lightness ,delimlightness)
      `(rainbow-identifiers-cie-l*a*b*-saturation ,delimsaturation))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'stekene-theme-common)
;;; stekene-theme-common.el ends here

<pre class="lang:lisp decode:true " title="Dark">;;; stekene-dark-theme.el --- The dark version of the stekene theme -*- lexical-binding: t -*-

;; Author: Fanael Linithien &lt;fanael4@gmail.com&gt;
;; URL: https://github.com/Fanael/stekene-theme

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(eval-when-compile (require 'stekene-theme-common))

(deftheme stekene-dark "The dark version of the stekene theme.")

(stekene-theme--set-faces
 stekene-dark
 ((foreground "#e0e0e0")
  (background "#242424")
  (region "#4f4f4f")
  (hlline "#333333")
  (highlight "#224422")
  (modelinebg "#3f3f3f")
  (gray1 "#777777")
  (gray2 "#a0a0a0")
  (dullgreen "#99b099")
  (dullred "#b79999")
  (dullyellow "#b4b499")
  (red "#ff9090")
  (orange1 "#ffa090")
  (orange2 "#ffbb90")
  (yellow1 "#eed599")
  (yellow2 "#ffee90")
  (blue1 "#99b4c4")
  (blue2 "#8894a4")
  (blue3 "#9999c8")
  (fringebg "#1a1a1a")
  (whitespaceline "#64231f")
  (whitespacetrailing "#94332f")
  (symbol1 "#edb9b8")
  (symbol2 "#e9bcab")
  (symbol3 "#dfc1a3")
  (symbol4 "#d1c6a1")
  (symbol5 "#c0caa5")
  (symbol6 "#afceaf")
  (symbol7 "#a0d0bd")
  (symbol8 "#96d1cd")
  (symbol9 "#94d0db")
  (symbol10 "#9ccde6")
  (symbol11 "#acc9eb")
  (symbol12 "#c0c4e9")
  (symbol13 "#d2bfe2")
  (symbol14 "#e2bbd5")
  (symbol15 "#ebb9c7")
  (delim1 "#b8968d")
  (delim2 "#ac9b83")
  (delim3 "#99a086")
  (delim4 "#85a494")
  (delim5 "#7aa5a6")
  (delim6 "#80a2b4")
  (delim7 "#949db7")
  (delim8 "#aa97af")
  (delim9 "#b8949e")
  (delimlightness 80)
  (delimsaturation 18)
  (block1 "#242a24")
  (block2 "#27272d")
  (block3 "#302a2a")
  (block4 "#2d332d")
  (block5 "#303036")
  (block6 "#393333")
  (block7 "#363c36")
  (block8 "#39393f")
  (block9 "#423c3c")))

(provide-theme 'stekene-dark)
;;; stekene-dark-theme.el ends here</pre>
&nbsp;
<pre class="lang:lisp decode:true " title="Light">;;; stekene-light-theme.el --- The light version of the stekene theme -*- lexical-binding: t -*-

;; Author: Fanael Linithien &lt;fanael4@gmail.com&gt;
;; URL: https://github.com/Fanael/stekene-theme

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(eval-when-compile (require 'stekene-theme-common))

(deftheme stekene-light "The light version of the stekene theme.")

(stekene-theme--set-faces
 stekene-light
 ((foreground "#242424")
  (background "#f8f8f8")
  (region "#bbbbbb")
  (hlline "#dddddd")
  (highlight "#aaccaa")
  (modelinebg "#dddddd")
  (gray1 "#919191")
  (gray2 "#666666")
  (dullgreen "#557755")
  (dullred "#775555")
  (dullyellow "#777755")
  (red "#e83333")
  (orange1 "#e85533")
  (orange2 "#b86833")
  (yellow1 "#777722")
  (yellow2 "#777722")
  (blue1 "#336688")
  (blue2 "#666699")
  (blue3 "#555588")
  (fringebg "#dddddd")
  (whitespaceline "#fac9c0")
  (whitespacetrailing "#fa8980")
  (symbol1 "#934748")
  (symbol2 "#8b4e34")
  (symbol3 "#7d5626")
  (symbol4 "#695e22")
  (symbol5 "#51642a")
  (symbol6 "#35693b")
  (symbol7 "#006b51")
  (symbol8 "#006c69")
  (symbol9 "#006b7f")
  (symbol10 "#00688f")
  (symbol11 "#0d6396")
  (symbol12 "#4c5b94")
  (symbol13 "#6f5288")
  (symbol14 "#854976")
  (symbol15 "#91455f")
  (delim1 "#7c544a")
  (delim2 "#6e5b3e")
  (delim3 "#586241")
  (delim4 "#3f6652")
  (delim5 "#2a6768")
  (delim6 "#32647a")
  (delim7 "#505d7e")
  (delim8 "#6d5673")
  (delim9 "#7d525e")
  (delimlightness 35)
  (delimsaturation 40)
  (block1 "#f8ebeb")
  (block2 "#ebf8eb")
  (block3 "#ebebf8")
  (block4 "#f8f8eb")
  (block5 "#ebf8f8")
  (block6 "#f8ebf8")
  (block7 "#e8dada")
  (block8 "#dae8da")
  (block9 "#dadae8")))

(provide-theme 'stekene-light)
;;; stekene-light-theme.el ends here</pre>
&nbsp;