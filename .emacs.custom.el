(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4afa8f2f327994645b2a15b89f8fc950e4eb0adf35c1b35077880ccd9f63b42d" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" default))
 '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
 '(flycheck-googlelint-linelength "120")
 '(flycheck-googlelint-root "project/src")
 '(flycheck-googlelint-verbose "3")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(command-log-mode paredit ido-completing-read+ multiple-cursors smex pyenv-mode rect rect+ gnuplot-mode gnuplot faustine faust-mode hi2 cmake-ide agda-editor-tactics py-snippets yasnippet-classic-snippets zenburn-theme underwater-theme melancholy-theme magit ccls flycheck-rtags company-rtags cmake-mode company-c-headers sr-speedbar dap-mode toml-mode rust-playground rustic eglot cargo cargo-mode flycheck-rust rust-mode js2-mode ecb projectile web-mode elpy flycheck-clang-tidy clang-format which-key ggtags bash-completion yasnippet-snippets flycheck-irony irony haskell-mode auto-complete-c-headers ac-c-headers license-snippets haskell-snippets rainbow-delimiters use-package flymake-cursor smartparens google-c-style flycheck-google-cpplint flymake iedit astyle)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "wheat" :background "black"))))
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
 '(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
 '(font-lock-function-name-face ((t (:foreground "gold"))))
 '(font-lock-keyword-face ((t (:foreground "springgreen"))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "#5c5cff"))))
 '(font-lock-variable-name-face ((t (:foreground "Coral"))))
 '(line-number ((t (:inherit default :background "black" :foreground "grey"))))
 '(menu ((((type x-toolkit)) (:background "light slate gray" :foreground "wheat" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(mode-line ((t (:foreground "black" :background "light slate gray"))))
 '(modeline ((t (:foreground "blue" :background "white"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 1.3))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.2))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.2))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.1))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.1))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.0))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "grey" :height 0.9))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 0.8))))
 '(rustic-compilation-column ((t (:inherit compilation-column-number))))
 '(rustic-compilation-line ((t (:foreground "LimeGreen"))))
 '(tool-bar ((((type x w32 mac) (class color)) (:background "midnight blue" :foreground "wheat" :box (:line-width 1 :style released-button)))))
 '(whitespace-tab ((t (:foreground "#636363")))))

(defun faces_x ()
;; these are used when in X
(custom-set-faces
'(default ((t (:foreground "wheat" :background "black"))))
'(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
'(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
'(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
'(font-lock-function-name-face ((t (:foreground "gold"))))
'(font-lock-keyword-face ((t (:foreground "springgreen"))))
'(font-lock-type-face ((t (:foreground "PaleGreen"))))
'(font-lock-variable-name-face ((t (:foreground "Coral"))))
'(menu ((((type x-toolkit)) (:background "light slate gray" :foreground "wheat" :box (:line-width 2 :color "grey75" :style released-button)))))
'(mode-line ((t (:foreground "black" :background "light slate gray"))))
'(tool-bar ((((type x w32 mac) (class color)) (:background "midnight blue" :foreground "wheat" :box (:line-width 1 :style released-button))))))
(set-cursor-color "deep sky blue")
(set-foreground-color "wheat")
(set-background-color "black")
(set-face-foreground 'default "wheat")
(set-face-background 'default "black"))
(defun faces_nox ()
;; these are used when in terminal
(custom-set-faces
'(default ((t (:foreground "white" :background "black"))))
'(font-lock-comment-face ((t (:foreground "magenta"))))
'(font-lock-function-name-face ((t (:foreground "red"))))
'(font-lock-keyword-face ((t (:foreground "green"))))
'(font-lock-type-face ((t (:foreground "blue"))))
'(font-lock-string-face ((t (:foreground "cyan"))))
'(font-lock-variable-name-face ((t (:foreground "orange"))))
'(menu ((((type x-toolkit)) (:background "white" :foreground "black" :box (:line-width 2 :color "grey75" :style released-button)))))
'(modeline ((t (:foreground "blue" :background "white")))))
(set-cursor-color "blue")
(set-foreground-color "white")
(set-background-color "black")
(set-face-foreground 'default "white")
(set-face-background 'default "black"))
