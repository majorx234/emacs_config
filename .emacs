; start package.el with emacs
(require 'package)
; initialize package.el
(package-initialize)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(dolist (source '(("melpa" . "https://melpa.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
 '(flycheck-googlelint-linelength "120")
 '(flycheck-googlelint-root "project/src")
 '(flycheck-googlelint-verbose "3")
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(which-key ggtags bash-completion yasnippet-snippets flycheck-irony irony haskell-mode auto-complete-c-headers ac-c-headers license-snippets haskell-snippets rainbow-delimiters use-package flymake-cursor smartparens google-c-style flycheck-google-cpplint flymake iedit astyle)))
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
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "Coral"))))
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
 '(tool-bar ((((type x w32 mac) (class color)) (:background "midnight blue" :foreground "wheat" :box (:line-width 1 :style released-button))))))

;;; rainbow-delimitters

(require 'use-package)

;; rainbow brackets
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

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
'(font-lock-variable-name-face ((t (:foreground "blue"))))
'(menu ((((type x-toolkit)) (:background "white" :foreground "black" :box (:line-width 2 :color "grey75" :style released-button)))))
'(modeline ((t (:foreground "blue" :background "white")))))
(set-cursor-color "blue")
(set-foreground-color "white")
(set-background-color "black")
(set-face-foreground 'default "white")
(set-face-background 'default "black"))
(if window-system
(faces_x)
(faces_nox))

;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)

;; Flash instead of that annoying bell
(setq visible-bell t)

;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)

;; bring up help for key bindings
(use-package which-key
:ensure t
:config
(which-key-mode))

;; http://stackoverflow.com/a/20788581/1305501
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)
)

;;hexview: (not working)
;(require 'hexview-mode)
;(global-set-key (kbd "C-x M-h") 'hexview-find-file)

; autosave desktop
(desktop-save-mode 1)

;;; programming part
;;; shortkeys:
(global-set-key (kbd "C-c c")        'comment-region)
(global-set-key (kbd "C-c C-u c")    'uncomment-region)

;;; smart parens:
(require 'smartparens-config)

;; tags for code navigation
(use-package ggtags
:ensure t
:config
(add-hook 'c-mode-common-hook
(lambda ()
(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
(ggtags-mode 1))))
)

;;; c/c++ part
;;cmake
;;cmake,make support (highlighting)
;; Add this code to your .emacs file to use the mode:
;;

(require 'cmake-mode)
(setq auto-mode-alist
    (append '(("CMakeLists\\.txt\\'" . cmake-mode)
              ("\\.cmake\\'" . cmake-mode))
            auto-mode-alist))

(setq-default c-basic-offset 2 c-default-style "linux")
(setq-default tab-width 2 indent-tabs-mode t)

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'set-newline-and-indent)

;start iedit (ok)
(require 'iedit)
(define-key global-map (kbd "C-c o") 'iedit-mode)

;; astyle (ok)
(defun astyle-this-buffer (pmin pmax)
  (interactive "r")
  (shell-command-on-region pmin pmax
                           "astyle --options=/home/ros/.astylerc" ;; add options here...
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*") t))
(global-set-key (kbd "C-c C-y") 'astyle-this-buffer)


;; flycheck
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))
;; /flycheck/: syntax checker
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;;; cpplint

;start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
; start yasnippet with emacs
(require 'yasnippet)
;;(yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
 ; (add-to-list 'achead:include-directories '"/home/major/c_zeuch/cpp11/lambdatest")
  (setq achead:include-directories
      (append '( "/usr/include/c++/10.2.0"
                 "/usr/include/c++/10.2.0/x86_64-pc-linux-gnu"
                 "/usr/include/c++/10.2.0/backward"
                 "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include"
                 "/usr/local/include"
                 "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include-fixed"
                 "/usr/include")
    achead:include-directories))
)
; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;; CEDET stuff (semantic completion)
;;; turn on Semantic
(semantic-mode 1)
;; let's define a function which adds semantic as a suggestion backend to auto complete
;; and hook this function to c-mode-common-hook 
;; ;connect semantic to autocpmplete
(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
; needs to save results of its parsing

;; web javascript
(add-hook 'js-mode-hook #'smartparens-mode)

;; haskell stuff
(require 'haskell-mode)
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (interactive-haskell-mode)
              (flycheck-mode))))

;;; bash completion for shell mode
;;;not work with zsh
(require 'bash-completion)
(bash-completion-setup)


;;; examples of own emacs things (nor used yet)
(defun mypath (name)
	(message "mypath: %s\n" name)
	)

(defun choose-project-directory (directory)
  "Open C/C++ Project"
  (interactive (list (read-directory-name "Project directory? "
                                          choose-directory-default-directory)))
	(mypath directory)
)

(defvar choose-directory-default-directory "/home/ros/c_zeuch"
  "default Project Directory.")

(global-set-key (kbd "C-c C-o") 'choose-project-directory)

