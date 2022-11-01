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

;;; show line numbers in prog mode:
(defun my-display-numbers-hook ()
  (display-line-numbers-mode t)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;;; magit
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)
(with-eval-after-load 'info
	(info-initialize)
	(add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/magit/Documentation/")
	)

(add-hook 'magit-mode-hook (lambda () (company-mode -1)))

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

;; clang-format
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

;;; project managment
(require 'projectile)
(projectile-global-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
(add-hook 'cmake-mode #'yas-minor-mode)

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

;; haskell stuff
(require 'haskell-mode)
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (interactive-haskell-mode)
              (flycheck-mode))))

;;; Rust stuff
(with-eval-after-load 'rust-mode
  ; remove rust-mode from the list of file extension mappings
  (setq auto-mode-alist (rassq-delete-all 'rust-mode auto-mode-alist))
  )
(add-hook 'rust-mode-hook
          (lambda () (error "Don't use rust-mode, use rustic")
						))

(use-package rustic
	:ensure
	:bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))
;; for rust-analyzer integration


;; Create / cleanup rust scratch projects quickly
(use-package rust-playground :ensure)


;; for Cargo.toml and other config files
(use-package toml-mode :ensure)

;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
     ))))

;;; web stuff
(require 'web-mode) 
(add-hook 'web-mode-hook 'autopair-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
 
(windmove-default-keybindings)

;;; smart parens:
(require 'smartparens-config)
;; web javascript
(add-hook 'js-mode-hook #'smartparens-mode)

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;;js2-mode steals TAB, let's steal it back for yasnippet
(defun js2-tab-properly ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    (unless (yas/expand)
      (indent-for-tab-command)
      (if (looking-back "^\s*")
          (back-to-indentation)))))
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "TAB") 'js2-tab-properly))

(add-hook 'js2-mode-hook 'autopair-mode)
(set-cursor-color "#aaaaaa")

;;; bash completion for shell mode
;;;not work with zsh
(require 'bash-completion)
(bash-completion-setup)

;;; Fixmee
(require 'fixmee)
(require 'button-lock)

(global-fixmee-mode 1)

;;; examples of own emacs things (not used yet)
(defun mypath (name)
	(message "mypath: %s\n" name)
	)

(defun open_my_cproject (directory)
	(setq projectdir (concat "-I" directory "/include"))
	(mypath projectdir)
  ((nil . ((company-clang-arguments . projectdir))))
  )

(defun choose-project-directory (directory)
  "Open C/C++ Project"
  (interactive (list (read-directory-name "Project directory? "
                                          choose-directory-default-directory)))
	(open_my_cproject directory)
)

(defvar choose-directory-default-directory "/home/ros/c_zeuch"
  "default Project Directory.")

(global-set-key (kbd "C-c C-o") 'choose-project-directory)

(put 'scroll-left 'disabled nil)
