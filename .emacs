; start package.el with emacs
(require 'package)
; initialize package.el
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

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
	 '(zenburn-theme underwater-theme melancholy-theme magit ccls flycheck-rtags company-rtags cmake-mode company-c-headers sr-speedbar dap-mode toml-mode rust-playground rustic eglot cargo cargo-mode flycheck-rust rust-mode js2-mode ecb projectile web-mode elpy flycheck-clang-tidy clang-format which-key ggtags bash-completion yasnippet-snippets flycheck-irony irony haskell-mode auto-complete-c-headers ac-c-headers license-snippets haskell-snippets rainbow-delimiters use-package flymake-cursor smartparens google-c-style flycheck-google-cpplint flymake iedit astyle)))
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
 ;; brightblue not working in older emacs versions (#5c5cff is hex color value)
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
'(font-lock-variable-name-face ((t (:foreground "orange"))))
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

;;; melancholy theme
(use-package zenburn-theme
  :ensure t	)
(load-theme 'zenburn t)

;;; show line numbers in prog mode:
(defun my-display-numbers-hook ()
  (display-line-numbers-mode t)
	(setq display-line-numbers 'relative)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;;; magit
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)
(with-eval-after-load 'info
	(info-initialize)
	(add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/magit/Documentation/")
)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)
(add-hook 'magit-mode-hook (lambda () (company-mode -1)))

;;; programming part
;;; shortkeys:
(global-set-key (kbd "C-c c")        'comment-region)
(global-set-key (kbd "C-c C-u c")    'uncomment-region)

;;; c/c++ part

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

(use-package lsp-mode
  :ensure
  :commands lsp
;;	:hook (prog-mode . lsp)
	:custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(defun dotfiles--lsp-deferred-if-supported ()
  "Run `lsp-deferred' if it's a supported mode."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (lsp-deferred)))

(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-if-supported)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; ccls
(use-package ccls
  :after projectile
;;  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++11")))

;; flycheck;
(require 'flycheck)
(use-package flycheck
  :ensure t
;;  :init (global-flycheck-mode)
)

;; flycheck
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))

;; /flycheck/: syntax checker
;(add-hook 'c++-mode-hook 'flycheck-mode)
;(add-hook 'c-mode-hook 'flycheck-mode)


;; flycheck: syntax checker rtags:
;; Optional explicitly select the RTags Flycheck checker for c or c++ major mode.
;; Turn off Flycheck highlighting, use the RTags one.
;; Turn off automatic Flycheck syntax checking rtags does this manually.
;(require 'flycheck-rtags)
;(defun my-flycheck-rtags-setup ()
;  "Configure flycheck-rtags for better experience."
;  (flycheck-select-checker 'rtags)
;  (setq-local flycheck-check-syntax-automatically nil)
;  (setq-local flycheck-highlighting-mode nil))
;(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;(add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)

;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
	:after lsp-mode
	:hook (lsp-mode . company-mode)
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
	(:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(use-package company-box
  :hook (company-mode . company-box-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;;; cpplint

;start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;source code navigation:
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
(setq speedbar-show-unknown-files t)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'cc-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/10.2.0/")

; CEDET Stuff
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)
(semantic-add-system-include "/usr/include/boost" 'c++-mode)
(semantic-add-system-include "/usr/include")
; start yasnippet with emacs
(require 'yasnippet)
;;(yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'cmake-mode #'yas-minor-mode)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;;cmake
;;cmake,make support (highlighting)
;; Add this code to your .emacs file to use the mode:
(require 'cmake-mode)
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(require 'rtags) ;; optional, must have rtags installed
(require 'cmake-ide)
(use-package cmake-ide
  :after projectile
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command 
            (concat "cd " cmake-ide-build-dir " && cmake .. && make"))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
  :init (cmake-ide-setup)
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

(setq cmake-ide-flags-c++ (append '("-std=c++14")))

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(rtags-enable-standard-keybindings)

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
              ("C-c C-c h" . lsp-ui-doc-glance)
              ("C-c C-c d" . xref-find-definitions))

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

;flex-pair
;(require 'flex-autopair)
;(flex-autopair-mode 1)

;;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'web-mode) 
(add-hook 'web-mode-hook 'autopair-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;; python stuff
;; Use flycheck-pyflakes for python. Seems to work a little better.
(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook
  ((python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  ;(evil-normal-state-map)
  )

(use-package lsp-ui
 :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
								lsp-ui-doc-delay 0.2
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
)

;;; oldschool (pre lsp) pyflakes
;(require 'flycheck-pyflakes)

;; elpy (now use lsp)
;(elpy-enable)
;(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;(define-key global-map (kbd "C-c o") 'iedit-mode)

(use-package pyvenv
  :ensure t
  :defer t
  :diminish
  :config
	(setenv "WORKON_HOME" "/home/me/chnol3000_framework")
	;(pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals (not used yet)
	; Show python venv name in modeline
	(setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
	(pyvenv-mode t))

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

;;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; autoload
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; org mode

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; org fix for yasnippet:
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))
;; org agende files
(setq org-agenda-files (quote
   ("~/c_zeuch/rust/org/notes/rust_development.org"
    "~/c_zeuch/rust/org/notes/")))

;;; notmuch email
(autoload 'notmuch "notmuch" "Notmuch mail" t)
