(setq custom-file "~/.emacs.custom.el")

(load-file custom-file)
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

(require 'use-package)

;;; general config
(setq inhibit-startup-message t)
;;; disable scrollbars & menu bar
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

;(menu-bar-mode -1)            ; Disable the menu bar

(show-paren-mode 1)

;; Set up the visible bell
(setq visible-bell t)

;;; set major mode to text mode
;(setq-default major-mode 'text-mode)
; set eval lisp stuff
(global-set-key (kbd "C-c j") 'eval-print-last-sexp)

;; Set up the visible bell
(setq visible-bell t)

;; rainbow brackets
(use-package rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

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

;;; highlight current line
(global-hl-line-mode +1)

;;; switch lines up/down
;;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)


;;;hexview: (not working)
; (require 'hexview-mode)
; (global-set-key (kbd "C-x M-h") 'hexview-find-file)

;;; autosave desktop
(desktop-save-mode 1)

;;; melancholy theme
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)

;;; show line numbers in prog mode:
(defun my-display-numbers-hook ()
  (display-line-numbers-mode t)
  (setq display-line-numbers 'relative)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

(require 'command-log-mode)
(use-package command-log-mode)
(add-hook 'LaTeX-mode-hook 'command-log-mode)

;;; magit
(use-package magit :ensure)
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

;;; Paredit
(use-package paredit :ensure)

(defun turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook  'turn-on-paredit)
(add-hook 'clojure-mode-hook     'turn-on-paredit)
(add-hook 'lisp-mode-hook        'turn-on-paredit)
(add-hook 'common-lisp-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook      'turn-on-paredit)
(add-hook 'racket-mode-hook      'turn-on-paredit)

(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'rc/duplicate-line)

;;; c/c++ part
(setq-default c-basic-offset 2 c-default-style "linux")
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'set-newline-and-indent)

;start iedit (ok)
(use-package iedit :ensure)
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
(use-package clang-format :ensure)
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

;; rmsbolt

(use-package rmsbolt ;;compiler explorer in emacs
  :ensure t
  ;; rmsbolt changes keybinding C-c C-c, which is bonded to comment code.
  ;; :bind (:map rmsbolt-mode-map ("C-c C-c" . rmsbolt-compile))
  :hook
  ;;rmsbolt does not support tree-sitter. We have to manually set it, coping from
  ;;rmsbolt.el
  (rmsbolt-mode . (lambda ()
                    (cond ((eq major-mode 'c-mode)
                           (setq rmsbolt-language-descriptor
                                 (make-rmsbolt-lang :compile-cmd "gcc"
                                                    :supports-asm t
                                                    :supports-disass t
                                                    :demangler "c++filt"
                                                    :compile-cmd-function #'rmsbolt--c-compile-cmd
                                                    :disass-hidden-funcs
                                                    rmsbolt--hidden-func-c)))
                          ((eq major-mode 'c++-mode)
                           (setq rmsbolt-language-descriptor
                                 (make-rmsbolt-lang :compile-cmd "g++"
                                                    :supports-asm t
                                                    :supports-disass t
                                                    :demangler "c++filt"
                                                    :compile-cmd-function #'rmsbolt--c-compile-cmd
                                                    :disass-hidden-funcs rmsbolt--hidden-func-c)))
                          ) ;;cond
                    ;;TODO adding GLSL/HLSL languages?
                    )) ;;rmsbolt-mode-hook
  )

;;; project managment
(use-package projectile :ensure)
(require 'projectile)
(projectile-global-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package lsp-mode
  :ensure
  :commands lsp
;;  :hook (prog-mode . lsp)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'php-mode 'lsp)
  (add-hook 'css-mode 'lsp)
  (add-hook 'web-mode 'lsp))

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

;; flycheck;
(use-package flycheck :ensure
;;  :init (global-flycheck-mode)
)
(require 'flycheck)

;; flycheck
(use-package flycheck-google-cpplint :ensure)
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

;; ccls
(use-package ccls
  :ensure
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

;; Clang stuff
(require 'clang-format)
(setq clang-format-style "file")

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :ensure t
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
  :ensure
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
(use-package google-c-style :ensure)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;source code navigation:
(use-package ggtags :ensure)
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

(use-package company-c-headers :ensure)
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
(use-package yasnippet :ensure)
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
(use-package cmake-mode
  :ensure
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(require 'cmake-mode)
(use-package cmake-font-lock
  :ensure
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package rtags :ensure)
(require 'rtags) ;; optional, must have rtags installed

(use-package cmake-ide
  :ensure
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
(require 'cmake-ide)
(setq cmake-ide-flags-c++ (append '("-std=c++14")))

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(rtags-enable-standard-keybindings)

;; haskell stuff
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (interactive-haskell-mode)
              (flycheck-mode))))
(require 'haskell-mode)

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
(use-package web-mode :ensure)
(require 'web-mode)
(add-hook 'web-mode-hook 'autopair-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

(windmove-default-keybindings)

;;; smart parens:
;;(use-package smartparens-config)
;;(require 'smartparens-config)
;; web javascript
(add-hook 'js-mode-hook #'smartparens-mode)

(add-hook 'js-mode-hook (lambda ()
                          (setq js-indent-level 4
                                indent-tabs-mode nil)))
;; javascript
(use-package js2-mode :ensure)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; js2 indentation
(add-hook 'js2-mode-hook (lambda ()
                             (setq js2-basic-offset 4
                                   indent-tabs-mode nil)))
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
(use-package flx-ido :ensure)
(require 'flx-ido)
(use-package ido-completing-read+ :ensure)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'web-mode)
(add-hook 'web-mode-hook 'autopair-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

(use-package yaml-mode :ensure)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;; python stuff
;; Use flycheck-pyflakes for python. Seems to work a little better.
(use-package lsp-mode
  :ensure
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
  :ensure
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
  (setenv "WORKON_HOME" "/home/ros/c_zeuch/python/python_data_analysis")
  ;(pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals (not used yet)
  ; Show python venv name in modeline
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode t))

;;(require 'pyenv-mode)
;; alternative to pyvenv

;;; bash completion for shell mode
;;;not work with zsh
(use-package bash-completion :ensure)
(require 'bash-completion)
(bash-completion-setup)

;;; Tramp mode
(setq tramp-default-method "ssh")

;;; Fixmee
(use-package fixmee :ensure)
(require 'fixmee)
(use-package button-lock :ensure)
(require 'button-lock)

(global-fixmee-mode 1)

(use-package smex :ensure)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package multiple-cursors :ensure)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

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
(put 'downcase-region 'disabled nil)


;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))

(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
; END TABS CONFIG

; start yasnippet with emacs
(require 'yasnippet)
;;(yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'cmake-mode #'yas-minor-mode)

;;;faust stuff
(use-package faust-mode :ensure)
(require 'faust-mode)

(use-package faustine :ensure)
(require 'faustine)
(add-to-list 'auto-mode-alist
             '("\\.dsp\\'" . faustine-mode))

;; rect+
(use-package rect+ :ensure)
(require 'rect+)
(define-key ctl-x-r-map "C" 'rectplus-copy-rectangle)
(define-key ctl-x-r-map "N" 'rectplus-insert-number-rectangle)
(define-key ctl-x-r-map "\M-c" 'rectplus-create-rectangle-by-regexp)
(define-key ctl-x-r-map "A" 'rectplus-append-rectangle-to-eol)
(define-key ctl-x-r-map "R" 'rectplus-kill-ring-to-rectangle)
(define-key ctl-x-r-map "K" 'rectplus-rectangle-to-kill-ring)
(define-key ctl-x-r-map "\M-l" 'rectplus-downcase-rectangle)
(define-key ctl-x-r-map "\M-u" 'rectplus-upcase-rectangle)

;; gnuplot
(autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
(defun gnuplot-rectangle (&optional title style)
  (interactive)
  (let* ((name (make-temp-file "plot"))
         (buf (find-file name))
         xlabel ylabel cols header n)
    (with-current-buffer buf
      (setq cols (split-string (car killed-rectangle)))
      (when (string-match-p "^[a-zA-Z]" (car cols))
        (setq header cols)
        (pop killed-rectangle))
      (setq n (length header))
      (pcase n
        (1 (setq ylabel (nth 1 header)))
        (2 (setq xlabel (nth 0 header)
                 ylabel (nth 1 header)))
        (_ (setq xlabel (nth 0 header))))
      (yank-rectangle)
      (save-buffer))

    (setq style (or style "line"))
    (with-temp-buffer
      (insert "set title  '" (or title "Title") "'\n")
      (insert "set xlabel '" (or xlabel "x-axis") "'\n")
      (insert "set ylabel '" (or ylabel "y-axis") "'\n")
      (insert "plot '" name "'")
      (setq n (length cols))
      (dotimes (i (1- n))
        (if (> n 1) (insert " using 1:" (number-to-string (+ i 2))))
        (if (< i n) (insert " with " style))
        (if (> n 2)
            (insert " title '" (nth (+ i 1) header) "'")
          (insert " notitle"))
        (if (> i 0) (insert ",")))
      (if (= 1 n) (insert " using 1 with " style " notitle"))
      (newline)
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot))

    ;; Cleanup
    (kill-buffer buf)
    ;; (delete-file name)
    ))

(defvar gnuplot--styles '(("line" . "line")
                          ("bar" . "histograms")
                          ("scatter" . "circles")
                          ("pie" . "pie")
                          ("half pie" . "pie_half")
                          ("donut" . "donut")
                          ("half donut" . "donut_half")
                          ("spider" . "spider")
                          ))
(defvar gnuplot--style-prev nil)
(defvar gnuplot--timer nil)
(defvar gnuplot--image-dir "~/Pictures/gplot")

(defvar gnuplot--flag-day nil)
(defvar gnuplot--flag-month nil)
(defvar gnuplot--flag-year nil)

(defun gnuplot--resolve-token (a)
  (cond ((= (length a) 4) (setq gnuplot--flag-year "%Y"))
        ((= (length a) 3) (setq gnuplot--flag-month "%b")) ;; Jan
        ((> (string-to-number a) 31) (setq gnuplot--flag-year "%y"))
        ((> (string-to-number a) 12) (setq gnuplot--flag-day "%d"))
        (t (setq gnuplot--flag-month "%m"))))

(defun gnuplot--gen-datefmt (datetime &optional datetime1)
  "Generate a gnuplot date format string or NIL if invalid."
  (let* (;(tokens (parse-time-string (org-read-date nil nil datetime)))
         (dash  (and (string-match-p "-" datetime) "-"))
         (slash (and (string-match-p "/" datetime) "/"))
         (time  (string-match-p ":" datetime))
         (sep   (or dash slash))
         (tokens1 (split-string datetime sep))
         (tokens2 (split-string datetime1 sep))
         res tokens3 out i)
    (when (or sep time)
      (setq gnuplot--flag-day nil
            gnuplot--flag-month nil
            gnuplot--flag-year nil)
      ;; 1. Check for length
      (setq tokens3 (mapcar 'gnuplot--resolve-token tokens1))
      ;; (pp tokens3)

      ;; 2. Use second date to disambiguate
      (unless (and gnuplot--flag-day gnuplot--flag-month gnuplot--flag-year)
      (pcase-let
          ((`(,s1 ,s2 ,s3) tokens1)
           (`(,t1 ,t2 ,t3) tokens2))
        (if (= (length s1) 4) (push "%Y" tokens3)
          (if (string= s1 t1) (push "%m" tokens3) (push "%d" tokens3)))

        (if (= (length s1) 3) (push "%b" tokens3) ;; Jan
          (if (string= s2 t2) (push "%m" tokens3) (push "%d" tokens3)))

        (when s3
          (if (= (length s3) 4) (push "%Y" tokens3)
            (if (string= s3 t3) (push "%m" tokens3) (push "%d" tokens3)))
          ))
      ;; (pcase-let
      ;;     ((`(,s1 ,s2) tokens1)
      ;;      (`(,t1 ,t2) tokens2))
      ;;   (if (= (length s1) 4) (push "%Y" tokens3))
      ;;   (if (string= s1 t1)   (push "%m" tokens3) (push "%d" tokens3))
      ;;   (if (string= s2 t2)   (push "%m" tokens3) (push "%d" tokens3))
      ;;   (if (= (length s2) 4) (push "%Y" tokens3))
      ;;   )
      ;; (pp tokens)
      (setq tokens3 (nreverse tokens3)))
      (setq res (mapconcat 'identity tokens3 sep))
      ;; (pcase-let
      ;;     ((`(,sec ,min ,hour ,day ,mon ,year dow dst tz) tokens))
      ;;   (setq res (apply 'concat
      ;;                    (append (if mon  (list "%m" sep))
      ;;                            (if day  (list "%d" sep))
      ;;                            (if year (list year-str)))
      ;;                    )))

      ;; 3. Seek user help
      (pcase-let
          ((`(,s1 ,s2 ,s3) out))
        (when (or (string= s1 s2)
                  (string= s3 s2))
          (if (string= s3 s2)
              (if (string= s1 "Y")
                  (setq out (list (mapconcat 'identity '("Y" "m" "d") sep)
                                  (mapconcat 'identity '("Y" "d" "m") sep)
                                  ))))
          (if (string= s1 s2)
              (if (string= s3 "Y")
                  (setq out (list (mapconcat 'identity '("m" "d" "Y") sep)
                                  (mapconcat 'identity '("d" "m" "Y") sep)
                                  ))))
          (pp out)
          ))
      )
    res))

(defun gnuplot--draw-line (style m n xlabel ylabel labelcol datetime)
  ""
  (let (j)
    (insert "set xlabel '" (or xlabel "x-axis") "'\n")
    (insert "set ylabel '" (or ylabel "y-axis") "'\n")
    (when datetime
      (insert "set xdata time\n")
      (insert "set format x \"" (if (< m 10) "%d-" "") "%b-%y\"\n")
      (insert "set timefmt \"" datetime "\"\n"))
    (insert "plot file")
    (setq j (if (or labelcol datetime (> n 1)) 1 0))
    (dotimes (i (- n j))
      (insert " using ")
      (when (not (string= style "histograms"))
        (insert (number-to-string
                 (if (or labelcol (= n 1)) 0 1))
                ":"))
      (insert (number-to-string (+ i j 1)))
      (if (and labelcol (not datetime)) (insert ":xtic(1)"))
      (if (< i n) (insert " with " style))
      (if (> n 2)
          (insert " title columnhead")
        (insert " notitle"))
      (if (< i (- n j 1)) (insert ", ''")))
    ))

(defun gnuplot--draw-pie (m header labelcol datetime N &optional donut)
  (if datetime (error "Date is not supported for piechart"))
  (let* ((col (if labelcol "2" "1"))
         (rows (format "%d:%d" 0 (- m 1 (if header 1 0))))
   (fmt1 "(sprintf('%s (%05.2f%%)', stringcolumn(1), percent($2)))")
   (fmt2 "(sprintf('%05.2f%%', percent($1)))"))
    (if header (insert "set datafile columnheaders\n"))
    (insert "stats file using " col " nooutput prefix 'A'\n")
    (insert "N         = " (number-to-string N) "\n")
    (insert "angle(x)  = x*N*180/A_sum\n")
    (insert "percent(x)= x*100/A_sum\n")

    (insert "centerX = 0\n")
    (insert "centerY = 0\n")
    (insert "radius  = 1\n")
    (insert "pos     = 0\n")
    (insert "colour  = 0\n")

    (insert "yposmax = 0.95*radius\n")
    (insert "xpos    = 1.5*radius\n")
    (insert "ypos(i) = yposmax - i*(yposmax)/(1.0*A_records)\n")

    (insert "set xrange [-N:2]\n")
    (insert "set yrange [-N:2]\n")
    (insert "unset key\n")
    (insert "unset tics\n")
    (insert "unset border\n")
    (insert "plot file using "
            "(centerX):(centerY):(radius):(pos):(pos=pos+angle($"
          col ")):(colour=colour+1) "
          "with circle linecolor var notitle")
    (if donut
  (insert "\\\n, '' using "
          "(centerX):(centerY):(0.5) "
          "with circle linecolor 'white' notitle"))
    (insert "\\\n, for [i=" rows "] file using (xpos):(ypos(i)):"
      (if labelcol fmt1 fmt2)
      " every ::i::i with labels left offset 3,0")
    (insert "\\\n, for [i=" rows "] '+' using (xpos):(ypos(i)) "
      "with points pointtype 5 pointsize 4 linecolor i+1\n")
  ))

(defun gnuplot--draw-spider (n header labelcol)
  (let* ((j (if labelcol 1 0))
   (colfmt (format "%d:%d" j (- n j))))
    (if (< (- n j) 3) (error "Atleast 3 axes needed"))

    (insert "set spiderplot\n")
    (insert "set style spiderplot fill transparent solid 0.30 border\n")
    (insert "set for [i=" colfmt "] paxis i range [0:100]\n")
    (if (not header)
  (insert "set for [i=" colfmt "] paxis i label sprintf('%d',i)\n"))
    (insert "set paxis 1 tics\n")
    (insert "set grid spider linetype black linewidth 0.2\n")

    (insert "plot for [i="
      (format "%d:%d" (1+ j) n)
      "] file using i:key(1)"
      (if header " title columnhead" "")
      "\n")
    ))

(defun gnuplot--draw (&optional title style)
  (or killed-rectangle (error "No tabular data"))
  (let* ((name (make-temp-file "plot"))
         (data killed-rectangle)
         (comma (and (string-match-p "," (car data)) ","))
         (pipe  (and (string-match-p "|" (car data)) "|"))
   (sep   (or comma pipe))
         (cols (split-string (car data) sep))
   (m (length data))
         (n (length cols))
   (str-check "^ *[a-zA-Z'\"]")
         buf xlabel ylabel header labelcol datetime)
    ;; Extract plot details
    ;; Column header
    (when (string-match-p str-check (car cols))
      (if (= n 1)
          (if (string-match-p str-check (nth 1 data))
              (error "Bad data")
            (setq ylabel (car cols)
                  labelcol nil))
        (when (string-match-p str-check (nth 1 cols))
          (setq header cols
                ;; style "histograms"
                )
          (pcase n
            (1 (setq ylabel (nth 1 header)))
            (2 (setq xlabel (nth 0 header)
                     ylabel (nth 1 header)))
            (_ (setq xlabel (nth 0 header))))
          ;; (pop killed-rectangle)
          )))
    ;; First column might be a label column or datetime
    (setq labelcol (string-match-p str-check (nth 1 data)))
    (setq datetime (gnuplot--gen-datefmt (car (split-string (nth 1 data) sep))
                                         (car (split-string (nth 2 data) sep))))

    (setq buf (find-file-noselect name))
    (with-current-buffer buf
      (yank-rectangle)
      (save-buffer))

    (setq style (or style "line"))
    ;; (setq style "spider")
    ;; (setq style (if labelcol "histograms"))
    ;; (setq style "pie")
    (with-temp-buffer
      ;; (insert "unset xdata\n")
      ;; (insert "unset ydata\n")
      ;; (insert "unset xrange\n")
      ;; (insert "unset yrange\n")
      ;; (insert "unset format xy\n")
      ;; (insert "unset timefmt\n")
      ;; First \n is to flush any pending commands
      (insert "\nreset\n")

      (insert "set datafile separator "
        (if sep (format "'%s'" sep) "whitespace") "\n")
      (insert "set style fill solid 0.5\n")
      (insert "set title  '" (or title  "Title")  "'\n")
      (insert "file='" name "'\n")

      (pcase style
  ("pie" (gnuplot--draw-pie m header labelcol datetime 2))
  ("pie_half" (gnuplot--draw-pie m header labelcol datetime 1))
  ("donut" (gnuplot--draw-pie m header labelcol datetime 2 t))
  ("donut_half" (gnuplot--draw-pie m header labelcol datetime 1 t))
  ("spider" (gnuplot--draw-spider n header labelcol))
  (_ (gnuplot--draw-line style m n xlabel ylabel labelcol datetime)))

      (newline)
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot))

    ;; Cleanup
    (kill-buffer buf)
    ;; (delete-file name)
    ))

(defun gnuplot--draw-preview ()
  (let* ((style (get-text-property (point) 'gplot-style))
    (gnuplot-inline-image-mode 'inline))
    (when (and (memq last-command '(left-char right-char previous-line next-line))
        (not (string= gnuplot--style-prev style)))
      (setq gnuplot--style-prev style)
      (gnuplot--draw nil style))
    )
  )

(defun gnuplot--timer-activate ()
  (if gnuplot--timer (cancel-timer gnuplot--timer))
  (setq gnuplot--timer (run-with-idle-timer 1 nil 'gnuplot--draw-preview)))

;; styles lines, points, linespoints, impulses, dots, steps, errorbars (or
;; yerrorbars), xerrorbars, xyerrorbars, boxes, boxerrorbars, boxxyerrorbars
;;;###autoload
(defun gnuplot-rectangle2 (prefix)
  (interactive "P")
  (let ((styles gnuplot--styles)
    title style)
    (when prefix
      (setq title (read-string "Title: "))
      (setq style (cdr (assoc (completing-read "Style: " styles) styles)))
      )
    (gnuplot--draw title style)))

;;;###autoload
(defun gnuplot-preview ()
  (interactive)
  (let ((styles gnuplot--styles)
  (icon (file-exists-p gnuplot--image-dir))
  (buf (get-buffer-create "*gnuplot options*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (cursor-intangible-mode 1)
      (add-hook 'post-command-hook 'gnuplot--timer-activate nil t)
      (dolist (i styles)
  (insert (propertize (car i)
    'cursor-intangible t
    'display (if icon (create-image
       (format "%s/gplot_%s.png"
       gnuplot--image-dir (car i))))
    'gplot-style (cdr i)
    )
" "))
      (view-mode 1))

    (pop-to-buffer buf t t)
    ))

(provide 'graph)
