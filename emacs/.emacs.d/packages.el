;; Shell variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :init
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-d") 'counsel-dired)
  (global-set-key (kbd "C-c t") 'counsel-load-theme)
  (setq ivy-use-virtual-buffers t
	ivy-use-selectable-prompt t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :ensure t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

(use-package which-key
  :ensure t
  :init
  :config (which-key-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
	evil-esc-delay 0
	evil-want-keybinding nil
	evil-undo-system 'undo-fu)
  :config
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (evil-mode))

(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  )

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))


(use-package company
  :ensure t
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-idle-delay 0
	company-minimum-prefix-length 1
	company-selection-wrap-around t
	company-echo-delay 0)
  (global-company-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; LSP mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
	gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024)
	lsp-idle-delay 0.500
	lsp-log-io nil) ;; 1mb
  :init
  (add-hook 'js-mode-hook 'lsp)
  (add-hook 'html-mode-hook 'lsp)
  (add-hook 'css-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'sh-mode 'lsp)
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

;; Lsp Ui for checking
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-completion-show-detail t
	lsp-ui-doc-enable nil))


(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-pyright
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   ;; Apps
   "a" '(:ignore t :which-key "Applications")
   "ad" 'dired
   "av" 'vterm-other-window
   "at" 'treemacs

   ;; Avy
   "f" 'avy-goto-char-2

   ;; Code stuff
   "l" '(:ignore t :which-key "Code stuff")
   "lf" 'format-all-buffer
   "ls" 'yas-insert-snippet
   "li" 'auto-insert

   ;; Universal argument
   "u" 'universal-argument

   ;; Restart Emacs
   "R" 'restart-emacs

   ;; Magit
   "g" 'magit

   ;; Search index
   "s" '(:ignore t :which-key "Search")
   "se" 'elisp-index-search
   "sf" 'emacs-index-search

   ;; Compile with make
   "c" 'compile

   ;; Projectile
   "p" 'projectile-command-map))

(use-package cmake-mode
  :ensure t)

(use-package vterm
  :ensure t)

(use-package avy
  :ensure t)

(use-package python
  :ensure t
  :init
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package tree-sitter
  :ensure t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Projects/")
	projectile-completion-system 'ivy)
  (projectile-mode))

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package magit
  :ensure t)

(use-package restart-emacs
  :ensure t)

(use-package hydra
  :ensure t)

(use-package realgud
  :ensure t)

(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  (ivy-prescient-mode))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-ivy-rich
  :ensure t)

(use-package writeroom-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package lua-mode
  :ensure t)

(use-package undo-fu
  :init
  (setq evil-undo-system 'undo-fu)
  :ensure t)

(use-package sly
  :ensure t)

(use-package no-littering
  :ensure t)

(use-package dap-mode
  :ensure t
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (add-hook 'dap-stopped-hook (lambda () (call-interactively 'dap-hydra)))
  :config
  (require 'dap-python)
  (require 'dap-gdb-lldb)
  (require 'dap-chrome))

;; (use-package web-mode
;;   :ensure t
;;   :config
;;   (setq web-mode-markup-indent-offset 2
;; 	web-mode-css-indent-offset 2
;; 	web-mode-code-indent-offset 2
;; 	web-mode-enable-auto-pairing t
;; 	web-mode-enable-css-colorization t
;; 	web-mode-enable-comment-interpolation t
;; 	web-mode-enable-current-column-highlight t)
;;   (setq web-mode-ac-sources-alist
;; 	'(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
;; 	  ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
;; 	  ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

;; (use-package perspective
;;   :ensure t
;;   :config

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep t))

(use-package treemacs
  :ensure t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
