(use-package exec-path-from-shell
  :ensure t
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  :init
  (setq evil-collection-company-use-tng t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))


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
  :init
  (setq
   flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :config
  (global-flycheck-mode))

;; LSP mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
	gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024)
	lsp-idle-delay 0.0
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
	lsp-ui-doc-enable nil
	lsp-headerline-breadcrumb-icons-enable nil))


(use-package lsp-pyright
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package vterm
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
  (setq emmet-expand-jsx-className? t
	emmet-move-cursor-after-expanding t
	emmet-move-cursor-between-quotes t)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'js-jsx-mode-hook 'emmet-mode)
  (add-hook 'js-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package magit
  :ensure t)

(use-package undo-fu
  :init
  (setq evil-undo-system 'undo-fu)
  :ensure t)

(use-package sly
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-enable-auto-pairing t
	web-mode-enable-css-colorization t
	web-mode-enable-comment-interpolation t
	web-mode-enable-current-column-highlight t)
  (setq web-mode-ac-sources-alist
	'(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
	  ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
	  ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep t))

(use-package format-all
  :ensure t
  :init
  (setq format-all-formatters '(("JSX" prettier)
				("JavaScript" prettier)
				("Shell" (shfmt "-i" "2"))))
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-c f") 'format-all-buffer))))

(use-package writeroom-mode
  :ensure t
  :config
  (global-set-key "\C-c\ w" 'writeroom-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))
