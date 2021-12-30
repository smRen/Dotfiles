;;; package --- Summary
;;; Commentary:
;;; Downloaded Packages"

;;; Code:
(defalias 'buffer-commands
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-buffer)
    (define-key map (kbd "p") 'previous-buffer)
    (define-key map (kbd "k") 'kill-current-buffer)
    (define-key map (kbd "l") 'switch-to-buffer)
    map)
  "Buffer related bindings.")

(defun horizontal-term ()
  "Makes a 15 line height terminal window"
  (interactive)
  (split-window-below -15)
  (other-window 1)
  (vterm))


(defalias 'terminal-commands
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") 'vterm)
    (define-key map (kbd "o") 'horizontal-term)
    map)
  "Terminal related bindings.")

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
	evil-esc-delay nil
	evil-want-keybinding nil
	evil-undo-system 'undo-fu)
  :config
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (evil-set-leader '(normal visual) (kbd "SPC")) ;; leader
  (evil-set-leader '(normal visual) "," t) ;; local leader
  (evil-define-key '(normal visual) 'global
    (kbd "C-u") 'evil-scroll-up
    (kbd "<leader>v") 'terminal-commands
    (kbd "<leader>p") 'projectile-command-map
    (kbd "<leader>b") 'buffer-commands
    (kbd "<leader>w") 'writeroom-mode)
  (evil-mode t))

(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (setq evil-motion-state-cursor 'box
	evil-visual-state-cursor 'box
	evil-normal-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-emacs-state-cursor  'hbar))

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
  (global-evil-surround-mode t))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode t))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode t))


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

(use-package lsp-pyright
  :ensure t)

;; LSP mode
(use-package lsp-mode
  :ensure t
  :init
  (setq gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024)
	lsp-idle-delay 0.0
	lsp-log-io nil)
  :init
  (add-hook 'js-mode-hook 'lsp)
  (add-hook 'html-mode-hook 'lsp)
  (add-hook 'css-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'python-mode-hook (lambda ()
				(setq lsp-pyright-venv-path ".venv"
				      lsp-pyright-python-executable-cmd ".venv/bin/python")
				(lsp)))
  (add-hook 'sh-mode 'lsp)
  (add-hook 'lsp-mode-hook (lambda ()
			     (setq lsp-completion-show-detail t
				   lsp-ui-doc-enable nil
				   lsp-headerline-breadcrumb-icons-enable nil)
			     (define-key lsp-mode-map (kbd "<leader>l") lsp-command-map)
			     (lsp-enable-which-key-integration)))
  :commands (lsp lsp-deferred))



;; Lsp Ui for checking
(use-package lsp-ui
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
  (global-tree-sitter-mode t)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Projects/"))
  (projectile-mode t ))

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
				("Python" black)
				("JavaScript" prettier)
				("Shell" (shfmt "-i" "2"))))
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (local-set-key (kbd "<leader>f") 'format-all-buffer))))

(use-package writeroom-mode
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))

(use-package which-key
  :ensure t
  :init
  :config
  (which-key-mode t))

;; (use-package selectrum
;;   :ensure t
;;   :config
;;   (selectrum-mode t))

;; (use-package perspective
;;   :ensure t
;;   :bind
;;   ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
;;   :config
;;   (persp-mode 1))

;;; packages.el ends here
