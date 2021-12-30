;;; package --- Summary
;;; Commentary:
;;; Downloaded Packages"

;;; Code:

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
  :commands
  evil-set-leader
  evil-define-key*
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
  :defines
  evil-collection-company-use-tng
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
  :commands
  lsp-enable-which-key-integration
  :defines
  lsp-completion-show-detail
  lsp-ui-doc-enable
  lsp-headerline-breadcrumb-icons-enable
  lsp-ui-imenu-auto-refresh
  lsp-ui-doc-show-with-cursor
  lsp-ui-doc-show-with-mouse
  lsp-headerline-breadcrumb-mode
  :init
  (setq gc-cons-threshold 100000000
	    read-process-output-max (* 1024 1024)
	    lsp-idle-delay 0.0
	    lsp-log-io nil)
  :init
  ;; (add-hook 'js-mode-hook 'lsp)
  ;; (add-hook 'html-mode-hook 'lsp)
  ;; (add-hook 'css-mode-hook 'lsp)
  ;; (add-hook 'c++-mode-hook 'lsp)
  ;; (add-hook 'c-mode-hook 'lsp)
  ;; (add-hook 'sh-mode 'lsp)
  (dolist (hook '(js-mode-hook html-mode-hook css-mode-hook c++-mode-hook c-mode-hook sh-mode-hook))
    (add-hook hook 'lsp-deferred))
  (add-hook 'python-mode-hook (lambda ()
				                (setq lsp-pyright-venv-path ".venv"
				                      lsp-pyright-python-executable-cmd ".venv/bin/python")
				                (lsp-deferred)))
  (add-hook 'lsp-mode-hook (lambda ()
			                 (define-key lsp-mode-map (kbd "<leader>l") lsp-command-map)
			                 (define-key lsp-mode-map (kbd "<leader>lw") 'lsp-ivy-workspace-symbol)
			                 (lsp-enable-which-key-integration)))
  :config
  (setq lsp-completion-show-detail t
		lsp-ui-doc-enable t
        lsp-headerline-breadcrumb-enable t
		lsp-headerline-breadcrumb-icons-enable t
        lsp-ui-imenu-auto-refresh t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t)
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
  :init
  (setq projectile-project-search-path '("~/Projects/")
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil)
  :config
  (projectile-mode t ))

(use-package emmet-mode
  :ensure t
  :defines
  emmet-expand-jsx-className?
  :init
  (setq emmet-expand-jsx-className? t
	    emmet-move-cursor-after-expanding t
	    emmet-move-cursor-between-quotes t)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (dolist (hook '(html-mode-hook js-jsx-mode-hook js-mode-hook css-mode-hook))
    (add-hook hook 'emmet-mode)))

(use-package magit
  :ensure t)

(use-package undo-fu
  :init
  (setq evil-undo-system 'undo-fu)
  :ensure t)

(use-package sly
  :ensure t)

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
                                ("HTML" prettier)
				                ("Python" black)
				                ("JavaScript" prettier)
				                ("Shell" (shfmt "-i" "2"))))
  (add-hook 'prog-mode-hook
	        (lambda ()
	          (local-set-key (kbd "<leader>=") 'format-all-buffer))))

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

(defalias 'buffer-commands
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-buffer)
    (define-key map (kbd "p") 'previous-buffer)
    (define-key map (kbd "k") 'kill-buffer)
    map)
  "Buffer related bindings.")

(defalias 'avy-commands
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") 'avy-goto-char-timer)
    (define-key map (kbd "f") 'avy-goto-char)
    (define-key map (kbd "r") 'avy-goto-word-1)
    map)
  "Avy related bindings.")

(defalias 'treemacs-commands
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'treemacs-display-current-project-exclusively)
    (define-key map (kbd "s") 'lsp-treemacs-symbols)
    map)
  "Treemacs related bindings.")

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package avy
  :ensure t
  :config
  (avy-setup-default))

(use-package counsel
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (evil-define-key '(normal visual) 'global
    (kbd "C-s") 'swiper
    (kbd "C-x f") 'counsel-find-file
    (kbd "C-x b") 'ivy-switch-buffer
    (kbd "C-x C-b") 'ivy-switch-buffer
    (kbd "C-x d") 'counsel-dired
    (kbd "C-x C-d") 'counsel-dired
    (kbd "C-x r") 'counsel-recentf
    (kbd "C-x C-r") 'counsel-recentf)
  (evil-define-key '(normal visual) 'global
    (kbd "<leader>b") 'buffer-commands
    (kbd "<leader>t") 'treemacs-commands
    (kbd "<localleader>n") 'centaur-tabs-forward
    (kbd "<localleader>p") 'centaur-tabs-backward
    (kbd "<leader>a") 'avy-commands)
  :config
  (counsel-mode t))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode t))

(use-package ivy-prescient
  :ensure t
  :init
  (ivy-prescient-mode t))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (all-the-icons-ivy-rich-mode t))

(use-package ivy-rich
  :ensure t
  :commands
  ivy-format-function-line
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode t)
  (ivy-rich-project-root-cache-mode t))

(use-package lsp-ivy
  :ensure t)

(use-package centaur-tabs
  :ensure t
  :commands
  centaur-tabs-headline-match
  :init
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-icons t
        centaur-tabs-height 32
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (add-hook 'vterm-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t))

(use-package ag
  :ensure t)

(use-package rg
  :ensure t)

(use-package treemacs
  :ensure t
  :commands
  treemacs-follow-mode
  treemacs-filewatch-mode
  treemacs-fringe-indicator-mode
  :config
  (setq treemacs-filewatch-mode t
        treemacs-follow-mode t
        treemacs-fringe-indicator-mode 'always
        treemacs-silent-refresh t))

(use-package treemacs-projectile
  :ensure t)

(use-package treemacs-evil
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode t))

;; (use-package perspective
;;   :ensure t
;;   :bind
;;   ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
;;   :config
;;   (persp-mode 1))

;;; packages.el ends here
