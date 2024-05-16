;;; Emacs-config-file --- Summary  -*- lexical-binding:t -*-

;;; Commentary:
;; Init file for Emacs

;;; Code:
(use-package emacs
  :config
  ;; Set side fringes
  (set-fringe-mode 10)

  ;; Disable menu bar, tool bar, and scroll bar
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Disable blinking cursor
  (blink-cursor-mode -1)

  ;; Save cursor place
  (save-place-mode +1)

  ;; Pixel precision for mouse scrolling
  (pixel-scroll-precision-mode +1)

  ;; Show column number
  (column-number-mode +1)

  ;; Always use spaces for tabs
  (indent-tabs-mode -1)

  ;; Save minibuffer history
  (savehist-mode +1)

  ;; Show symbols
  (global-prettify-symbols-mode +1)

  ;; Recent files
  (recentf-mode +1)

  ;; Associate certain files to correct ts-mode
  (add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '(".bashrc" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("Containerfile" . dockerfile-ts-mode))

  ;; Third party packages
  (defvar package-archives)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; Make async shell command faster
  (setq read-process-output-max (* 64 1024 1024))
  (setq process-adaptive-read-buffering nil)
  (setq process-connection-type nil)

  ;; Set fonts
  (add-to-list 'default-frame-alist
	       '(font . "Hack Nerd Font-11"))
  (set-face-attribute 'default t :font "Hack Nerd Font-11")

  ;; Start emacs maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Custom settings in its own file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror)

  ;;; Prevent Extraneous Tabs
  (setq-default indent-tabs-mode nil)

  :hook

  ;; Enable line modes in programming modes
  (prog-mode . display-line-numbers-mode)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-setup . cursor-intangible-mode)

  :custom
  ;; Allow pin entry inside of emacs minibuffer
  (setq epg-pinentry-mode 'loopback)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Use short answers
  (use-short-answers t)

  ;; Tab first then try complete
  (tab-always-indent 'complete)

  ;; Backup settings
  (lock-file-name-transforms
	'(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
  (auto-save-file-name-transforms
	'(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
  (backup-directory-alist
	'((".*" . "~/.emacs.d/aux/")))
  (kept-old-versions 2)
  (kept-new-versions 6)
  (vc-make-backup-files t)
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)

  ;; Disable startup screen and messages
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message "smakey18")

  ;; Name and email
  (user-full-name "Ren Abelard Arriola Odion")
  (user-mail-address "smakey18@gmail.com")

  ;; Allow consecutive mark jumps (C-u C-SPC...C-SPC)
  (set-mark-command-repeat-pop t)

  ;; Don't reset cursor position when point moves offscreen
  (scroll-conservatively 101)
  (scroll-preserve-screen-position +1)

  ;; ;; Completion related settings
  ;; (completion-auto-help 'always)
  ;; (completion-styles '(flex partial-completion substring))
  ;; (completion-category-overrides '((file (styles basic substring))))
  ;; (read-buffer-completion-ignore-case t)
  ;; (completions-format 'one-column)
  ;; (completion-auto-select 'second-tab)
  ;; (completions-detailed t)
  :bind
  ;; Use hippie expand
  ([remap dabbrev-expand] . hippie-expand))

;; Native LSP
(use-package eglot
  :hook ((c++-ts-mode bash-ts-mode typescript-ts-mode) . eglot-ensure)
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package flymake
  :hook ((emacs-lisp-mode) . flymake-mode))

;; Window movement
(use-package windmove
  :bind (("C-c w h" . windmove-left)
         ("C-c w l" . windmove-right)
         ("C-c w k" . windmove-up)
         ("C-c w j" . windmove-down)
         ("C-c w H" . windmove-swap-states-left)
         ("C-c w L" . windmove-swap-states-right)
         ("C-c w K" . windmove-swap-states-up)
         ("C-c w J" . windmove-swap-states-down)))

;; Add color to compilation buffer
(use-package ansi-color
  :commands (ansi-color-apply-on-region)
  :config
  (defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))

(use-package elec-pair
  :hook
  ;; Enable electric pairs in following modes
  ((c++-ts-mode bash-ts-mode emacs-lisp-mode typescript-ts-mode) . electric-pair-local-mode))

;; Custom settings for C/C++
(use-package c-ts-mode
  :defer t
  :custom
  ;; Indent and code style
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux))

;; Custom settings for json-ts
(use-package json-ts-mode
  :defer t
  :custom
  (json-ts-mode-indent-offset 8))

;; ;; Default minibuffer completion
;; (use-package icomplete
;;   :demand t
;;   :custom
;;   (completion-styles '(partial-completion substring flex))
;;   (completion-category-overrides '((file (styles basic substring))))
;;   ;;  (read-file-name-completion-ignore-case t)
;;   (read-buffer-completion-ignore-case t)
;;   ;;  (completion-ignore-case t)
;;   :config
;;   ;;  (icomplete-mode)
;;   ;;  (icomplete-vertical-mode)
;;   (fido-vertical-mode +1)
;;   :bind (:map icomplete-minibuffer-map
;;               ("C-n" . icomplete-forward-completions)
;;               ("C-p" . icomplete-backward-completions)))

;; ;; Color theme
(use-package doom-themes
  :ensure t
  :commands (doom-themes-visual-bell-config doom-themes-org-config)
  :custom
  (doom-vibrant-brighter-comments t)
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-vibrant-brighter-comments t)
  (doom-vibrant-brighter-modeline t)
  (doom-vibrant-padded-modeline t)
  :init
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; LSP Mode
;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp-booster--advice-final-command lsp-booster--advice-json-parse)
;;   :init
;;   ;; For LSP Booster
;;   (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;     "Try to parse bytecode instead of json."
;;     (or
;;      (when (equal (following-char) ?#)
;;        (let ((bytecode (read (current-buffer))))
;; 	 (when (byte-code-function-p bytecode)
;; 	   (funcall bytecode))))
;;      (apply old-fn args)))
;;   (advice-add (if (progn (require 'json)
;; 			 (fboundp 'json-parse-buffer))
;; 		  'json-parse-buffer
;; 		'json-read)
;; 	      :around
;; 	      #'lsp-booster--advice-json-parse)

;;   (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;     "Prepend emacs-lsp-booster command to lsp CMD."
;;     (let ((orig-result (funcall old-fn cmd test?)))
;;       (if (and (not test?) ;; for check lsp-server-present?
;; 	       (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;; 	       lsp-use-plists
;; 	       (not (functionp 'json-rpc-connection)) ;; native json-rpc
;; 	       (executable-find "emacs-lsp-booster"))
;; 	  (progn
;; 	    (message "Using emacs-lsp-booster for %s!" orig-result)
;; 	    (cons "emacs-lsp-booster" orig-result))
;; 	orig-result)))

;;   (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;;   (setq lsp-keymap-prefix "C-c l")

;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;; 	  '(orderless))) ;; Configure orderless

;;   :hook (;; Auto start in the following modes
;; 	 ((c++-ts-mode bash-ts-mode cmake-ts-mode json-ts-mode typescript-ts-mode dockerfile-ts-mode) . lsp-deferred)
;; 	 (lsp-completion-mode . my/lsp-mode-setup-completion))
;;   :commands (lsp lsp-deferred)
;;   :custom
;;   (lsp-completion-provider :none) ;; For corfu
;;   (lsp-idle-delay 0.1)
;;   (gc-cons-threshold 100000000)
;;   (read-process-output-max (* 1024 1024)))

;; (use-package dap-mode
;;   :ensure t
;;   :hook ((dap-stopped) . (lambda () (call-interactively #'dap-hydra)))
;;   :config
;;   (dap-auto-configure-mode +1)
;;   (require 'dap-cpptools)
;;   (require 'dap-node))

;; ;; Extra lsp features
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; Better terminal
(use-package vterm
  :ensure t
  :defer t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t)

;; Snippets
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :config
  (yas-reload-all)
  :hook
  ;; Enable yas-snippets in the following modes
  ((c++-ts-mode bash-ts-mode emacs-lisp-mode cmake-ts-mode json-ts-mode typescript-ts-mode) . yas-minor-mode))

;; Actual snippets
(use-package yasnippet-snippets
  :ensure t)

(use-package corfu
  :ensure t
  :commands (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
		corfu-popupinfo-delay nil)
    (corfu-mode +1)))
  (global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
	 (shell-mode . corfu-mode)
	 (eshell-mode . corfu-mode)
	 (minibuffer-setup . #'corfu-enable-in-minibuffer)))

;; Git porcelain
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-dispatch)
         ("C-c f" . magit-file-dispatch)))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :bind (("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)		;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer

	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop) ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)
	 ("M-g g" . consult-goto-line)		  ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)	  ;; orig. goto-line
	 ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-fd) ;; Alternative: consult-fd
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history) ;; orig. next-matching-history-element
	 ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  :custom
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (global-flycheck-mode +1)
;;   :custom
;;   (flycheck-check-syntax-automatically '(save mode-enable)))

;; (use-package consult-flycheck
;;   :ensure t)

;; (use-package consult-lsp
;;   :ensure t
;;   :config
;;   (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; Centered window
(use-package writeroom-mode
  :ensure t
  :defer t)

;; Automated treesitter
(use-package treesit-auto
  :ensure t
  :commands (global-treesit-auto-mode treesit-auto-add-to-auto-mode-alist)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 4))

;; Autofind python env
(use-package pet
  :ensure t
  :defer t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Minibuffer Completion
(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :init
  (vertico-mode)
  :custom
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ;; Move minibuffer stuff to middle
;; ;; Very glitchy
;; (use-package vertico-posframe
;;   :ensure t
;;   :config
;;   (vertico-posframe-mode 1))

;; Completion style
(use-package orderless
  :ensure t)

;; Annotations
(use-package marginalia
  :ensure t
  :commands (marginalia-mode)
  :init
  (marginalia-mode +1))

;; Icons
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Project management
(use-package projectile
  :ensure t
  :commands (projectile-mode)
  :defines (projectile-mode-map)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :hook
  (project-find-functions . project-projectile)
  :custom
  ;; Allow compilation buffer to be editable (useful for interactive apps)
  (projectile-comint-mode t)
  ;; Auto search Projects folder for projects
  (projectile-project-search-path '("~/Projects")))

(use-package consult-projectile
  :ensure t
  :bind (:map projectile-mode-map
	      ([remap projectile-find-file] . consult-projectile-find-file)
	      ([remap projectile-find-dir] . consult-projectile-find-dir)
	      ([remap projectile-find-file-other-window] . consult-projectile-find-file-other-window)
	      ([remap projectile-switch-to-buffer-other-window] . consult-projectile-switch-to-buffer-other-window)
	      ([remap projectile-recentf] . consult-projectile-recentf)
	      ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer))
  :after projectile)

(use-package ripgrep
  :ensure t
  :defer t)

(use-package git-modes
  :ensure t)

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind
  (([remap scroll-down-command] . golden-ratio-scroll-screen-down)
   ([remap scroll-up-command] . golden-ratio-scroll-screen-up)))

(use-package doom-modeline
  :ensure t
  :commands (doom-modeline-mode)
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-vcs-max-length 30))

(use-package cape
  :ensure t
  :bind (("C-c c p" . completion-at-point)
	 ("C-c c t" . complete-tag)
	 ("C-c c d" . cape-dabbrev)
	 ("C-c c h" . cape-history)
	 ("C-c c f" . cape-file)
	 ("C-c c k" . cape-keyword)
	 ("C-c c s" . cape-elisp-symbol)
	 ("C-c c e" . cape-elisp-block)
	 ("C-c c a" . cape-abbrev)
	 ("C-c c l" . cape-line)
	 ("C-c c w" . cape-dict)
	 ("C-c c :" . cape-emoji)
	 ("C-c c _" . cape-tex)
	 ("C-c c &" . cape-sgml)
	 ("C-c c r" . cape-rfc1345)
	 ("C-c c y" . yasnippet-capf))
  :init
  (defun smren/elisp-capf-setup ()
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))
  :hook
  (emacs-lisp-mode . smren/elisp-capf-setup))

(use-package yasnippet-capf
  :ensure t
  :after cape)

(use-package corfu-terminal
  :ensure t
  :commands (corfu-terminal-mode)
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :defines (corfu-margin-formatters)
  :commands (nerd-icons-corfu-formatter)
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package git-gutter
  :ensure t
  :commands (global-git-gutter-mode)
  :init
  (global-git-gutter-mode +1))

(use-package avy
  :ensure t
  :commands (avy-setup-default)
  :init
  (avy-setup-default)
  :bind (("C-c a c" . avy-goto-char)))

;;; init.el ends here
