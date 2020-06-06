(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
	
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package which-key
  :config (which-key-mode)) ;; global which key mode

(use-package all-the-icons)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  (load-theme 'doom-snazzy t))

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-horizon") ; use the colorful treemacs theme
  ;;(doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  ;;(doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package vterm)
(use-package vterm-toggle)
(global-set-key [f3] 'vterm-toggle)
(global-set-key [C-f3] 'vterm-toggle-cd)

;; you can cd to the directory where your previous buffer file exists
;; after you have toggle to the vterm buffer with `vterm-toggle'.
(define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                ;;(display-buffer-reuse-window display-buffer-at-bottom)
                (display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                (direction . bottom)
                (dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))


;; Recent files
;;(recentf-mode 1)
;;(setq recentf-max-menu-items 25)
;;(setq recentf-max-saved-items 25)
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Prevent shell prompt deletion
(setq comint-prompt-read-only t)

;; Basic config stuff
;; User customizations
(global-set-key (kbd "<f1>") nil) ;; Disable f1 key for treemacs
(setq-default case-fold-search nil) ;; No search case
(setq scroll-preserve-screen-position `t)
(setq user-mail-address "smakey18@gmail.com") ;; Default email
(tool-bar-mode -1)                             ; No toolbar
(scroll-bar-mode -1)                           ; No scrollbar
(menu-bar-mode -1)                             ; No menubar
(setq inhibit-startup-message t)               ; No message at startup
;;(setq visible-bell t)                          ; No beep when reporting errors
(setq ring-bell-function 'ignore)              ; no bell
(global-hl-line-mode t)                        ; Highlight cursor line
(setq-default indent-tabs-mode nil)            ; Use spaces instead of tab
(show-paren-mode 1)                            ; Highlight parenthesis pairs
;; (windmove-default-keybindings)                 ; Shift arrows switch windows
(defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
(blink-cursor-mode 0)                          ; No blinking cursor
;;(add-to-list 'default-frame-alist
;;                       '(font . "Hack-11"))
;;(set-face-attribute 'default t :font "Hack-10" )
;;(set-frame-font "Fira Code Retina-12" t)
;; (setq blink-matching-paren-distance nil)       ; Blinking parenthesis
;; (setq show-paren-style 'expression)            ; Highlight text between parens
	
;; Line number
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers but excempting certain major modes defined in `display-line-numbers-exempt-modes'."
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))
(global-display-line-numbers-mode)

;; Kind of fuzzy matching native
;;(ido-mode t)
;;(setq ido-everywhere t)
;;(setq ido-enable-flex-matching t)

;; Another fuzzy searching thing
;;(use-package helm
;;  :config (helm-mode 1))

;; Ivy fuzzy searcher again
(use-package ivy
  :config (ivy-mode 1))
(use-package counsel)
(use-package swiper)
(use-package counsel-tramp)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c f") 'counsel-describe-function)
(global-set-key (kbd "C-c v") 'counsel-describe-variable)
(global-set-key (kbd "C-c o") 'counsel-describe-symbol)
(global-set-key (kbd "C-c l") 'counsel-find-library)
(global-set-key (kbd "C-c i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-c l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-x d") 'counsel-dired)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(use-package ivy-hydra)
(setq tramp-default-method "ssh")
(setq tramp-terminal-type "dumb")
(define-key global-map (kbd "C-c C-t") 'counsel-tramp)

;; Disable line highlight in vterm
(add-hook 'vterm-mode-hook (lambda ()
                            (setq-local global-hl-line-mode
                                        nil)))

(use-package restart-emacs)

(use-package projectile
  :config (projectile-mode 1))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(use-package counsel-projectile)
(setq projectile-project-search-path '("~/Projects/"))
(setq projectile-completion-system 'ivy)

;; Dashboard
(use-package page-break-lines)
(use-package dashboard
  :config
  (setq dashboard-items '((recents . 5)
                        (projects . 5)
                        (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

;; Company Autocompletion
(use-package company
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :config
  ;; Provide instant autocompletion.
  (setq company-idle-delay 0)
  ;; Number of characters to activate
  (setq company-minimum-prefix-length 1)
  ;; Use Company mode everywhere.
  (global-company-mode t))

;; Indent guide
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-delay 0)
  (setq highlight-indent-guides-character ?│))

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t) ;; Warning rebind conflict!
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-easymotion)
(evilem-default-keybindings "C-,")

(use-package flycheck
  :init (global-flycheck-mode))

;; LSP
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map)

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred))
         ;; if you want which-key integration
         ;;(lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-delay 2))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)

;; Python environment
(use-package transient)
(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (pyvenv-mode 1))

(use-package poetry
  ;; :mode ("\\.py\\'" . python-mode)
  ;; :interpreter ("python" . python-mode)
  :config
  (poetry-tracking-mode))

;; Language specific
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :after (poetry)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))  ; or lsp-deferred


(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 40
        centaur-tabs-set-icons t
        centaur-tabs-set-bar 'left
        ;; x-underline-at-descent-line t
        centaur-tabs-set-modified-marker t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "cantarell" 130)
  (centaur-tabs-mode 1)
  :bind
  (:map evil-normal-state-map
	     ("g t" . centaur-tabs-forward)
	     ("g T" . centaur-tabs-backward)))

;; Use ipython for shell
;; (setq python-shell-interpreter "ipython"
;;         python-shell-interpreter-args "--simple-prompt -i")

;; (use-package direnv
;;   :config
;;   (direnv-mode))

;; Create directory if not made yet when opening a new file
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("M-0"       . treemacs-select-window)
        ("<f1>" . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  )

(use-package treemacs-projectile
  :after treemacs projectile
  )

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  )

(use-package treemacs-persp
  :after treemacs persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

;; ;; Like spacemacs
;; (use-package major-mode-hydra
;;   :ensure t
;;   :bind
;;   ("s-SPC" . major-mode-hydra))

;; (setq major-mode-hydra-title-generator
;;       '(lambda (mode)
;;          (s-concat "\n"
;;                    (s-repeat 10 " ")
;;                    (all-the-icons-icon-for-mode mode :v-adjust 0.05)
;;                    " "
;;                    (symbol-name mode)
;;                    " commands")))

;; (setq major-mode-hydra-invisible-quit-key "q")

;; (major-mode-hydra-define emacs-lisp-mode nil
;;   ("Eval"
;;    (("b" eval-buffer "buffer" :color red)
;;     ("e" eval-defun "defun" :color red)
;;     ("r" eval-region "region" :color red))
;;    "REPL"
;;    (("I" ielm "ielm" :color red))
;;    "Test"
;;    (("t" ert "prompt" :color red)
;;     ("T" (ert t) "all" :color red)
;;     ("F" (ert :failed) "failed" :color red))
;;    "Doc"
;;    (("d" describe-foo-at-point "thing-at-pt" :color red)
;;     ("f" describe-function "function" :color red)
;;     ("v" describe-variable "variable" :color red)
;;     ("i" info-lookup-symbol "info lookup" :color red))
;;    "Emacs"
;;    (("R" restart-emacs "Restart" :color red)
;;     ("Q" save-buffers-kill-terminal "Quit" :color red))))

;; (use-package helpful
;;   :pretty-hydra
;;   ((:color teal :quit-key "q")
;;    ("Helpful"
;;     (("f" helpful-callable "callable")
;;      ("v" helpful-variable "variable")
;;      ("k" helpful-key "key")
;;      ("c" helpful-command "command")
;;      ("d" helpful-at-point "thing at point"))))
;;   :bind ("C-h" . helpful-hydra/body))

;; (use-package elisp-mode
;;   :ensure t
;;   :mode "\\.el\\'"
;;   :mode-hydra
;;   (go-mode
;;    (:title "Elisp Commands")
;;    ("Doc"
;;     (("d" eval-buffer "doc at point"))
;;     "Imports"
;;     (("ia" eval-defun "add")
;;      ("ir" eval-region "cleanup")))))

(use-package hydra
  :defer t
  :bind
  ("M-p" . hydra-projectile/body)
  ("C-c C-f" . hydra-flycheck/body)
  ("C-c s" . hydra-system/body)
  :custom
  (hydra-default-hint nil))

(defhydra hydra-projectile (:color blue)
  "
^
^Projectile^        ^Buffers^           ^Find^              ^Search^
^──────────^────────^───────^───────────^────^───^─^
_q_ quit            _b_ list            _d_ directory       _r_ replace
_i_ reset cache     _K_ kill all        _D_ root            _R_ regexp replace
^^                  _S_ save all        _f_ file            _s_ rg
^^                  ^^                  _p_ project         ^^
^^                  ^^                  ^^                  ^^
"
  ("q" nil)
  ("b" counsel-projectile-switch-to-buffer)
  ("d" counsel-projectile-find-dir)
  ("D" projectile-dired)
  ("f" counsel-projectile-find-file)
  ("i" projectile-invalidate-cache :color red)
  ("K" projectile-kill-buffers)
  ("p" counsel-projectile-switch-project)
  ("r" projectile-replace)
  ("R" projectile-replace-regexp)
  ("s" counsel-projectile-rg)
  ("S" projectile-save-project-buffers))

(defhydra hydra-flycheck (:color blue)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^─────^
_q_ quit            _<_ previous        _?_ describe
_m_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _s_ select
^^                  _l_ list            ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-manual)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))

(defhydra hydra-system (:color blue)
  "
^
^System^            ^Packages^          ^Processes^         ^Shell^
^──────^────────────^────────^──────────^─────────^─────────^─────^─────────────
_q_ quit            _p_ list            _s_ list            _e_ eshell
_Q_ quit Emacs      _P_ upgrade         ^^                  _t_ vterm
_R_ restart Emacs   ^^                  ^^                  _T_ ansi-term
"
  ("q" nil)
  ("Q" save-buffers-kill-terminal)
  ("R" restart-emacs)
  ("e" (eshell t))
  ("p" paradox-list-packages)
  ("P" paradox-upgrade-packages)
  ("s" list-processes)
  ("t" vterm)
  ("T" ansi-term))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(elisp-mode posframe hydra-posframe helpful major-mode-hydra treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil centaur-tabs direnv pyvenv lsp-python-ms dap-mode lsp-ivy lsp-ui lsp-mode flycheck-status-emoji flycheck highlight-indent-guides evil-easymotion evil-commentary evil-surround restart-emacs evil-collection company ivy-hydra counsel-projectile projectile helm vterm doom-modeline doom-themes all-the-icons which-key use-package)))
 ;; Start fullscreen

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
