;;; package-settings --- Summary
;;; Commentary:
;;; Settings for third party packages"

;;; Code:

(defun smren/require-pack (package-list)
  "Function to require third party packages"
  (dolist (package-name package-list)
    (require package-name)))

;; Evil settings
(setq evil-want-C-u-scroll t
      evil-want-integration t
      evil-want-keybinding nil
      evil-motion-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-emacs-state-cursor  'hbar
      evil-collection-company-use-tng t
      evil-undo-system 'undo-redo)

(smren/require-pack '(evil evil-collection))
(evil-collection-init)
(evil-mode 1)
(global-evil-surround-mode 1)
(evil-commentary-mode 1)

(evil-terminal-cursor-changer-activate)
(global-set-key (kbd "C-c v") #'vterm)

;; Company (autocomplete)
(global-company-mode)
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)
(setq company-ide-delay 0
      company-selection-wrap-around t
      company-minimum-prefix-length 1
      company-echo-delay 0)

;; Theme
(smren/require-pack '(doom-themes))
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-challenger-deep t)
(doom-modeline-mode 1)


;; Format all
(smren/require-pack '(format-all))
(setq-default format-all-formatters '(("JSX" prettier)
                                      ("HTML" prettier)
                                      ("C++" (clang-format "-style={IndentWidth: 4}"))
                                      ("Python" black)
                                      ("JavaScript" prettier)
                                      ("Shell" (shfmt "-i" "2")))
              format-all-show-errors 'warning)
(define-key prog-mode-map (kbd "C-c =") #'format-all-buffer)

;; Smartparens
(smren/require-pack '(smartparens smartparens-config))
(add-hook 'prog-mode-hook #'smartparens-mode)

;; Lsp mode
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-icons-enable nil
      lsp-keymap-prefix "C-c l"
      lsp-enable-snippet nil)

(dolist (mode-hook '(js-mode-hook
                     c++-mode-hook
                     c-mode-hook
                     html-mode-hook
                     css-mode-hook
                     sh-mode-hook
                     python-mode-hook))
  (add-hook mode-hook 'lsp))

;; Flycheck
(add-hook 'flycheck-mode-hook
          (lambda ()
            "Add extra linters/checkers after lsp"
            (let ((current-mode major-mode))
              (cond ((eq current-mode 'js-mode) (flycheck-add-next-checker 'lsp 'javascript-eslint))
                    ((memq current-mode '(c++-mode c-mode)) (flycheck-add-next-checker 'lsp 'c/c++-cppcheck))))))

;; Lsp pyright
(setq lsp-pyright-venv-path ".venv"
      lsp-pyright-python-executable-cmd ".venv/bin/python")

;; Projectile
(smren/require-pack '(ag rg))
(setq projectile-project-search-path '("~/Projects")
      projectile-indexing-method 'alien
      projectile-enable-caching t)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Emmet-mode
(smren/require-pack '(emmet-mode))
(setq emmet-expand-jsx-className? t
      emmet-move-cursor-after-expanding t
      emmet-move-cursor-between-quotes t
      emmet-indent-after-insert nil)

(dolist (mode-hook '(js-mode-hook
                     js-jsx-mode
                     html-mode-hook
                     css-mode-hook))
  (add-hook mode-hook 'emmet-mode))

;;; package-settings.el ends here
