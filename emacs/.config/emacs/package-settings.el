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

;; Magit
(smren/require-pack '(magit))
(global-set-key (kbd "C-c g") #'magit)

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
(smren/require-pack '(lsp-mode))
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-icons-enable nil
      lsp-keymap-prefix "C-c l"
      lsp-enable-snippet nil)
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

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

;; Completion system
(ivy-mode 1)
(counsel-mode 1)
(ivy-rich-mode t)
(ivy-rich-project-root-cache-mode 1)
(ivy-prescient-mode 1)

(setq ivy-rich-path-style 'abbrev
      ivy-use-virtual-buffers t
      enable-recursive-minibuffers t
      ivy-re-builders-alist '((swiper . ivy--regex-plus)
                              (t . ivy--regex-fuzzy)))
(global-set-key (kbd "C-s") #'swiper)

;; Execute commands in vterm
(smren/require-pack '(vterm))

(defun smren/get-project-vterm-buffer ()
  "Return the vterm buffer of current project in buffer list.
Otherwise return nil"
  (interactive)
  (car (member (format "*vterm %s*" (projectile-project-name))
               (mapcar #'buffer-name (buffer-list)))))

(defun smren/vterm-execute-cmd-in-project (command)
  "Execute shell command in current project"
  (let ((cur-buf (current-buffer))
        (vterm-buf (smren/get-project-vterm-buffer)))
    (unless vterm-buf
      (projectile-run-vterm)
      (setq vterm-buf (smren/get-project-vterm-buffer)))
    (display-buffer vterm-buf t)
    (switch-to-buffer-other-window vterm-buf)
    (vterm--goto-line -1)
    (message "Executing shell command: %s" command)
    (vterm-send-string command)
    (vterm-send-return)
    (switch-to-buffer-other-window cur-buf)))

;; Make keymaps
(defun smren/run-make-configure-in-project ()
  "Run the make configure in current projectile root"
  (interactive)
  (smren/vterm-execute-cmd-in-project "make configure"))

(defun smren/run-make-build-in-project ()
  "Run the make build in current projectile root"
  (interactive)
  (smren/vterm-execute-cmd-in-project "make build"))

(defun smren/run-make-test-in-project ()
  "Run the make test in current projectile root"
  (interactive)
  (smren/vterm-execute-cmd-in-project "make test"))

(defun smren/run-make-run-in-project ()
  "Run the make run in current projectile root"
  (interactive)
  (smren/vterm-execute-cmd-in-project "make run"))

(defun smren/run-make-clean-in-project ()
  "Run the make clean in current projectile root"
  (interactive)
  (smren/vterm-execute-cmd-in-project "make clean"))

(defun smren/run-custom-command-in-project (command)
  "Prompt for running a custon command at projectile root"
  (interactive "sEnter command to run in Vterm shell:")
  (smren/vterm-execute-cmd-in-project command))

(defalias 'smren/personal-commands
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'smren/run-make-configure-in-project)
    (define-key map (kbd "c") 'smren/run-make-build-in-project)
    (define-key map (kbd "P") 'smren/run-make-test-in-project)
    (define-key map (kbd "u") 'smren/run-make-run-in-project)
    (define-key map (kbd "k") 'smren/run-make-clean-in-project)
    (define-key map (kbd "&") 'smren/run-custom-command-in-project)
    map)
  "Make related bindings")

(global-set-key (kbd "C-c m") 'smren/personal-commands)

;; Writeroom mode
(global-set-key (kbd "C-c w") #'writeroom-mode)

;; Org-mode
(setq org-export-backends '(ascii html icalendar latex md odt))

;; PDF viewer
(smren/require-pack '(pdf-tools))
(setq pdf-view-display-size 'fit-height)

;;; package-settings.el ends here
