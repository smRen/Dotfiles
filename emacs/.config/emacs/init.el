;;; package --- Summary
;;; Commentary:
;;; Initialize settings"

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Third party packages
(defvar smren/third-party-package-list
  '(evil
    evil-terminal-cursor-changer
    evil-collection
    ;; evil-surround
    ;; evil-commentary
    ;; company
    ;; lsp-mode
    ;; lsp-pyright
    ;; flycheck
    ;; dap-mode
    ;; lsp-ui
    ;; cmake-mode
    ;; vterm
    ;; json-mode
    ;; emmet-mode
    ;; magit
    ;; sly
    ;; doom-themes
    ;; format-all
    ;; writeroom-mode
    ;; doom-modeline
    ;; smartparens
    ;; which-key
    ;; hydra)
  )
  "List of third party packages")

(defun smren/get-not-installed-packages (package-list)
  "Get packages that are not installed from third-party-package-list"
  (seq-filter (lambda (package) (not (package-installed-p package))) package-list))

;; Install packages
(when-let ((not-installed-packages (smren/get-not-installed-packages smren/third-party-package-list)))
  (package-refresh-contents)
  (dolist (package-name not-installed-packages)
    (package-install package-name))
  (message "Installed the following packages:\n%s" not-installed-packages))

;; Load settings
(dolist (filename '("package-settings.el" "user-settings.el"))
  (load-file (expand-file-name filename user-emacs-directory)))

;; Move customizations to its own file
(let ((custom-file-name (expand-file-name "custom.el" user-emacs-directory)))
  (when (not (file-exists-p custom-file-name))
    (format "%s doesn't exist...creating one" custom-file-name)
    (if (with-temp-buffer (write-file custom-file-name))
	(message "Custom.el created")))
  (setq custom-file custom-file-name))
(load custom-file)

(provide 'init)
;;; init.el ends here
