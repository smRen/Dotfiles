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
    evil-surround
    evil-commentary
    company
    lsp-mode
    lsp-pyright
    flycheck
    rg
    ag
    ;; dap-mode
    projectile
    cmake-mode
    vterm
    json-mode
    emmet-mode
    magit
    sly
    doom-themes
    doom-modeline
    format-all
    writeroom-mode
    smartparens
    hydra)
  "List of third party packages")

;; Install packages
(dolist (package smren/third-party-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load settings
(dolist (filename '("user-settings.el" "package-settings.el"))
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
