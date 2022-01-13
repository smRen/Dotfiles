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
    pdf-tools
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
    dap-mode
    ivy
    counsel
    ivy-rich
    ivy-prescient
    lsp-ivy
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
    matlab-mode
    hydra)
  "List of third party packages")

;; Install packages
(let ((not-installed-packages (seq-remove #'package-installed-p smren/third-party-package-list)))
  (when not-installed-packages
    (package-refresh-contents)
    (mapc #'package-install not-installed-packages)
    (message "Installed the following packages: %s" not-installed-packages)))

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
