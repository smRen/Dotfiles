;;; Install script --- Summary
;;; Commentary:
;;; Install script for automation

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
    json-mode
    emmet-mode
    magit
    sly
    doom-themes
    doom-modeline
    format-all
    writeroom-mode
    smartparens
    org-contrib
    hydra)
  "List of third party packages")

;; Install packages
(let ((not-installed-packages (seq-remove #'package-installed-p smren/third-party-package-list)))
  (when not-installed-packages
    (package-refresh-contents)
    (mapc #'package-install not-installed-packages)
    (message "Installed the following packages: %s" not-installed-packages)))

(provide 'install)
;;; install.el ends here
