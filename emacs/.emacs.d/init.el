;;; package --- Summary
;;; Commentary:
;;; Initialize settings"

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(dolist (filename '("packages.el" "user_settings.el" "custom.el"))
  (load-file (expand-file-name filename user-emacs-directory)))
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ivy all-the-icons-ivy-rich ivy-rich ivy-prescient counsel-projectile counsel writeroom-mode which-key web-mode vterm use-package undo-fu tree-sitter-langs smartparens sly projectile magit lsp-ui lsp-pyright json-mode format-all flycheck evil-terminal-cursor-changer evil-surround evil-matchit evil-commentary evil-collection emmet-mode doom-themes doom-modeline company cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
