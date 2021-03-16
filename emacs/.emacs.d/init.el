(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cmake-mode c++ undo-fu lua-mode mu4e smartparens yasnippet-snippets which-key vterm use-package tree-sitter-langs restart-emacs realgud projectile poetry org-bullets magit lsp-ui lsp-pyright lsp-ivy json-mode ivy-prescient hydra general format-all flycheck exec-path-from-shell evil-terminal-cursor-changer evil-surround evil-matchit evil-commentary evil-collection emmet-mode doom-modeline counsel company-box ayu-theme avy all-the-icons-ivy-rich)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
