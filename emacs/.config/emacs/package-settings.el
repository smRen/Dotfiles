;;; package-settings --- Summary
;;; Commentary:
;;; Settings for third party packages"

;;; Code:

;; Evil settings
(setq evil-want-C-u-scroll t
      evil-want-integration t
      evil-want-keybinding nil
      evil-motion-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-emacs-state-cursor  'hbar
      evil-collection-company-use-tng t)

(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(evil-mode 1)
(evil-set-leader '(normal visual) (kbd "SPC")) ;; leader
(evil-set-leader '(normal visual) "," t) ;; local leader

(evil-define-key '(normal visual) 'global
  (kbd "<leader>v") 'vterm)

;;; package-settings.el ends here
