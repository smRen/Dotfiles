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
      evil-collection-company-use-tng t
      evil-undo-system 'undo-redo)

(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(evil-mode 1)
(global-evil-surround-mode 1)
(evil-commentary-mode 1)

(evil-terminal-cursor-changer-activate)
(evil-set-leader '(normal visual) (kbd "SPC")) ;; leader
(evil-set-leader '(normal visual) "," t) ;; local leader
(evil-define-key '(normal visual) 'global
  (kbd "<leader>v") 'vterm)

;; Company (autocomplete)

;;; package-settings.el ends here
