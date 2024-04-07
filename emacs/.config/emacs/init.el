;; Start emacs maximized
;;(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Make new emacs frames maximized
;;(add-hook 'after-make-frame-functions 'toggle-frame-maximized)

;; Load teme
(add-hook 'after-init-hook (lambda ()
			     (load-theme 'wheatgrass t)))

;; Show line numbers globally
(global-display-line-numbers-mode 1)

;; Disable menu bar and file menu
(menu-bar-mode -1)
