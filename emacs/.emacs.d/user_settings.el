(setq user-full-name "Ren Odion"
      user-mail-address "smakey18@gmail.com")

;; Minibuffer stuff
(fido-vertical-mode 1)
(setq completion-styles '(initials partial-completion flex) 
      completion-cycle-threshold 10)
(savehist-mode 1)

;; Tabs
(global-tab-line-mode t)

;; Emoji
(set-fontset-font t 'symbol "Noto Color Emoji")

(add-to-list 'default-frame-alist
             '(font . "Hack-13"))
;; Nicer lambdas
(global-prettify-symbols-mode t)

;; Stop alarms
(setq ring-bell-function 'ignore)

;; Make C-x C-b go to choose buffer
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Disable annoying backups
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

;;Terminal cursor
(evil-terminal-cursor-changer-activate)

;;Stop getting prompts about killing a buffer with a live process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;;y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Intro message
(defun display-startup-echo-area-message ()
  (message "Welcome elite hacker"))

;; Disable initial screen
(setq inhibit-startup-screen t)

;; Make maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Org configs
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Scroll
(setq scroll-conservatively 101)

;; Show matching parenthesis
(show-paren-mode 1)

;; Disable toolbar, menubar, and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Stop annoying message about following symlinks
(setq vc-follow-symlinks t)

;; Programming

;; Javascript
;; Make js have indent of 2 spaces
;; (setq js2-basic-offset 2)
;; (setq js-indent-level 2)
