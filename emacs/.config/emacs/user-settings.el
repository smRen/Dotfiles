;;; package --- Summary
;;; Commentary:
;;; User settings"

;;; Code:
(setq user-full-name "Ren Odion"
      user-mail-address "smakey18@gmail.com")

;; Preserve history
(savehist-mode t)

;; Recent files
(recentf-mode t)

;; Emoji
(set-fontset-font t 'symbol "Noto Color Emoji")

(let ((font-size 13) ;; Default font size of 13
      (height (display-pixel-height))
      (width (display-pixel-width)))
  (if (and (>= height 1440) (>= width 2560))
      (setq font-size 10))
  (add-to-list 'default-frame-alist
               `(font . ,(format "Hack-%d" font-size))))

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

(defun display-startup-echo-area-message ()
  "Initial message"
  (message "Welcome elite hacker"))

;; Disable initial screen
(setq inhibit-startup-screen t)

;; Make maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Scroll
(setq scroll-conservatively 101)

;; Show matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode t)

;; Disable toolbar, menubar, and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Stop annoying message about following symlinks
(setq vc-follow-symlinks t)

;; Delimit non-blank lines
(setq-default indicate-empty-lines t)

;; Stop the anacronism
(setq sentence-end-double-space nil)

;; Show proper newline formatting
(setq-default indicate-buffer-boundaries 'left)

;; Banish the tabs and make tabspace sensible
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Programming
(setq c-basic-offset 4
      js-indent-level 2
      css-indent-offset 2)

;;; user_settings.el ends here