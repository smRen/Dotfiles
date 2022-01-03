;;; package --- Summary
;;; Commentary:
;;; "MacOS specific settings and disable Common Lisp warnings"
;;; Code:

;; Non-default shell in macos
(when (eq system-type 'darwin)
    (setenv "SHELL" "/usr/local/bin/bash")
    (setq shell-file-name "/usr/local/bin/bash"
          insert-directory-program "/usr/local/bin/gls")
    (add-to-list 'exec-path "/usr/local/bin/")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

;; Stop CL deprecation warning
(setq byte-compile-warnings '(not obsolete))

;; Disable annoying backups
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)


(provide 'early-init)
;;; early-init.el ends here
