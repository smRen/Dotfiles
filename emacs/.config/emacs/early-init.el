;;; package --- Summary
;;; Commentary:
;;; "MacOS specific settings and disable Common Lisp warnings"
;;; Code:

;; Non-default shell in macos
(when (eq system-type 'darwin)
    (setenv "SHELL" "/usr/local/bin/bash")
    (setq shell-file-name "/usr/local/bin/bash"
          insert-directory-program "/usr/local/bin/gls")
    (dolist (path '("/usr/local/bin" "/Library/TeX/texbin"))
      (push path exec-path))
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin:/usr/local/bin")))

;; Stop CL deprecation warning
(setq byte-compile-warnings '(not obsolete))

;; Disable annoying backups
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)


(provide 'early-init)
;;; early-init.el ends here
