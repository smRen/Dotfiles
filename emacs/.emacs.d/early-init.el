;;; package --- Summary
;;; Commentary:
;;; "MacOS specific settings and disable Common Lisp warnings"
;;; Code:

;; Non-default shell in macos
(when (eq system-type 'darwin)
    (setenv "SHELL" "/usr/local/bin/bash")
    (setq shell-file-name "/usr/local/bin/bash")
    (add-to-list 'exec-path "/usr/local/bin/")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

(provide 'early-init)
;;; early-init.el ends here
