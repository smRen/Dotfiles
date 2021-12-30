;; Non-default shell in macos
(when (eq system-type 'darwin)
    (setenv "SHELL" "/usr/local/bin/bash")
    (setq shell-file-name "/usr/local/bin/bash")
    (add-to-list 'exec-path "/usr/local/bin/")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

;; Stop cl deprecation warning
(setq byte-compile-warnings '(cl-functions))
       
