(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load-file (expand-file-name (concat user-emacs-directory "packages.el")))
(load-file (expand-file-name (concat user-emacs-directory "user_settings.el")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
