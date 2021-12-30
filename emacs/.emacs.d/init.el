;;; package --- Summary
;;; Commentary:
;;; Initialize settings"

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(dolist (filename '("packages.el" "user_settings.el" "custom.el"))
  (load-file (expand-file-name filename user-emacs-directory)))
(provide 'init)
;;; init.el ends here
