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

(dolist (filename '("packages.el" "user-settings.el"))
  (load-file (expand-file-name filename user-emacs-directory)))

(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (not (file-exists-p custom-file))
    (format "%s doesn't exist...creating one" custom-file)
    (if (with-temp-buffer (write-file custom-file))
	(message "Custom.el created")))
  (load custom-file))


(provide 'init)
;;; init.el ends here
