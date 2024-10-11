(add-to-list 'custom-theme-load-path (expand-file-name "themes/" tnh-emacs/emacs-config-directory))

(use-package nord-theme
	:ensure t)

(load-theme 'nord t)
(provide 'tnh-emacs-theme)
