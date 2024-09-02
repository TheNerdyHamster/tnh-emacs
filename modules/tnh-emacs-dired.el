(use-package all-the-icons
	:ensure t)

(use-package all-the-icons-dired
	:ensure t)

(defun tnh-emacs/dired-mode-hook ()
	(interactive)
	(dired-hide-details-mode 1)
	(all-the-icons-dired-mode 1)
	(hl-line-mode 1))

(use-package dired
	:ensure nil
	:bind (:map dired-mode-map
							("b" . dired-up-directory)
							("H" . dired-hide-details-mode))
	:config
	(setq dired-listing-switches "-agho --group-directories-first"
				dired-omit-verbose t
				dired-dwim-target t
				dired-hide-details-hide-symlink-targets nil
				dired-kill-when-opening-new-dired-buffer t
				delete-by-moving-to-trash t)
	(add-hook 'dired-mode-hook #'tnh-emacs/dired-mode-hook)) 

(provide 'tnh-emacs-dired)
