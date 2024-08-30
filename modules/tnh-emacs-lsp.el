(use-package eglot
	:bind (:map eglot-mode-map
							("C-c C-a" . eglot-code-actions)
							("C-c C-r" . eglot-rename))
	:config
	(setq eglot-autoshutdown t))

(use-package terraform-mode
	:ensure t
	:custom (terraform-indent-level 4)
	:config
	(defun tnh-emacs/terraform-mode-init ()
		(outline-minor-mode 1))
	(add-hook 'terraform-mode-hook 'tnh-emacs/terraform-mode-init))

(provide 'tnh-emacs-lsp)
