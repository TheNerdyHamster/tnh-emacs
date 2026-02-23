(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-r" . eglot-rename))
  :config
  (setq eglot-autoshutdown t))

(use-package consult-eglot
  :ensure t
  :after eglot)

(use-package consult-eglot
  :ensure t
  :after eglot)

(provide 'tnh-eglot)
