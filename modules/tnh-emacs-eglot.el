(eval-when-compile
  (require 'use-package))

(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-r" . eglot-rename))

  :config
  (setq eglot-autoshutdown t))

(provide 'tnh-emacs-eglot)
