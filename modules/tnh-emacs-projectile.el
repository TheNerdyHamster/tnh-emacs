(eval-when-compile
  (require 'use-package))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/Documents/code/github.com/" "~/.emacs.d"))
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config
  (setq projectile-auto-discover nil)
  (projectile-mode +1))


(provide 'tnh-emacs-projectile)
