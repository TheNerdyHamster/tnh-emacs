(defcustom tnh-projectile-search-paths
  `()
  "List of paths Projectile should scan for projects."
  :type '(repeat directory)
  :group 'tnh)

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-project-search-path
        (append (list (expand-file-name user-emacs-directory))
                (mapcar #'expand-file-name tnh-projectile-search-paths)))
  :custom
  (projectile-mode-line-prefix " Proj")
  (projectile-max-depth 10)
  (projectile-generic-command (when (executable-find "rg") "rg --files --hidden -0"))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package ibuffer-projectile
  :ensure t
  :after projectile)

(provide 'tnh-projectile)
