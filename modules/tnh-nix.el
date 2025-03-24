(use-package nix-ts-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode)))

(use-package nix-mode
  :ensure t
  :unless (package-installed-p 'nix-ts-mode))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               `((nix-mode nix-ts-mode) . ,(eglot-alternatives '("nixd" "nil")))))

(use-package nixpkgs-fmt
  :ensure t)

(use-package nixfmt
  :ensure t)

(provide 'tnh-nix)
