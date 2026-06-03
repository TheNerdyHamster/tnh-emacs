(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook ((before-save . gofmt-before-save)
         (go-mode . eglot-ensure)
         (go-mode . (lambda ()
                      (setq-local tab-width 4)
                      (setq-local indent-tabs-mode t))))
  :custom
  (gofmt-command "gofumpt")
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) . ("gopls")))))

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(provide 'tnh-go)
