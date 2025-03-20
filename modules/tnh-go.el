(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook ((before-save . gofmt-before-save)
         (go-mode . eglot-ensure)
	 (go-mode . (lambda ()
		      (setq tab-width 4)
		      (setq indent-tabs-mode t))))
  :custom
  (gofmt-command "gofumpt")
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))))

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(provide 'tnh-go)
