(setq tab-always-indent 'complete)

(use-package orderless
  :ensure t
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :config
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (corfu-terminal-mode))

(use-package
 kind-icon
 :ensure t
 :after corfu
 :functions kind-icon-margin-formatter
 :config
 (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
 :custom (kind-icon-default-face 'corfu-default))

(provide 'tnh-corfu)
