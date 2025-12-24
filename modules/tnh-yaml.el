(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\.erb\\.yaml\\'" . yaml-mode)
  :hook (yaml-mode . goto-address-prog-mode))

(provide 'tnh-yaml)
