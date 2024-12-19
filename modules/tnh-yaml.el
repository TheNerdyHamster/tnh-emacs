(eval-when-compile
  (require 'use-package))

(use-package
 yaml-mode
 :ensure t
 :config
 (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(provide 'tnh-yaml)
