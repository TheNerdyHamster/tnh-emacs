(eval-when-compile
  (require 'use-package))

(use-package
 org
 :ensure t
 :config (setq org-export-allow-bind-keywords t))

(provide 'tnh-org)
