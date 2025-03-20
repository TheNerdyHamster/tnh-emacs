(eval-when-compile
  (require 'use-package))

(use-package vertico
  :ensure t
  :custom (vertico-cycle t)
  :custom-face (vertico-current ((t (:background "#3a3f5a"))))
  :config
  (require 'vertico-directory)
  (vertico-mode))

(provide 'tnh-emacs-vertico)
