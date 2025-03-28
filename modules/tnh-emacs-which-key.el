(eval-when-compile
  (require 'use-package))

(use-package which-key
  :ensure t
  :defer 1
  :commands
  (which-key--show-keymap which-key--hide-popup-ignore-command)
  :custom
  (which-key-show-transient-maps t)
  :config
  (which-key-mode))

(provide 'tnh-emacs-which-key)
