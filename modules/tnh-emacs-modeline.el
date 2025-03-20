(eval-when-compile
  (require 'use-package))

(use-package hide-mode-line
  :ensure t
  :init (setq hide-mode-line-excluded-modes nil))

(use-package mini-echo
  :ensure t
  :after hide-mode-line
  :config (mini-echo-mode 1))

(provide 'tnh-emacs-modeline)
