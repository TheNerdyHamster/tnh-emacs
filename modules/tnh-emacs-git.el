(eval-when-compile
  (require 'use-package))

(use-package magit
  :ensure t
  :bind
  (("C-M-;" . magit-status-here)
   ("C-c C-g" . magit-status-here)))

(use-package git-modes
  :ensure t)

(use-package magit-delta
  :ensure t
  :hook (magit-modde . magit-detla-mode))

(use-package git-gutter
  :ensure t
  :bind
  (("C-c [" . git-gutter:next-hunk)
   ("C-c ]" . git-gutter:previous-hunk))
  :config
  (global-git-gutter-mode))

(provide 'tnh-emacs-git)
