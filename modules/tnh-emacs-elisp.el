(eval-when-compile
  (require 'use-package))

(use-package elisp-autofmt
  :ensure t
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :config
  (setq elisp-autofmt-load-packages-local '("use-package")))

;; This is also within tnh-emacs-base module
(use-package emacs-lisp-mode
  :mode ("\\.el\\'" "\\.el.tmp\\'"))

(provide 'tnh-emacs-elisp)
