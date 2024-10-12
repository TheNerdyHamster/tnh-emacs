(eval-when-compile
  (require 'use-package))

(use-package
 elisp-autofmt
 :ensure t
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :config
 (setq elisp-autofmt-load-packages-local '("use-package")))

(provide 'tnh-elisp)
