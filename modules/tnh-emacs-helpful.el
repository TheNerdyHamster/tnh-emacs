(eval-when-compile
  (require 'use-package))

(use-package helpful
  :ensure t
  :bind
  (([remap describe-function] . helpful-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap desctibe-key] . helpful-key)))

(provide 'tnh-emacs-helpful)
