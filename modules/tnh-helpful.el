(eval-when-compile
  (require 'use-package))

(use-package
 helpful
 :ensure t
 :bind
 (([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap desribe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)))

(provide 'tnh-helpful)
