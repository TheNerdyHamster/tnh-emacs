(eval-when-compile
  (require 'use-package))

(use-package ansible
  :ensure t
  :bind (("C-c a d" . 'ansible-decrypt-buffer)
	 ("C-c a e" . 'ansible-encrypt-buffer)))

(provide 'tnh-ansible)
