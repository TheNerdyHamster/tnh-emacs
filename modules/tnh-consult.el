(eval-when-compile (require 'use-package))
(use-package consult
	:ensure t
	:bind (("C-x C-b" . consult-buffer)
				 :map minibuffer-local-map
				 ("C-r" . consult-history))
	:custom
	(completion-in-region-function #'consult-completion-in-region))

(use-package consult-dir
	:ensure t
	:bind (("C-x C-d" . consult-dir)
				 :map vertico-map
				 ("C-x C-d" . consult-dir)
				 ("C-x C-j" . consult-dir-jump-file))
	:custom
	(consult-dir-project-list-function nil))

(use-package wgrep
	:after consult
	:hook (grep-mode . wgrep-setup))

(provide 'tnh-consult)
