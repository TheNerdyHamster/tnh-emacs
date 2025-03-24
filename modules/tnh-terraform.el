(use-package terraform-mode
  :ensure t)

(use-package reformatter
  :ensure t
  :config
  (reformatter-define terraform-format
		      :program "terraform"
		      :args '("fmt" "-")))

(provide 'tnh-terraform)
