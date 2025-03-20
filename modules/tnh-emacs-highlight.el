(eval-when-compile
  (require 'use-package))

(use-package
 highlight-indent-guides
 :ensure t
 :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package
 rainbow-delimiters
 :ensure t
 :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :ensure t :hook (org-mode emacs-lisp-mode))

(use-package
 smartparens
 :ensure t
 :hook (prog-mode . smartparens-mode)
 :config (sp-use-smartparens-bindings))

(provide 'tnh-emacs-highlight)
