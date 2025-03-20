(eval-when-compile
  (require 'use-package))

(use-package
 corfu
 :ensure t
 :bind
 (:map
  corfu-map ("<esc>" . corfu-quit) ("C-f" . corfu-quick-complete))
 :custom
 (text-mode-ispell-word-completion nil)
 (corfu-auto t)
 (corfu-auto-delay 0.1)
 (corfu-auto-prefix 2)
 (corfu-count 20)
 (corfu-cycle t)
 (corfu-preselect 'first)
 (corfu-preview-currect t)
 (corfu-quit-at-boundary t)
 (corfu-quit-no-match t)

 :init
 (global-corfu-mode)
 (corfu-indexed-mode)
 (corfu-history-mode)
 (corfu-echo-mode))


(use-package
 kind-icon
 :ensure t
 :after corfu
 :functions kind-icon-margin-formatter
 :config
 (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
 :custom (kind-icon-default-face 'corfu-default))

(use-package
 orderless
 :ensure t
 :config (setq completion-styles '(orderless)))

(provide 'tnh-emacs-corfu)
