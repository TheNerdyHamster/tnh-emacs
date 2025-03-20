(use-package flymake
  :ensure t
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! c" . flymake-start))
  :config
  (unless (version< emacs-version "28.1")
    (setq eldoc-documentation-function 'eldoc-documentation-compose)
    (add-hook 'flymake-mode-hook
              (lambda ()
                (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t)))))

(use-package flymake-flycheck
  :ensure t
  :after flycheck
  :hook (flymake-mode . flymake-flycheck-auto)
  :config
  (setq-default
   flycheck-disabled-checkers
   (append (default-value 'flycheck-disabled-checkers)
           '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck))))

(provide 'tnh-flymake)
