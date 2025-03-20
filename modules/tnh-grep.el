(setq-default grep-highlight-matches t
	      grep-scroll-output t)

(when *is-mac*
  (setq-default locate-command "mdfind"))

(use-package wgrep
  :ensure t)
(with-eval-after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(when (and (executable-find "ag")
           (maybe-require-package 'ag))
  (use-package wgrep-ag
    :ensure t
    :config
    (setq-default ag-highlight-search t))
  (global-set-key (kbd "M-?") 'ag-project))

(when (and (executable-find "rg")
           (use-package rg
	     :ensure t))
  (global-set-key (kbd "M-?") 'rg-project))

(provide 'tnh-grep)
