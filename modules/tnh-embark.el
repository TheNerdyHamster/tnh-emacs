(use-package embark
 :ensure t
 :defer t
 :bind
 (("C-." . embark-act)
  ("M-." . embark-dwim)
  :map
  minibuffer-local-map
  ("C-d" . embark-act)
  :map
  embark-region-map
  ("D" . denote-region))
 :custom
 (embark-action-indicator
  (lambda (map _target)
    (which-key--show-keymap "Embark" map nil nil 'no-paging)
    #'which-key--hide-popup-ignore-command)
  embark-become-indicator embark-action-indicator)
 :config (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
 :ensure t
 :after (embark consult)
 :hook (embark-collect-mode . consult-pnreview-at-point-mode))

(provide 'tnh-embark)
