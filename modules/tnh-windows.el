(use-package switch-window
  :ensure t
  :bind
  (("C-x o" . switch-window))
  :config
  (setq-default switch-window-shortcut-style 'qwerty)
  (setq-default switch-window-timeout nil))

(provide 'tnh-windows)
