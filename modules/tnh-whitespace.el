(setq-default show-trailing-whitespace nil)

(defun tnh/show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'tnh/show-trailing-whitespace))

(provide 'tnh-whitespace)
