(defun tnh-emacs/dig (host type)
  (interactive "sHost: \nsType: ")
  (dig host type))

;; Vterm
(defun tnh-emacs/vterm-open (name)
  (interactive "sSession name: ")
  (vterm (format "term-%s" name)))

(provide 'tnh-functions)
