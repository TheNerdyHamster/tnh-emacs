(defun tnh-emacs/dig (host type)
	(interactive "sHost: \nsType: ")
	(dig host type))

;; Vterm
(defun tnh-emacs/vterm-open (name)
	(interactive "sSession name: ")
	(vterm (format "term-%s" name)))

(defhydra+ tnh-emacs/emacs-hydra ()
	("t" tnh-emacs/vterm-open "Open new vterm session" :column "General"))

(provide 'tnh-emacs-custom)
