;;; tnh-rfc.el -- Emacs font settings
;;; Commentary:
;;; Code:

(use-package rfc-mode
  :ensure t
  :config
  (setq rfc-mode-directory (expand-file-name "~/rfc/")))
(provide 'tnh-rfc)

;;; tnh-rfc.el ends here
