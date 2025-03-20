;;; tnh-dired.el --- Dired configuration -*- lexical-binding: t -*-
;;; Commentaru:
;;; Code:

(defun tnh/dired-mode-hook ()
  (interactive)
  (dired-hide-details-mode 1)
  (all-the-icons-dired-mode 1)
  (hl-line-mode 1))

(use-package
 dired
 :ensure nil
 :bind
 (:map
  dired-mode-map
  ("b" . dired-up-directory)
  ("H" . dired-hide-details-mode))
 :config
 (setq
  dired-listing-switches
  "-agho --time-style=long-iso --group-directories-first"
  dired-omit-verbose t
  dired-dwim-target t
  dired-hide-details-hide-symlink-targets nil
  dired-kill-when-opening-new-dired-buffer t
  delete-by-moving-to-trash t)
 (add-hook 'dired-mode-hook #'tnh/dired-mode-hook))

(provide 'tnh-dired)

;;; tnh-dired.el ends here
