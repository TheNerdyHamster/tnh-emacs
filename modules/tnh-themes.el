;;; tnh-themes.el --- Default configuration for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; A modular way to load and change themes

;;; Code:

(defcustom tnh-current-theme 'modus-vivendi-tritanopia
  "Default theme"
  :type 'symbol
  :group 'tnh-themes)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (doom-themes-org-config))


(load-theme tnh-current-theme t)

(defun tnh/switch-theme ()
  "Interactively switch to a pre-defined theme and load it"
  (interactive)
  (let* ((availble-themes '(modus-vivendi-tritanopia doom-one modus-vivendi-deuteranopia doom-tokyo-night))
	 (choice (completing-read "Switch to theme: " availble-themes nil t)))
    (when choice
      (load-theme (intern choice) t))))

(provide 'tnh-themes)

;;; tnh-themes.el ends here
