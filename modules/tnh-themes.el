;;; tnh-themes.el --- Default configuration for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-org-config))

(provide 'tnh-themes)

;;; tnh-themes.el end here
