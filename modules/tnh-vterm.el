;;; tnh-vterm.el --- Default configuration for vterm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  :bind
  (("C-c t" . vterm)))

(provide 'tnh-vterm)

;;; tnh-vterm.el end here

