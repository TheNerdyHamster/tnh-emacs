;;; tnh-gemini.el --- Setup and configure gemini -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :ensure t)

(use-package gemini-cli
  :ensure t
  :vc (:url "https://github.com/linchen2chris/gemini-cli.el" :rev :newest)
  :config
  (setq gemini-cli-terminal-backend 'vterm)
  (gemini-cli-mode)
  :bind-keymap ("C-c c" . gemini-cli-command-map))

(provide 'tnh-gemini)

;;; tnh-gemini.el ends here
