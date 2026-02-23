;;; tnh-makefile.el --- Makefile configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package make-mode
  :ensure nil
  :mode (("[Mm]akefile'" . makefile-gmake-mode)
         ("\.make'" . makefile-gmake-mode)
         ("\.mk'" . makefile-gmake-mode))
  :hook (makefile-mode . (lambda ()
                           (setq indent-tabs-mode t)
                           (setq tab-width 4)
                           (setq show-trailing-whitespace t))))

(provide 'tnh-makefile)

;;; tnh-makefile.el ends here
