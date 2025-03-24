;; init.el --- Load the configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; The init file bootstraps the full configuration,
;;; from different types of modules

;;; Code:

(require 'tnh-benchmark) ;; Measure startup time.

(defconst *spell-check-support-enabled* nil)
(defconst *is-mac* (eq system-type 'darwin))

;; Process performance tuning
(setq fast-read-process-output (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bootstrap

(require 'tnh-utils)
(require 'tnh-packages)

(use-package gcmh
  :ensure t
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
			       (gcmh-mode)
			       (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)

(use-package diminish
  :ensure t)

(use-package scratch
  :ensure t)

(use-package command-log-mode
  :ensure t)

(require 'tnh-frame-hooks)
(require 'tnh-themes)
(require 'tnh-osx-keys)
(require 'tnh-gui-frames)
(require 'tnh-icons)
(require 'tnh-dired)
(require 'tnh-isearch)
(require 'tnh-grep)
(require 'tnh-uniquify)
(require 'tnh-flymake)
(require 'tnh-eglot)


(require 'tnh-vertico)
(require 'tnh-embark)
(require 'tnh-marginalia)
(require 'tnh-consult)
(require 'tnh-corfu)
(require 'tnh-which-key)
(require 'tnh-windows)
(require 'tnh-whitespace)

(require 'tnh-git)

(require 'tnh-projectile)

(require 'tnh-compile)
(require 'tnh-markdown)
(require 'tnh-go)
(require 'tnh-yaml)
(require 'tnh-terraform)
(require 'tnh-nix)

(require 'tnh-folding)

(use-package treesit
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :config
  (require 'tnh-treesitter))



