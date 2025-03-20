;;; tnh-gui-frames.el --- Behavior specific to non-TTY frames -*- lexical-binding: t -*-
;; Commentary:
;;; Code:

(defun tnh/maybe-suspend-frame ()
  "Stop minimizing windows under OS-X"
  (interactive)
  (unless (and *is-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'tnh/maybe-suspend-frame)

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when (and *is-mac* (fboundp 'toggle-frame-fullscreen))
  (global-set-key (kbd "s-f") 'toggle-frame-fullscreen))

(add-hook 'term-mode-hook
	  (lambda ()
	    (setq line-spacing 0)))

(when *is-mac*
  (use-package ns-auto-titlebar
    :ensure t
    :config
    (ns-auto-titlebar-mode)))

(use-package disable-mouse
  :ensure t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(provide 'tnh-gui-frames)

;;; tnh-gui-frames.el end here
