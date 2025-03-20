;;; tnh-frame-hooks.el --- Provide specific hooks for GUI/TTY frame creation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar after-make-console-frame-hook '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in repose to the newly-created FRAME.
Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
		   'after-make-window-system-frame-hooks
		   'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(provide 'tnh-frame-hooks)
;;; tnh-frame-hooks.el ends here
