;;; tnh-osx-keys.el --- MacOS key configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-mac*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq mouse-wheel-scroll-amount '(1
				    ((shift) . 5)
				    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others))


(provide 'tnh-osx-keys)

;;; tnh-osx-keys.el end here
