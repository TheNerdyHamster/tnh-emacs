(use-package hydra
	:ensure t)

;; Git
(defvar tnh-emacs/git-prefix-map (make-sparse-keymap)
  "Keymap for git operations.")

(global-set-key (kbd "C-c g") tnh-emacs/git-prefix-map)

;; Emacs general stuff

;; Custom emacs operations bindings
(defhydra tnh-emacs/emacs-hydra ()
	"Keymap for emacs general operations")

(global-set-key (kbd "C-c s") 'tnh-emacs/emacs-hydra/body)

(provide 'tnh-emacs-keys)
