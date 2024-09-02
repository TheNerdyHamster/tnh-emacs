;; Helpers
(defun define-key* (keymap &rest keys)
  "Define multiple keys in a keymap"
  (while keys
    (define-key keymap
		(kbd (pop keys))
		(pop keys))))

(put 'define-key* 'lisp-indent-function 1)

;; Git
(defvar tnh-emacs/git-prefix-map (make-sparse-keymap)
  "Keymap for git operations.")

(global-set-key ("C-c g") tnh-emacs/git-prefix-map)
