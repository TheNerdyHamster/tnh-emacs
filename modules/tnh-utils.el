;;; tnh-utils.el --- Elisp helper functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (gnus-yes-or-no-p (format "Really delete '%s'?"
				  (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(provide 'tnh-utils)

;;; tnh-utils.el ends here
