(setq-default compilation-scroll-output t)

(use-package alert
  :ensure t)

(defun tnh/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (when (buffer-live-p buf)
    (unless (catch 'is-visible
              (walk-windows (lambda (w)
                              (when (eq (window-buffer w) buf)
                                (throw 'is-visible t))))
              nil)
      (alert (concat "Compilation " result)
             :buffer buf
             :category 'compilation))))

(defvar tnh/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(defun tnh/save-compilation-buffer (&rest _)
  "Save the compilation buffer to find it later."
  (setq tnh/last-compilation-buffer next-error-last-buffer))

(defun tnh/find-prev-compilation (orig &optional edit-command)
  "Find the previous compilation buffer, if present, and recompile there."
  (if (and (null edit-command)
           (not (derived-mode-p 'compilation-mode))
           tnh/last-compilation-buffer
           (buffer-live-p (get-buffer tnh/last-compilation-buffer)))
      (with-current-buffer tnh/last-compilation-buffer
        (funcall orig edit-command))
    (funcall orig edit-command)))

(defun tnh/shell-command-in-view-mode (&rest _)
  "Put `*Shell Command Output*` buffers into view-mode."
  (with-current-buffer "*Shell Command Output*"
    (view-mode 1)))

(defun tnh/colourise-compilation-buffer ()
  "Apply ANSI colors in the compilation buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(use-package compile
  :bind ([f6] . recompile)
  :hook (compilation-finish-functions . tnh/alert-after-compilation-finish)
  :config
  (advice-add 'compilation-start :after 'tnh/save-compilation-buffer)
  (advice-add 'recompile :around 'tnh/find-prev-compilation)
  (advice-add 'shell-command-on-region :after 'tnh/shell-command-in-view-mode))

(use-package ansi-color
  :hook (compilation-filter-hook . tnh/colourise-compilation-buffer))

(provide 'tnh-compile)
