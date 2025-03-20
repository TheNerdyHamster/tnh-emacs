;;; tnh-benchmark.el --- Startup and benchmark information -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Startup time hook
(defun tnh/display-startup-info ()
  (message "TNH-Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'tnh/display-startup-info)

(provide 'tnh-benchmark)

;;; tnh-benchmark.el end here
