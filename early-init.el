(setq frame-resize-pixelwise t
			frame-inhibit-implied-resize t
			frame-title-format '("%b")
			auto-window-vscroll nil
			byte-compile-warnings '(not obsolete)
			ring-bell-function 'ignore
			load-prefer-newer noninteractive
			package-enable-at-startup t
			site-run-file nil
			warning-suppress-log-types '((comp) (vytecomp))
			use-dialog-box nil
			use-file-dialog nil
			use-short-answers t
			inhibit-splash-screen t
			inhibit-startup-screen t
			inhibit-x-resources t
			inhibit-default-init t
			inhibit-startup-message t
			inhibit-startup-echo-area-message user-login-name
			inhibit-startup-buffer-menu t)

; Disable all graphical elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(tooltip-mode -1)
(tool-bar-mode -1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.5)

  ;; Save `file-name-handler-alist` and `vc-handled-backends`, so they can
  ;; be restored after garbage collection is completed.
  (defvar tnh--file-name-handler-alist file-name-handler-alist)
t  (defvar tnh--vc-handled-backends vc-handled-backends)

	(setq gc-cons-threshold (if (display-graphic-p) 40000000 10000000))

	(eval-and-compile
		(defun tnh-emacs/default-gc ()
			(setq-default gc-cons-threshold 800000))
		(defun tnh-emacs/maybe-gc ()
			(unless (frame-focus-state)
				(garbage-collect))))

	(add-hook 'after-init-hook #'tnh-emacs/default-gc)
	(add-function :after after-focus-change-function 'tnh-emacs/maybe-gc)

	(add-hook 'emacs-startup-hook
			(lambda ()
				(setq gc-cons-threshold (* 8 1000 1000)
				gc-cons-percentage 0.1
				file-name-handler-alist tnh--file-name-handler-alist
				vc-handled-backends tnh--vc-handled-backends)))

(setq tramp-verbose 10)
(require 'tramp)
(setq tramp-default-method "ssh")
(tramp-cleanup-all-connections)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(setq-default tnh-emacs/emacs-config-directory (file-name-directory load-file-name)
							user-emacs-directory (expand-file-name "~/.cache/emacs/")
							package-user-dir (expand-file-name "packages/" user-emacs-directory)
							url-history-file (expand-file-name "url/history" user-emacs-directory)
							custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(when (featurep 'native-compile)
	(let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
		(setq-default native-compile-target-directory path
									native-comp-eln-load-path (list path)
									native-comp-async-report-warnings-errors nil
									native-comp-deferred-compilation 5
									native-comp-speed 2
									package-native-compile t)
		(when (fboundp 'startup-redirect-eln-cache)
			(startup-redirect-eln-cache path))))

(add-to-list 'load-path (expand-file-name "modules/" tnh-emacs/emacs-config-directory))

(set-default-coding-systems 'utf-8)

(add-to-list 'display-buffer-alist
						 '("\\*Help\\*"
							 (display-buffer-reuse-window display-buffer-pop-up-window)
							 (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
						 '("\\*Completions\\*"
							 (display-buffer-reuse-window display-buffer-pop-up-window)
							 (inhibit-same-window . t)
							 (window-height . 10)))

(add-to-list 'display-buffer-alist
						 '("\\*Dictionay\\*"
							 (display-buffer-in-side-window)
							 (side . left)
							 (window-width . 70)))

(setq initial-major-mode 'fundamental-mode)
