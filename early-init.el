;;; early-init.el --- Early init configuration for Emacs -*- lexical-binding: t; -*-

;; Garbage collection
;; Defer garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (defun tnh/reset-gc-cons-threshold ()
            "Reset garbage collection settings after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; Collect garbage when Emacs is idle.
(add-hook 'focus-out-hook #'garbage-collect)

;; Default locations
(setq-default
 tnh/emacs-config-directory (file-name-directory load-file-name)
 user-emacs-directory (expand-file-name "~/.cache/emacs/")
 package-user-dir (expand-file-name "packages/" user-emacs-directory)
 url-history-file (expand-file-name "url/history" user-emacs-directory)
 custom-file (expand-file-name "custom.el" tnh/emacs-config-directory))
(load custom-file :noerror)

;; Native compilation
(when (featurep 'native-compile)
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq-default
     native-compile-target-directory path
     native-comp-eln-load-path (list path)
     native-comp-async-report-warnings-errors nil
     native-comp-deferred-compilation 5
     native-comp-speed 2
     package-native-compile t)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache path))))

;; Custom load-path
(add-to-list
 'load-path
 (expand-file-name "modules/" tnh/emacs-config-directory))

;; Set default coding systems
(set-default-coding-systems 'utf-8)

;; Configure window configuration for special windows.
(add-to-list
 'display-buffer-alist
 '("*Help*" (display-buffer-reuse-window
    display-buffer-pop-up-window)
   (inhibit-same-window . t)))

(add-to-list
 'display-buffer-alist
 '("*Completions*" (display-buffer-reuse-window
    display-buffer-pop-up-window)
   (inhibit-same-window . t) (window-height . 10)))

(add-to-list
 'display-buffer-alist
 '("*Dictionary*"
   (display-buffer-in-side-window)
   (side . left)
   (window-width . 70)))

;; Set name for default frame.
(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

;; Configure minimal frame.
(setq
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 frame-title-format '("%b")
 auto-window-vscroll nil
 byte-compile-warnings '(not obsolete)
 ring-bell-function 'ignore
 load-prefer-newer noninteractive
 package-enable-at-startup t
 site-run-file nil
 warning-suppress-log-types '((comp) (bytecomp))
 use-dialog-box nil
 use-file-dialog nil
 use-short-answers t
 inhibit-splash-screen t
 inhibit-x-resources t
 inhibit-default-init t
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-buffer-menu t)

;; Remove macos bar but keep round corners
(add-to-list 'default-frame-alist '(undecorated-round .t))

;; Disable all graphical elements
(blink-cursor-mode 1)
(global-font-lock-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(tooltip-mode -1)
(tool-bar-mode -1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Make the initial buffer load-faster.
(setq initial-major-mode 'fundamental-mode)

;; Centralize backup and auto-save files
(let ((backup-dir (expand-file-name "backups/" tnh/emacs-config-directory))
      (auto-save-dir (expand-file-name "auto-saves/" tnh/emacs-config-directory)))
  (make-directory backup-dir t)
  (make-directory auto-save-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
	backup-by-copying t
	delete-old-versions t
	auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

;;; early-init.el ends here
