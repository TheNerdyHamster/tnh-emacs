;; Garbage collection
(setq gc-cons-threshold
      (if (display-graphic-p)
	  40000000
	10000000))

(eval-and-compile
  (defun tnh-emacs/default-gc ()
    (setq-default gc-cons-threshold 800000))
  (defun tnh-emacs/maybe-gc ()
    (unless (frame-focus-state)
      (garbage-collect))))

(add-hook 'after-init-hook #'tnh-emacs/default-gc)
(add-function :after after-focus-change-function 'tnh-emacs/maybe-gc)


;; Default locations
(setq-default
 tnh-emacs/emacs-config-directory (file-name-directory load-file-name)
 user-emacs-directory (expand-file-name "~/.cache/emacs/")
 package-user-dir (expand-file-name "packages/" user-emacs-directory)
 url-history-file (expand-file-name "url/history" user-emacs-directory)
 custom-file (expand-file-name "custom.el" user-emacs-directory))
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
 (expand-file-name "modules/" tnh-emacs/emacs-config-directory))

;; Set default coding systems
(set-default-coding-systems 'utf-8)

;; Configure window configuration for special windows.
(add-to-list
 'display-buffer-alist
 '("\\*Help\\*" (display-buffer-reuse-window
    display-buffer-pop-up-window)
   (inhibit-same-window . t)))

(add-to-list
 'display-buffer-alist
 '("\\*Completions\\*" (display-buffer-reuse-window
    display-buffer-pop-up-window)
   (inhibit-same-window . t) (window-height . 10)))

(add-to-list
 'display-buffer-alist
 '("\\*Dictionay\\*"
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
