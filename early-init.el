(setq frame-resize-pixelwise t
frame-inhibit-implied-resize t
frame-title-format '("%b")
ring-bell-function 'ignore
use-dialog-box nil
use-file-dialog nil
use-short-answers t
inhibit-splash-screen t
inhibit-startup-screen t
inhibit-x-resources t
inhibit-startup-echo-area-message user-login-name
inhibit-startup-buffer-menu t)

; Disable all graphical elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold most-positive-fixnum
gc-cons-percentage 0.5)

;; Save `file-name-handler-alist` and `vc-handled-backends`, so they can
;; be restored after garbage collection is completed.
(defvar tnh--file-name-handler-alist file-name-handler-alist)
(defvar tnh--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
vc-handled-backends nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 8 1000 1000)
		  gc-cons-percentage 0.1
		  file-name-handler-alist tnh--file-name-handler-alist
		  vc-handled-backends tnh--vc-handled-backends)))

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(setq package-enable-at-startup t)
