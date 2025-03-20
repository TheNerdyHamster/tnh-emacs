;; Startup time hook
(defun tnh/display-startup-info ()
  (message "TNH-Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'tnh/display-startup-info)

;; Package configuration
(eval-when-compile
  (require 'package)
  (setq package-archives
	'(("melpa" . "http://melpa.org/packages/")
	  ("melpa-stable" . "https://stable.melpa.org/packages/")
	  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	  ("gnu-elpa" . "https://elpa.gnu.org/packages/")))
  (setq
   package-install-upgrade-built-in t
   package-archive-priorities
   '(("gnu-elpa" . 200)
     ("melpa" . 150)
     ("melpa-stable" . 100)
     ("nongnu" . 50)))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (put 'use-package 'lisp-indent-function 1)
  (use-package
      use-package-core
    :custom
    (use-package-minimum-reported-time 0.005)
    (use-package-enable-imenu-support t)))

;; Theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-org-config))
