;;; tnh-packages.el --- Setup and configure use-package -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(provide 'tnh-packages)

;;; tnh-packages.el ends here
