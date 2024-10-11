(eval-when-compile (require 'use-package))
(use-package all-the-icons
	:ensure t
	:if (display-graphic-p)
	:commands all-the-icons-install-fonts
	:custom
	(all-the-icons-dired-monochrome nil)
	:custom-face
	(all-the-icons-dired-dir-face ((t (:foreground "orange"))))
	:init
	(unless (find-font (font-spec :name "all-the-icons"))
		(all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
	:ensure t
	:after all-the-icons)

(provide 'tnh-all-the-icons)

