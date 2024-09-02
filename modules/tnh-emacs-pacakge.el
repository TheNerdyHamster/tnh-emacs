(require 'package)

(setq package-vc-register-as-project nil)

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("gnu-elpa" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("gnu-elpa" . 3)
	("melpa" . 2)
	("nongnu" . 1)))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(provide 'tnh-emacs-package)
