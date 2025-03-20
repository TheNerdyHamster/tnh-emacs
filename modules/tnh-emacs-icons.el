(eval-when-compile
  (require 'use-package))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :custom (all-the-icons-dired-monochrome nil)
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground "orage"))))
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'tnh-emacs-icons)
