(use-package emacs-lisp-mode :mode ("\\.el\\'" "\\.el.tmp\\'"))

(use-package savehist
  :demand t
  :custom
  (history-delete-duplicates t)
  (history-length 3000)
  :config (savehist-mode))

(use-package files
  :demand t
  :functions emacs-tmp-dir
  :init
  (defconst emacs-tmp-dir
    (expand-file-name (format "emacs%d/" (user-uid))
		      temporary-file-directory))
  (setq
   auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
   backup-directory-alist `((".*" . ,emacs-tmp-dir))
   auto-save-timeout 3
   auto-save-list-file-name nil
   auto-save-interval 0
   auto-save-default t
   auto-save-list-file-prefix emacs-tmp-dir)
  :custom
  (backup-inhibited t)
  (confirm-kill-processes nil)
  (create-lockfiles nil)
  (delete-old-versions t)
  (make-backup-files nil)
  (version-control t)
  (vc-make-backup-files t)
  (kept-new-versions 5)
  (kept-old-versions 0))

(use-package autorevert
  :defer 3
  :custom (global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode t))

(provide 'tnh-emacs-base)
