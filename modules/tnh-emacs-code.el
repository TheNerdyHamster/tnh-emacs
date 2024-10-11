(use-package rainbow-delimiters
	:ensure t
	:hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
	:ensure t
	:hook (org-mode
				 emacs-lisp-mode))

(use-package smartparens
	:ensure t
	:hook (prog-mode . smartparens-mode)
	:config
	(sp-use-smartparens-bindings))

(use-package magit
	:ensure t
	:bind (("C-M-;" . magit-status-here)
				 ("C-c C-g" . magit-status-here)))

(use-package highlight-indent-guides
	:ensure t
	:config
	(add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package yaml-mode
	:ensure t
	:config
	(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
	(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package ansible
	:ensure t
	:bind (("C-c a d" . 'ansible-decrypt-buffer)
				 ("C-c a e" . 'ansible-encrypt-buffer)))

(use-package nix-mode
	:ensure t
	:mode "\\.nix\\'")

(use-package emacs-lisp-mode
	:mode ("\\.el\\'" "\\.el.tmpl\\'"))

(defun tnh-emacs/project-current-name ()
	(file-name-nondirectory
	 (directory-file-name
		(project-root (project-current)))))

(defun tnh-emacs/project-close-tab ()
	(interactive)
	(let* ((project-name (tnh-emacs/project-current-name))
				 (tab-index (tab-bar--tab-index-by-name project-name)))
		(project-kill-buffers t)
		(when tab-index
			(tab-bar-close-tab (1+ tab-index)))))

(defun tnh-emacs/project-magit-status ()
	(interactive)
	(magit-status (project-root (project-current))))

(use-package project
	:ensure nil
	:bind (("C-M-p" . project-find-file)
				 ;; ("C-x p a" . projectile-add-known-project) 
				 :map project-prefix-map
				 ("k" . tnh-emacs/project-close-tab)
				 ("F" . consult-ripgrep))
	:config
	(setq project-vc-extra-root-markers '(".project.el" ".projectile"))
	(add-to-list 'project-switch-commands '(tnh-emacs/project-magit-status "Magit" "m"))
	(add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" "F")))
	;; (setq project-list '("~/Documents/code/gitlab.netnod.se"))
	;; (setq project-switch-commands '((project-find-file "Find file")
	;; 																(project-find_regexp "Find regexp")
	;; 																(project-dired "Dired")
	;; 																(project-eshell "Eshell"))))

(provide 'tnh-emacs-code)
