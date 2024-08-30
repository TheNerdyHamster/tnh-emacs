;; -- No littering
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
			url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering
	:ensure t
	:config
	(setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))  
(setq auto-save-default nil)

;; -- Native compilation settings
(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; -- Basic bindings

;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)
(repeat-mode 1)

(column-number-mode)

(dolist (mode '(prog-mode-hook
				conf-mode-hook
				text-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)

;; Font
(set-face-attribute 'default nil
										:font "FiraCode Nerd Font"
										:weight 'normal)

(set-face-attribute 'fixed-pitch nil
										:font "FiraCode Nerd Font"
										:weight 'light)

;; Tabbar
(use-package tab-bar
	:ensure nil
	:bind (("s-[" . tab-bar-switch-to-prev-tab)
				 ("s-]" . tab-bar-switch-to-next-tab)
				 ("s-{" . (lambda ()
										(interactive)
										(tab-move -1)))
				 ("s-}". (lambda ()
									 (interactive)
									 (tab-move 1))))
	:custom
	(tab-bar-show t)
	(tab-bar-close-button-show nil)
	(tab-bar-auto-width nil)
	(tab-bar-history-mode 1)
	(tab-bar-mode 1))
;; Editing

(setq-default tab-width 2
		indent-tab-mode nil)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(defhydra+ tnh-emacs/emacs-hydra ()
	("e" (lambda () (interactive) (find-file "~/.config/emacs")) "Open Emacs configuration" :column "Configuration")
	("r" restart-emacs "Restart emacs" :column "General"))

;; Emacs server daemon
;; (server-start)

(provide 'tnh-emacs-core)
