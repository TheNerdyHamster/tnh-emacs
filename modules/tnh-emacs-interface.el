(use-package vertico
	:ensure t
	:custom
	(vertico-cycle t)

	:custom-face
	(vertico-current ((t (:background "#3a3f5a"))))

	:config
	(require 'vertico-directory)
	(vertico-mode))

(use-package corfu
	:ensure t
	:bind (:map corfu-map
							("TAB" . corfu-insert)
							("C-f" . corfu-insert))
	:custom
	(corfu-auto t)
	(corfu-cycle t)
	(corfu-quit-at-boundary t)
	(corfu-quit-no-match t)
	:config
	(global-corfu-mode 1)
	;; (defun corfu-enable-in-minibuffer ()
	;; 	"Enable Corfu in the minibuffer."
	;; 	(when (local-variable-p 'completion-at-point-functions)
	;; 		;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
	;; 		(setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
	;; 								corfu-popupinfo-delay nil)
	;; 		(corfu-mode 1)))
	;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
	)

(use-package helpful
	:ensure t
	:custom
	(counsel-describe-function-function #'helpful-function)
	(counsel-describe-variable-function #'helpful-variable)
	:bind (([remap describe-function] . helpful-function)
				 ([remap describe-command] . helpful-command)
				 ([remap desribe-variable] . helpful-variable)
				 ([remap describe-key] . helpful-key)))

(use-package kind-icon
	:ensure t
	:after corfu
	:custom (kind-icon-default-face 'corfu-default)
	:config
	(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
	:ensure t
	:config
	(orderless-define-completion-style orderless+initialism
																		 (orderless-matching-styles '(orderless-initialism
																																	orderless-literal
																																	orderless-regexp)))
	(setq completion-styles '(orderless)
				completion-category-defaults nil
				orderless-matching-styles '(orderless-literal orderless-regexp)
				completion-category-overrides
				'((file (styles partial-completion)))))

(use-package wgrep
	:after consult
	:hook (grep-mode . wgrep-setup))

(use-package consult
	:ensure t
	:bind (("C-x C-b" . consult-buffer)
				 :map minibuffer-local-map
				 ("C-r" . consult-history))
	:custom
	(completion-in-region-function #'consult-completion-in-region))

(use-package consult-dir
	:ensure t
	:bind (("C-x C-d" . consult-dir)
				 :map vertico-map
				 ("C-x C-d" . consult-dir)
				 ("C-x C-j" . consult-dir-jump-file))
	:custom
	(consult-dir-project-list-function nil))

(use-package embark
	:ensure t
	:after vertico
	:bind (("C-." . embark-act)
				 ("M-." . embark-dwim)
				 :map minibuffer-local-map
				 ("C-d" . embark-act)
				 :map embark-region-map
				 ("D" . denote-region))
	:config
	(delete #'embark-mixed-indicator embark-indicators)
	(add-to-list 'embark-indicators 'embark-minimal-indicator)

	(setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
	:ensure t
	:after embark)

(use-package savehist
	:ensure t
	:init
	(savehist-mode))

(use-package marginalia
	:ensure t
	:after vertico
	:config
	(marginalia-mode))

(use-package nov
	:ensure t
	:config
	(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Emacs zone
;;	(setq zone-timer (run-with-idle-timer 120 t 'zone))

(defun zone-choose (pgm)
	"Choose a PGN to run for `zone'."
	(interactive
	 (list
		(completing-read
		 "Program: "
		 (mapcar 'symbol-name zone-programs))))
	(let ((zone-programs (list (intern pgm))))
		(zone pgm)))


(provide 'tnh-emacs-interface)
