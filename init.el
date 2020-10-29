;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq
   backup-by-copying t      ; don't clobber symlinks
;;   backup-directory-alist
;;   '((".*" . temporary-file-directory))    ; don't litter my fs tree
;;   auto-save-file-name-transforms
;;    '((".*" . temporary-file-directory t))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(setq inhibit-startup-message t)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(tooltip-mode -1)

(menu-bar-mode -1)

(set-fringe-mode 10)

(setq visible-bell t)

(global-hl-line-mode +1)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;(set-frame-parameter (selected-frame) 'alpha '(90 90))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(set-face-attribute 'default nil :font "Fira Code NF" :height 100)

(use-package all-the-icons
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package page-break-lines)

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 5)
			    (projects . 10)))
    (setq dashboard-show-shortcuts nil
	  dashboard-banner-logo-title "Welcome to Hamster-Emacs"
	  dashboard-set-file-icons t
	  dashboard-set-heading-icons t
;	  dashboard-startup-banner 0 
	  dashboard-set-navigator t
	  dashboard-navigator-buttons
	  `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Github"
	      "Browse homepage"
              (lambda (&rest _) (browse-url "https://github.com/Lilahamstern/Hamster-emacs")))
            (,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
              "Linkedin"
              "My Linkedin"
              (lambda (&rest _) (browse-url "https://www.linkedin.com/in/leo-ronnebro/" error)))
	  )))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
	dashboard-center-content t)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
   :config
   (general-create-definer lh/leader-keys-def
     :keymaps '(normal insert visual emacs)
     :prefix "SPC"
     :global-prefix "C-SPC")

   (lh/leader-keys-def
    "u" '(:ignore u :which-key "UI")
    "ut" '(counsel-load-theme :which-key "Choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t
  	evil-want-keybinding nil
  	evil-want-C-u-scroll t
  	evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(lh/leader-keys-def
 "us" '(hydra-text-scale/body :which-key "scale text"))

(defun lh/org-mode-setup ()
    ;(org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq evil-auto-indent nil))

  (defun lh/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
			    '(("^ *\\([-]\\) "
			       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
)
    ;; ;; Set faces for heading levels
    ;; (dolist (face '((org-level-1 . 1.2)
    ;;                 (org-level-2 . 1.1)
    ;;                 (org-level-3 . 1.05)
    ;;                 (org-level-4 . 1.0)
    ;;                 (org-level-5 . 1.1)
    ;;                 (org-level-6 . 1.1)
    ;;                 (org-level-7 . 1.1)
    ;;                 (org-level-8 . 1.1)))
;;      (set-face-attribute (car face) nil :font "Fira Code NF" :weight 'regular :height (cdr face))))

  (use-package org
    :hook (org-mode . lh/org-mode-setup)
    :config
    (setq org-ellipsis " ▾"
	  org-hide-emphasis-markers t)
    (lh/org-font-setup))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun lh/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                                   (expand-file-name "~/Emacs/hamster-emacs/init.org"))
	(let ((org-confirm-babel-evaluate nil))
	  (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'lh/org-babel-tangle-config)))

(defun lh/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . lh/org-mode-visual-fill))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(use-package forge
  :after magit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
