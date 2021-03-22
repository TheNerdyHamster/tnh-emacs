(defvar tnh/default-font-size 100)

(defvar tnh/frame-transparency '(100 . 100))

;; Secrets
(defvar tnh/wk-token  nil "Wakatime API token")
(defvar tnh/vpn-host  nil "Openforti VPN Host name")
(defvar tnh/vpn-user  nil "Openforti VPN user")
(defvar tnh/vpn-cert  nil "Openforti VPN cert")

(let ((secrets (expand-file-name ".secrets.el" user-emacs-directory)))
  (load secrets t))

(setq gc-cons-threshold (* 50 1000 1000))

(defun tnh/display-startup-time ()
  (message " TNH-Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'tnh/display-startup-time)

(server-start)

(setq package-enable-at-startup nil)
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight.el to install new packages
;(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(let ((straight-current-profile 'pinned))
  (straight-use-package 'org-plus-contrib)
  (straight-use-package 'org)
  ;; Pin org-mode version.
  (add-to-list 'straight-x-pinned-packages
	       '("org" . "924308a150ab82014b69c46c04d1ab71e874a2e6")))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering
  :straight t)

(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)
(setq warning-minimum-level :error)

(scroll-bar-mode -1)			; Disable visible scrollbar
(tool-bar-mode -1)			; Disable the toolbar
(tooltip-mode -1)			; Disable tooltips
(set-fringe-mode 5)			; Give text some breathing room

(menu-bar-mode -1)			; Disable menu bar

(setq visible-bell t)			; Enable visible bell

(column-number-mode)
(global-display-line-numbers-mode t)

(setq x-stretch-cursor t)
(global-hl-line-mode +1)


(set-frame-parameter (selected-frame) 'alpha tnh/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,tnh/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                 vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Default buffer
;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height tnh/default-font-size)

(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height tnh/default-font-size)

(use-package ligature
  :load-path "~/.emacs.d/github/ligature"
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
  ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
  "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
  "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
  "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
  "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
  "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
  "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
  "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
  "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :straight t
  :after evil
  :config
  (general-auto-unbind-keys)
  (general-override-mode +1)

  (general-create-definer tnh/leader-key
    :states '(normal insert visual emacs treemacs)
    :keymap 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (tnh/leader-key
   "t" '(:ignore t :wk "toggle")
   "tt" '(counsel-load-theme :wk "change theme")
   "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :custom
  (evil-undo-system 'undo-redo)

  :bind 
  (:map evil-motion-state-map
        ("q" . nil))
  :config
  (evil-mode 1)
  ;;(evil-define-key 'normal 'insert 'visual (kbd "C-c") 'hydra-master/body)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-delete-char)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package key-chord
  :straight t
  :config
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map  "kj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package doom-themes
  :straight t)

(use-package spaceduck
  :straight (:host github :repo "tathran/spaceduck-emacs"
             :branch "main"))

(defun tnh/apply-theme ()
  "Apply selected theme, and make the frame transparent."
  (interactive)
  (load-theme 'spaceduck t))

(tnh/apply-theme)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom 
  (doom-modeline-height 5)
  (doom-themes-visual-bell-config)
  :config
  (display-battery-mode t)
  (display-time-mode t))

(use-package time
  :config
  (setq display-time-format "%a %d/%m %H:%M")
        display-time-day-and-date t
        display-time-default-load-average nil)

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  ;(setq which-key-popup-type 'frame)
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :straight t
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
  :straight t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :straight t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

(use-package ivy-prescient
  :straight t
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :straight t)

(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "quit" :exit t))

(tnh/leader-key
  "ts" '(hydra-text-scale/body :wk "scale text"))

(use-package page-break-lines
  :straight t
  :init (page-break-lines-mode t))

(use-package dashboard
  :straight t
  :init
  (progn
    (setq dashboard-items '((recents . 10)
                            (projects . 10)))
    (setq dashboard-show-shortcuts nil
          dashboard-banner-logo-title "Welcome to The Nerdy Hamster Emacs"
          dashboard-set-file-icons t
          dashboard-set-heading-icons t
          dashboard-startup-banner 'logo
          dashboard-set-navigator t
          dashboard-navigator-buttons
    `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Github"
              "Browse homepage"
              (lambda (&rest _) (browse-url "https://github.com/TheNerdyHamster/The-Nerdy-Hamster-Emacs")))
            (,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
              "Linkedin"
              "My Linkedin"
              (lambda (&rest _) (browse-url "https://www.linkedin.com/in/leo-ronnebro/" error)))
          ))))
  :config
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package eyebrowse
  :straight t
  :bind
  ("M-0" . eyebrowse-last-window-config)
  ("M-1" . eyebrowse-switch-to-window-config-1)
  ("M-2" . eyebrowse-switch-to-window-config-2)
  ("M-3" . eyebrowse-switch-to-window-config-3)
  ("M-4" . eyebrowse-switch-to-window-config-4)
  ("M-5" . eyebrowse-switch-to-window-config-5)
  ("M-6" . eyebrowse-switch-to-window-config-6)
  ("M-7" . eyebrowse-switch-to-window-config-7)
  ("M-8" . eyebrowse-switch-to-window-config-8)
  ("M-9" . eyebrowse-switch-to-window-config-9)
  :hook
  (after-init . eyebrowse-mode)
  :custom
  (eyebrowse-new-workspace t))

(tnh/leader-key
   "o" '(:ignore t :wk "org")
   "oa" '(org-agenda :wk "agenda")
   "oc" '(org-capture :wk "capture")
   "ol" '(org-store-link :wk "link"))

(defun tnh/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Nerd Font" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun tnh/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun tnh/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'tnh/org-babel-tangle-config)))

(use-package org
  :commands (org-capture org-agenda)
  :hook ((org-mode . tnh/org-mode-setup)
         (org-mode . tnh/org-font-setup)
         (org-mode . org-bullets-mode))
  :custom
  (org-ellipsis " ▾")
  (org-agenda-files
   '("~/Documents/Org/"))
  :config

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org-bullets
  :straight t
  :after org
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun tnh/org-mode-visual-fill ()
  (setq visual-fill-column-width 140
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :after org
  :hook (org-mode . tnh/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(defun tnh/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . tnh/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (setq lsp-completion-provider :capf)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :straight t
  :after lsp)

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs
               `(csharp-mode . ("/home/leo/.cache/emacs/var/omnisharp/cache/server/v1.37.5/run"))))

(defun tnh/go-mode-setup ()
  (setq tab-width 2
        evil-shift-width 2))

(defun tnh/go-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'eglot-code-action-organize-imports))

(use-package go-mode
  :straight t
  :hook ((go-mode . eglot-ensure)
         (go-mode . tnh/go-save-hooks)
         (go-mode . tnh/go-mode-setup))
  :mode "\\.go\\'")

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2))

(defun tnh/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :straight t
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))


(use-package prettier-js
  :straight t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(defun tnh/rust-mode-setup ()
  (setq indent-tabs-mode nil
        rust-format-on-save t))

(use-package rust-mode
  :straight t
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . tnh/rust-mode-setup))
  :mode "\\.rs\\'")

(use-package cargo
  :straight t
  :defer t)

(use-package csharp-mode
  :straight t
  :hook
  (csharp-mode . rainbow-delimiters-mode)
  (csharp-mode . company-mode)
  (csharp-mode . lsp-deferred)
  ;(csharp-mode . flycheck-mode)
  (csharp-mode . omnisharp-mode))

(use-package omnisharp
  :straight t
  :after csharp-mode company
  :commands omnisharp-install-server
  :config
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 2
        tab-width 2
        evil-shift-width 2)
  (tnh/leader-key
    "o" '(:ignore o :which-key "omnisharp")
    "o r" '(omnisharp-run-code-action-refactoring :which-key "omnisharp refactor")
    "o b" '(recompile :which-key "omnisharp build/recompile")
    )
  (add-to-list 'company-backends 'company-omnisharp))

(use-package python
  :hook (python-mode . eglot-ensure)
  :custom
  (python-shell-interpreter "python3"))

(use-package pyvenv
  :straight t
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'")

(use-package company
  :straight t
  :after eglot
  :hook (eglot-managed-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map eglot-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (setq company-backends '(company-capf))
  (setq company-auto-commit t))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :straight t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :straight t
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter-fringe
  :straight t
  :preface
  (defun tnh/git-gutter-enable ()
    (when-let* ((buffer (buffer-file-name))
                (backend (vc-backend buffer)))
      (require 'git-gutter)
      (require 'git-gutter-fringe)
      (git-gutter-mode 1)))
  :hook
  (after-change-major-mode . tnh/git-gutter-enable)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [192] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [192] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:modified [192] nil nil '(center t)))

(use-package vterm
  :straight t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :straight t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(defun tnh/fish-save-hook ()
  (add-hook 'before-save-hook 'fish_indent-before-save))

(use-package fish-mode
  :straight t
  :hook (fish-mode . tnh/fish-save-hook)
  :mode "\\.fish\\'")

(use-package restart-emacs
  :straight t)

(use-package elcord
  :straight t
  :config
  (elcord-mode 1))

(use-package wakatime-mode 
  :straight t
  :config
  (setq wakatime-api-key tnh/wk-token)
  (global-wakatime-mode))

(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

;; (defun tnh/connect-vpn ()
;;   "Connect to vpn via openfortivpn."
;;   (interactive)
;;   (let (pwd (read-passwd "Enter pwd: ")))
;;     (message "%s" pwd))

(defun tnh/connect-vpn ()
  "Connect to vpn via openfortivpn."
  (interactive)
  (let ((pwd (read-passwd "Enter password: ")))
    (start-process-shell-command "tnh/openfortivpn" "vpn-log" (format "sudo -S openfortivpn %s -u %s --trusted-cert %s -p %s" tnh/vpn-host tnh/vpn-user tnh/vpn-cert pwd))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun tnh/sudo-find-file ()
  (interactive)
  (counsel-find-file "/sudo::/"))

(defun tnh/insert-pdf-links ()
  "Generate a todo list of files within a directory"
  (interactive)
  (let ((path (read-directory-name "Directory:")))
    (let ((files (directory-files path nil "\\.pdf$")))
      (dolist (file files)
        (message (format "path is %s" path))
        (newline)
        (insert (format "*** [ ] [[file:%s%s][%s]]" path file file))))))

(setq gc-cons-threshold (* 2 1000 1000))
