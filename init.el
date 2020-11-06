(defvar he/default-font-size 100)
(defvar he/default-variable-font-size 100)

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

(setq he/exwm-enabled (and (eq window-system 'x)
                           (seq-contains command-line-args "--use-exwm")))

(when he/exwm-enabled
(use-package exwm
  :init
  (setq mouse-autoselect-window nil
        focus-follow-mouse t
        exwm-workspace-warp-cursor t
        exwm-workspace-number 5)
  :config
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (string-equal exwm-class-name "Vimb")
                (exwm-workspace-rename-buffer (format "vimb: %s" exwm-title)))))

(exwm-enable))

(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-h
        ?\M-x
        ?\M-&     ;; Async shell command
        ?\M-:     ;; Eval
        ?\C-\M-j  ;; Buffer list
        ?\C-\M-k  ;; Browser list
        ?\C-\     ;; Ctrl+Space
        ?\C-\;)))

(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

(setq inhibit-startup-message t)
 (setq initial-buffer-choice "*dashboard*")

 (scroll-bar-mode -1)        ; Disable visible scrollbar
 (tool-bar-mode -1)          ; Disable the toolbar
 (tooltip-mode -1)           ; Disable tooltips
 (set-fringe-mode 10)        ; Give some breathing room

 (menu-bar-mode -1)            ; Disable the menu bar

 ;; Set up the visible bell
;; (setq visible-bell t)

 (global-hl-line-mode +1)    ; Enable line highlight
 (column-number-mode)
 (global-display-line-numbers-mode t)

 ;; Disable line numbers for some modes
 (dolist (mode '(org-mode-hook
                 vterm-mode-hook
                 shell-mode-hook
	               treemacs-mode-hook
                 eshell-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)
 ;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(set-face-attribute 'default nil :font "Fira Code NF" :height he/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code NF" :height he/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code NF" :height he/default-variable-font-size :weight 'regular)

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
(global-unset-key (kbd "C-SPC"))

(use-package general
  :config
  
  (general-override-mode +1)

  (general-create-definer he/leader-keys
    :states '(normal insert visual emacs treemacs)
    :keymap 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    :non-normal-prefix "C-SPC"))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(he/leader-keys
 ;; General
 "/"  '(evilnc-comment-or-uncomment-lines :which-key "Comment")
 "s"  '(swiper :which-key "Search file")
 "."  '(counsel-find-file :which-key "Find file")
 "<"  '(counsel-switch-buffer :which-key "Switch buffer")
 "SPC"'(counsel-projectile-find-file :which-key "Find project file")
 ;; Buffer
 "b"  '(:ignore t :which-key "buffer")
 "bs" '(save-buffer :which-key "Save buffer")
 "bn" '(evil-next-buffer :which-key "Next buffer")
 "bp" '(evil-prev-buffer :which-key "Prev buffer")
 "bk" '(kill-buffer :which-key "Kill buffer")
 ;; Magit/Git
 "g"  '(:ignore t :which-key "Git")
 "gs" '(magit :which-key "Git status")
 ;; Open
 "o"  '(:ignore t :which-key "open")
 "oa"  '(counsel-linux-app :which-key "Application")
 "op" '(treemacs :which-key "treemacs")
 "od" '(docker :which-key "docker")
 ;: Org
 "O"  '(:ignore t :which-key "org")
 "Oa" '(org-agenda :which-key "Agenda")
 ;; Toggle
 "t"  '(:ignore t :which-key "toggle")
 ;; Window
 "w"  '(:ignore t :which-key "window")
 
 "tab" '(:ignore t :which-key "Workspaces")
 "<tab>s" '(exwm-workspace-switch :which-key "Switch workspace")
 "<tab>1" '((lambda () (interactive) (exwm-workspace-switch 0)) :which-key "Workspace 1")
 "<tab>2" '((lambda () (interactive) (exwm-workspace-switch 1)) :which-key "Workspace 2")
 "<tab>3" '((lambda () (interactive) (exwm-workspace-switch 2)) :which-key "Workspace 3")
 "<tab>4" '((lambda () (interactive) (exwm-workspace-switch 3)) :which-key "Workspace 1")
 "<tab>5" '((lambda () (interactive) (exwm-workspace-switch 4)) :which-key "Workspace 5")
 "<tab>m" '(exwm-workspace-move-window :which-key "Move window to workspace")
 )

(load-theme 'modus-vivendi)
;; (use-package doom-themes
;;   :init (load-theme 'doom-dracula t))

(use-package command-log-mode)

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom 
  (doom-modeline-height 15)
  (doom-themes-visual-bell-config))

 (display-battery-mode t)
 (display-time-mode t)

;; (use-package sublimity
;;   :init
;;   (require 'sublimity-scroll)
;;   :config
;;   (sublimity-mode 1))

(use-package treemacs)

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-projectile
  :after treemacs)
  
(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package docker
  :ensure t)

(use-package yasnippet-snippets)

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package evil-nerd-commenter)

(use-package expand-region)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :config
  (rainbow-mode 1))

(use-package centaur-tabs
  :demand
  :init
  (setq centaur-tabs-style "bar"
    centaur-tabs-set-icons t
    centaur-tabs-set-close-button nil)
  :config
  (centaur-tabs-mode t))

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 5)
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

(use-package which-key
 :init (which-key-mode)
 :diminish which-key-mode
 :config
 (setq which-key-idle-delay 0.4))

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
  :bind (("C-M-j" . 'counsel-switch-buffer)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1)) 

(use-package smex 
  :defer 1
  :after counsel)

(use-package ivy-posframe
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
   (ivy-posframe-mode 1))

(use-package helpful
 :custom
 (counsel-describe-function-function #'helpful-callable)
 (counsel-describe-variable-function #'helpful-variable)
 :bind
 ([remap describe-function] . counsel-describe-function)
 ([remap describe-command] . helpful-command)
 ([remap describe-variable] . counsel-describe-variable)
 ([remap describe-key] . helpful-key))

;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)

(defun he/org-font-setup ()
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
   (set-face-attribute (car face) nil :font "Fira Code NF" :weight 'regular :height (cdr face)))

 ;; Ensure that anything that should be fixed-pitch in Org files appears that way
 (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
 (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun he/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . he/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Documents/Org/Tasks.org"
          "~/Documents/Org/Habits.org"
          "~/Documents/Org/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-note")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Documents/Org/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Documents/Org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Documents/Org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Documents/Org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Documents/Org/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (he/org-font-setup))

(use-package org-bullets
 :after org
 :hook (org-mode . org-bullets-mode)
 :custom
 (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun he/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . he/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun he/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'he/org-babel-tangle-config)))

(use-package toc-org
  :hook (toc-org . org-mode))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
          ("<tab>" . company-complete-selection))
         (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (setq company-backends '(company-capf))

(use-package company-prescient
  :init (company-prescient-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package go-mode
  :mode "\\.go\\'")
 
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package csharp-mode
  :hook
  (csharp-mode . rainbow-delimiters-mode)
  (csharp-mode . company-mode)
  (csharp-mode . flycheck-mode))

(use-package omnisharp
  :after csharp-mode
  :commands omnisharp-install-server
  :config
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 2
        tab-width 2
        evil-shift-width 2)
  (he/leader-keys
    "o" '(:ignore o :which-key "omnisharp")
    "o r" '(omnisharp-run-code-action-refactoring :which-key "omnisharp refactor")
    "o b" '(recompile :which-key "omnisharp build/recompile")
    ))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile*\\'")

(defun he/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . he/lsp-mode-setup)
        (typescript-mode . lsp-deferred)
        (go-mode . lsp-deferred)
        (csharp-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "SPC-c l")  ;; Or 'C-l', 's-l'
  :config
  (setq lsp-completion-provider :capf)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))
  ;; :custom
  ;; (setq lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package flycheck
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (he/leader-keys
    "e" '(:ignore t :which-key "errors")
    "e l" '(flycheck-list-errors :which-key "list errors")
    )
  )

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t))

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "<tab>") 'org-cycle)
  (define-key eww-mode-map (kbd "S-<tab>") 'org-shifttab)
  (define-key eww-mode-map (kbd "C-t") 'shrface-toggle-bullets)
  (define-key eww-mode-map (kbd "C-j") 'shrface-next-headline)
  (define-key eww-mode-map (kbd "C-k") 'shrface-previous-headline)
  (define-key eww-mode-map (kbd "C-i") 'shrface-links-counsel) ; or 'shrface-links-helm
  (define-key eww-mode-map (kbd "C-o") 'shrface-headline-counsel)) ; or 'shrface-headline-helm

(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))
                  
(defun shrface-shr-tag-pre-highlight (pre)
  "Highlighting code in PRE."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 (setq-local fill-column 120)
                 (indent-rigidly (point-min) (point-max) 2)
                 (if (eq "" (dom-texts pre))
                     nil
                   (progn
                     (setq-local fill-column shrface-paragraph-fill-column)
                     (indent-rigidly (point-min) (point-max) shrface-paragraph-indentation)))
                 (buffer-string)))
         (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                   (let ((sym (language-detection-string code)))
                     (and sym (symbol-name sym)))))
         (mode (and lang
                    (shr-tag-pre-highlight--get-lang-mode lang))))
    (shr-ensure-newline)
    (insert (make-string shrface-paragraph-indentation ?\ )) ; make indent string
    ;; (insert (propertize (concat "#+BEGIN_SRC " lang) 'face 'org-block-begin-line))
    (shr-ensure-newline)
    (setq start (point))
    (insert
     (or (and (fboundp mode)
              (with-demoted-errors "Error while fontifying: %S"
                (shrface-tag-pre-highlight-fontify code mode)
                ))
         code))
    (shr-ensure-newline)
    (setq end (point))
    (insert (make-string shrface-paragraph-indentation ?\ )) ; make indent string
    ;; (insert (propertize "#+END_SRC" 'face 'org-block-end-line ) )
    (let* ((beg start)
           (xx (make-overlay beg end)))
      (overlay-put xx 'face '(:background "#292b2e" :height 90)))
    (shr-ensure-newline)
    (insert "\n")))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package elcord
  :config
  (elcord-mode 1))
