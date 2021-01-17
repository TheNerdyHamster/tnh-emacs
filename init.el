(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0))                               ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-save-list-file-prefix nil                   ; Prevent tracking for auto-saves
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 cursor-type 'bar                                 ; Prefer a bar-shaped cursor by default
 delete-by-moving-to-trash t                      ; Delete files to trash
 fill-column 80                                   ; Set width for automatic line breaks
 gc-cons-threshold (* 8 1024 1024)                ; We're not living in the 70s anymore
 read-process-output-max (* 1024 1024)            ; Increase the read output for larger files.
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 2                                  ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; Use a single space after dots
 show-help-function nil                           ; Disable help text on most UI elements
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(fringe-mode '(3 . 0))                            ; Initialize thinner vertical fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'exile)                     ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(column-number-mode)                              ; Toggle column number mode for mode lines.
(global-display-line-numbers-mode t)              ; Toggle line numbers within buffer
(setq confirm-kill-processes nil)                 ; No need to confirm to kill a process....
(setq confirm-kill-emacs nil)                       ; Confirm to quit emacs

(if (eq window-system 'ns)
    (set-frame-parameter nil 'fullscreen 'maximized)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(add-hook 'focus-out-hook #'garbage-collect)

(defvar nhe/waka-time-token    nil               "The Waka time API token to use.")
(defvar nhe/font-family        "Fira Code NF"    "Default font family to use")
(defvar nhe/font-size-default  100               "The font size to use for default text.")
(defvar nhe/font-size-large    1.2               "The font size to use for larger text.")
(defvar nhe/font-size-small    .9                "The font size to use for smaller text.")

(let ((config.el (expand-file-name ".config.el" user-emacs-directory)))
  (load config.el t))

(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

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

(straight-use-package 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(server-start)

(use-package gruvbox-theme
    :straight t
    :config
    (load-theme 'gruvbox t)
    (set-face-attribute 'default nil :family "FiraCode NF" :height 110))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(dolist (mode '(org-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font nhe/font-family :height nhe/font-size-default)

(set-face-attribute 'fixed-pitch nil :font nhe/font-family :height nhe/font-size-default)

(set-face-attribute 'variable-pitch nil :font nhe/font-family :height nhe/font-size-small :weight 'regular)

(use-package ligature
  :disabled
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

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom 
  (doom-modeline-height 15)
  (doom-themes-visual-bell-config)
  :config
  (display-battery-mode t)
  (display-time-mode t))

(use-package time
  :config
  (setq display-time-format "%a %d/%m %H:%M")
        display-time-day-and-date t
        display-time-default-load-average nil)

(use-package treemacs
  :straight t
  :config
  (progn
    (setq 
      treemacs-position 'right))
  (treemacs-git-mode 'deferred))

(use-package treemacs-evil
  :straight t
  :after treemacs evil)

(use-package treemacs-projectile
  :straight t
  :after treemacs projectile)

(use-package treemacs-magit
  :straight t
  :after treemacs magit)

(use-package treemacs-all-the-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package dashboard
  :straight t
  :ensure t
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

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4)
  (setq which-key-sort-order 'which-key-prefix-then-key-order))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :straight t
  :config
  (general-auto-unbind-keys)
  (general-override-mode +1)

  (general-create-definer nhe/leader-key-hydra
    :states '(normal insert visual emacs treemacs)
    :keymap 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer nhe/leader-key
    :states '(normal insert visual emacs treemacs)
    :keymap 'override
    :prefix "C-M-SPC"
    :global-prefix "C-M-SPC"
    :non-normal-prefix "C-M-SPC")

  (general-create-definer nhe/local-leader-key
    :states '(normal insert visual emacs treemacs)
    :keymap 'override
    :prefix "C-M-SPC m"
    :global-prefix "C-M-SPC m"
    :non-normal-prefix "C-M-SPC m"))

(nhe/leader-key-hydra
  "/"   '(evilnc-comment-or-uncomment-lines :wk "comment/uncomment")
  ";"   '(counsel-M-x :wk "M-x")
  "."   '(counsel-find-file :wk "find file")
  "SPC" '(counsel-projectile-find-file :wk "find file project")
  "TAB" '(evil-switch-to-windows-last-buffer :wk "switch to previous buffer")
  "b" '(hydra-buffers/body :wk "buffers...")
  "d" '(hydra-dates/body :wk "dates...")
  "f" '(hydra-file/body :wk "file...")
  "g" '(hydra-git/body :wk "git...")
  "h" '(hydra-help/body :wk "help...")
  "l" '(hydra-lsp/body :wk "lsp...")
  "m" '(nhe/hydra-super :wk "mode...")
  "o" '(hydra-open/body :wk "open...")
  "p" '(hydra-projectile/body :wk "projectile...")
  "q" '(hydra-quit/body :wk "quit...") 
  "s" '(hydra-search/body :wk "search...")
  "t" '(hydra-toggle/body :wk "toggle...")
  "w" '(hydra-window/body :wk "window..."))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :bind 
  (:map evil-motion-state-map
   ("q" . nil))
  :config
  (evil-mode 1)
  ;;(evil-define-key 'normal 'insert 'visual (kbd "C-c") 'hydra-master/body)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

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

(nhe/leader-key
  "t" '(:ignore t :wk "toggle"))

(nhe/local-leader-key
  :keymaps 'prog-mode
  "=" '(:ignore t :wk "format")
  "d" '(:ignore t :wk "documentation")
  "g" '(:ignore t :wk "goto")
  "i" '(:ignore t :wk "insert"))

(use-package key-chord
  :straight t
  :defer t
  :config
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map  "kj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package hydra
  :straight t
  :custom 
  (hydra-default-hint nil)
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" (text-scale-adjust 0.1) "in")
    ("k" (text-scale-adjust -0.1) "out")
    ("f" nil "finished" :exit t))

  (nhe/leader-key
    "t s" '(hydra-text-scale/body :wk "scale text")))

(defvar-local nhe/hydra-super-body nil)

(defun nhe/hydra-heading (&rest headings)
  "Format HEADINGS to look preatty in a hydra docstring"
  (mapconcat (lambda (it)
               (propertize (format "%-20s" it) 'face 'shadow))
             headings
             nil))

(defun nhe/hydra-set-super ()
  (when-let* ((suffix "-mode")
              (position (- (length suffix)))
              (mode (symbol-name major-mode))
              (name (if (string= suffix (substring mode position))
                        (substring mode 0 position)
                     mode))
              (body (intern (format "hydra-%s/body" name))))
   (when (functionp body)
     (setq nhe/hydra-super-body body))))

(defun nhe/hydra-super ()
  (interactive)
  (if nhe/hydra-super-body
      (funcall nhe/hydra-super-body)
    (user-error "nhe/hydra-super: nhe/hydra-super-body is not set!")))

(defhydra hydra-buffers (:color blue)
  (concat "\n " (nhe/hydra-heading "Buffer" "Manage" "Next/Prev")
          "
 _q_ quit              _b_ switch            _n_ next        ^^
 ^^                    _d_ kill              _p_ prev        ^^
 ^^                    _i_ ibuffer           ^^              ^^
 ^^                    _s_ save              ^^              ^^
")
  ("q" nil)
  ("b" counsel-switch-buffer)
  ("d" kill-current-buffer)
  ("i" ibuffer-list-buffers)
  ("s" save-buffer)
  ("n" evil-next-buffer :color red)
  ("p" evil-prev-buffer :color red))

(defhydra hydra-dates (:color blue)
  (concat "\n " (nhe/hydra-heading "Dates" "Insert" "Insert with Time")
          "
 _q_ quit              _d_ short             _D_ short             ^^
 ^^                    _i_ iso               _I_ iso               ^^
 ^^                    _l_ long              _L_ long              ^^
")
  ("q" nil)
  ("d" nhe/date-short)
  ("D" nhe/date-short-with-time)
  ("i" nhe/date-iso)
  ("I" nhe/date-iso-with-time)
  ("l" nhe/date-long)
  ("L" nhe/date-long-with-time))

(defhydra hydra-file (:color blue)
  (concat "\n " (nhe/hydra-heading "File" "Operations")
          "
 _q_ quit              _f_ find file             ^^            ^^
 _o_ open...           _s_ save file             ^^            ^^
 ^^                    _r_ recover file          ^^            ^^
")
  ("q" nil)
  ("o" hydra-find-file/body)
  ("f" counsel-find-file)
  ("s" save-buffer)
  ("r" recover-file))

(defhydra hydra-find-file (:color blue)
  (concat "\n " (nhe/hydra-heading "Find Files" "Dotfiles" "Notes")
          "
 _q_ quit              _e_ emacs             _n_ notes        ^^
 ^^                    _d_ desktop           _b_ blog         ^^
 ^^                    _c_ configs           _s_ self study   ^^
 ^^                    ^^                    ^^               ^^
")
  ("q" nil)
  ("e" (lambda () (interactive) (find-file "~/.emacs.d/Emacs.org")))
  ("d" (lambda () (interactive) (find-file "~/Desktop.org")))
  ("c" (lambda () (interactive) (find-file "~/README.org")))
  ("n" (lambda () (interactive) (counsel-find-file "~/Documents/Org/")))
  ("b" (lambda () (interactive) (counsel-find-file "~/Documents/Blog/")))
  ("s" (lambda () (interactive) (counsel-find-file "~/Documents/Org/Study"))))

(defhydra hydra-git (:color blue)
  (concat "\n " (nhe/hydra-heading "Git" "Do")
          "
 _q_ quit              _b_ blame             _p_ previous          ^^
 ^^                    _c_ clone             _n_ next              ^^
 ^^                    _g_ status            _r_ revert            ^^
 ^^                    _i_ init              _s_ stage             ^^
")
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-clone)
  ("g" magit-status)
  ("i" magit-init)
  ("n" git-gutter:next-hunk :color red)
  ("p" git-gutter:previous-hunk :color red)
  ("r" git-gutter:revert-hunk)
  ("s" git-gutter:stage-hunk :color red))

(defhydra hydra-help (:color blue)
  (concat "\n " (nhe/hydra-heading "Help" "Describe" "") 
          "
 _q_ quit              _f_ describe function     _k_ describe key         ^^
 ^^                    _p_ describe package      _b_ describe binding     ^^
 ^^                    _m_ describe mode         _v_ describe variable    ^^
")
  ("q" nil)
  ("f" describe-function)
  ("p" describe-package)
  ("m" describe-mode)
  ("k" describe-key)
  ("b" counsel-descbinds)
  ("v" describe-variable))

(defhydra hydra-open (:color blue)
  (concat "\n " (nhe/hydra-heading "Open" "Management" "Tools")
          "
 _q_ quit              _p_ project sidebar   _w_ world clock ^^
 ^^                    ^^                    ^^              ^^
 ^^                    ^^                    ^^              ^^
 ^^                    ^^                    ^^              ^^
")
  ("q" nil)
  ("p" treemacs)
  ("w" display-time-world))

(defhydra hydra-projectile (:color blue)
  (concat "\n " (nhe/hydra-heading "Projectile" "Do" "Find" "Search")
          "
 _q_ quit              _K_ kill buffers      _b_ buffer            _r_ replace
 ^^                    _i_ reset cache       _d_ directory         _R_ regexp replace
 ^^                    _n_ new               _D_ root              _s_ rg
 ^^                    _S_ save buffers      _f_ file              ^^
 ^^                    ^^                    _p_ project           ^^
")
  ("q" nil)
  ("b" counsel-projectile-switch-to-buffer)
  ("d" counsel-projectile-find-dir)
  ("D" projectile-dired)
  ("f" counsel-projectile-find-file)
  ("i" projectile-invalidate-cache :color red)
  ("K" projectile-kill-buffers)
  ("n" projectile-add-known-project)
  ("p" counsel-projectile-switch-project)
  ("r" projectile-replace)
  ("R" projectile-replace-regexp)
  ("s" counsel-projectile-rg)
  ("S" projectile-save-project-buffers))

(defhydra hydra-quit (:color blue)
  (concat "\n " (nhe/hydra-heading "Quit" "Emacs") 
          "
 _q_ quit              _s_ save and quit     ^^              ^^
 ^^                    _Q_ quit no-save      ^^              ^^
 ^^                    _r_ restart emacs     ^^              ^^
 ^^                    ^^                    ^^              ^^
")
  ("q" nil)
  ("s" save-buffers-kill-emacs)
  ("Q" kill-emacs)
  ("r" restart-emacs))

(defhydra hydra-search (:color blue)
  (concat "\n " (nhe/hydra-heading "Search" "Buffer" "Project")
          "
 _q_ quit        _s_ search buffer      _p_ search project   ^^
 ^^              _r_ replace buffer     _R_ replace project  ^^
 ^^              ^^                     ^^                   ^^
 ^^              ^^                     ^^                   ^^
 ^^              ^^                     ^^                   ^^
")
  ("q" nil)
  ("s" swiper)
  ("p" counsel-projectile-rg)
  ("r" query-replace)
  ("R" projectile-replace-regexp))

(defhydra hydra-toggle (:color blue)
  (concat "\n " (nhe/hydra-heading "Toggle" "UI" "Line numbers")
          "
 _q_ quit        _m_ maximize           _n_ mode: %s`display-line-numbers                                        ^^
 ^^              ^^                     _N_ absolute: %s`display-line-numbers-current-absolute                   ^^
 ^^              ^^                     ^^                                                                       ^^
 ^^              ^^                     ^^                                                                       ^^
 ^^              ^^                     ^^                                                                       ^^
")
  ("q" nil)
  ("m" toggle-frame-maximized :color blue)
  ("n" nhe/display-line-numbers-toggle-type)
  ("N" nhe/display-line-numbers-toggle-absolute))

(defhydra hydra-window (:color blue)
  (concat "\n " (nhe/hydra-heading "Window" "Movements" "Manage" "Split")
          "
 _q_ quit        _h_ window left        _w_ flip windows               _v_ split horizontally
 ^^              _j_ window down        _s_ swap window                _b_ split vertically
 ^^              _k_ window up          _d_ delete window              ^^
 ^^              _l_ window right       _o_ delete other windows       ^^
")
  ("q" nil)
  ("h" evil-window-left :color red)
  ("j" evil-window-down :color red)
  ("k" evil-window-up :color red)
  ("l" evil-window-right :color red)
  ("w" aw-flip-window)
  ("s" ace-swap-window)
  ("d" evil-window-delete)
  ("o" delete-other-windows)
  ("v" evil-window-split)
  ("b" evil-window-vsplit))

(defhydra hydra-org (:color pink)
  (concat "\n " (nhe/hydra-heading "Org" "Links" "Outline")
          "
 _q_ quit              _i_ insert            _<_ previous          ^^
 ^^                    _n_ next              _>_ next              ^^
 ^^                    _p_ previous          _a_ all               ^^
 ^^                    _s_ store             _g_ go                ^^
 ^^                    ^^                    _v_ overview          ^^
")
  ("q" nil)
  ("<" org-backward-element)
  (">" org-forward-element)
  ("a" outline-show-all :color blue)
  ("g" counsel-org-goto :color blue)
  ("i" org-insert-link :color blue)
  ("n" org-next-link)
  ("p" org-previous-link)
  ("s" org-store-link)
  ("v" org-overview :color blue))

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
  :after counsel
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :straight t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1)) 

(use-package smex 
  :straight t
  :defer 1
  :after counsel)

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :straight t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package smartparens
  :straight t
  :init (smartparens-global-mode 1)
  :config
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))

(show-paren-mode t)

(setq show-paren-style 'expression)

(use-package undo-tree
  :straight t
  :init (global-undo-tree-mode 1)
  :config
  (defhydra hydra-undo-tree (:timeout 4)
    "undo / redo"
    ("u" undo-tree-undo "undo")
    ("r" undo-tree-redo "redo")
    ("t" undo-tree-visualize "undo-tree visualize" :exit t))

  (nhe/leader-key
    "u" '(hydra-undo-tree/body :wk "undo/redo")))

(use-package super-save
  :straight t
  :ensure t
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

(use-package evil-nerd-commenter
  :straight t)

(use-package expand-region
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :straight t
  :config
  (rainbow-mode 1))

(use-package pdf-tools
  :straight t
  :pin manual ;; manually update
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package company
  :straight t
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
  (setq company-auto-commit t))

(use-package company-prescient
  :straight t
  :init (company-prescient-mode 1))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :straight t
  :hook (after-init-hook . global-flycheck-mode))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/Rrojects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :straight t
  :config (counsel-projectile-mode))

(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :disabled t
  :straight t
  :after magit)

(use-package git-gutter-fringe
  :straight t
  :preface
  (defun nhe/git-gutter-enable ()
    (when-let* ((buffer (buffer-file-name))
                (backend (vc-backend buffer)))
      (require 'git-gutter)
      (require 'git-gutter-fringe)
      (git-gutter-mode 1)))
  :hook
  (after-change-major-mode . nhe/git-gutter-enable)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [192] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [192] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:modified [192] nil nil '(center t)))

(use-package gitattributes-mode
  :straight t)
(use-package gitconfig-mode
  :straight t)
(use-package gitignore-mode
  :straight t)

(use-package git-commit
  :straight t
  :hook
  (git-commit-mode . (lambda () (setq-local fill-column 72)))
  :custom
  (git-commit-summary-max-length 50))

(defun he/org-font-setup ()
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.1)
                (org-level-2 . 1.05)
                (org-level-3 . 1.0)
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
  :hook 
  (org-mode . he/org-mode-setup)
  (org-mode . nhe/hydra-set-super)
  :config
  (setq org-ellipsis " ")

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
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun he/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . he/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun nhe/org-babel-tangle-config ()
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nhe/org-babel-tangle-config 
                                              'run-at-end 'only-in-org-mode)))

(use-package org-make-toc
  :straight t
  :hook (org-mode . org-make-toc-mode))

(use-package org-mime
  :straight t)

(use-package restart-emacs
  :straight t)

(setq display-time-world-list
  '(("America/Los_Angeles" "Seattle")
    ("America/New_York" "New York")
    ("Europe/Stockholm" "Stockholm")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

(use-package elcord
  :straight t
  :config
  (elcord-mode 1))

(use-package wakatime-mode 
  :straight t
  :defer 2
  :config
  (setq wakatime-api-key nhe/waka-time-token)
  (global-wakatime-mode))

(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq-default
 display-line-numbers-current-absolute nil        ; Current line is 0
 display-line-numbers-type 'relative              ; Prefer relative numbers
 display-line-numbers-width 2)                    ; Enforce width to reduce computation

(defun nhe/display-line-numbers-toggle-absolute ()
  "Toggle the value of `display-line-numbers-current-absolute'."
  (interactive)
  (let ((value display-line-numbers-current-absolute))
    (setq-local display-line-numbers-current-absolute (not value))))

(defun nhe/display-line-numbers-toggle-type ()
  "Cycle through the possible values of `display-line-numbers'.
Cycle between nil, t and 'relative."
  (interactive)
  (let* ((range '(nil t relative))
         (position (1+ (cl-position display-line-numbers range)))
         (position (if (= position (length range)) 0 position)))
    (setq-local display-line-numbers (nth position range))))

(defvar nhe/current-transparency 100 "Current transparency")
(defun change-transparency (n)
  "change transparency to a given value"
  (interactive "nValue: ")
  (setq nhe/current-transparency n)
  (set-frame-parameter (selected-frame) 'alpha `(,n . ,n))
  (add-to-list 'default-frame-alist `(alpha . (,n . ,n))))

(defun nhe/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun nhe/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun nhe/date-long ()
  "Insert the current date, long format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun nhe/date-long-with-time ()
  "Insert the current date, long format, eg. December 09, 2016 - 14:34."
  (interactive)
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

(defun nhe/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun nhe/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016.12.09 14:34"
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))
