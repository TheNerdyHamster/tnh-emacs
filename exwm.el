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

  ;; (use-package exwm-randr
  ;;   :if nhe/exwm-enabled
  ;;   :after (exwm)
  ;;   :config
  ;;   (exwm-randr-enable))

(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-h
        ?\M-x
        ?\M-&     ;; Async shell command
        ?\M-:     ;; Eval
        ?\C-\M-j  ;; Buffer list
        ?\C-\M-k  ;; Browser list
        ?\C-\     ;; Ctrl+Space
        ?\C-\;))

(defun exwm/run-in-bg (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/bind-function (key invocation &rest bindings)
  "Bind KEYs to FUNCTIONs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (funcall ',invocation)))
    (setq key (pop bindings)
          invocation (pop bindings))))

(defun exwm/bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (exwm/run-in-bg ,command)))
    (setq key (pop bindings)
          command (pop bindings))))

(defun nhe/exwm-init-hook ()
  (exwm-workspace-switch-create 1)
  
  (vterm)
  
  (exwm/run-in-bg "dunst")
  (exwm/run-in-bg "nm-applet")
  (exwm/run-in-bg "redshift -l 57.70716:11.96679 -t 6500:3500"))
  

(use-package exwm
  :if nhe/exwm-enabled
  :config
  (add-hook 'exwm-mode-hook
            (lambda ()
              (evil-local-set-key 'motion (kbd "C-u") nil)))

  (defun nhe/setup-window-by-class-name ()
    (interactive)
    (pcase exwm-class-name
      ("teams-for-linux" (exwm-workspace-move-window 3))
      ("discord" (exwm-workspace-move-window 3))
      ("Spotify" (exwm-workspace-move-window 4))))
      
  (add-hook 'exwm-init-hook #'nhe/exwm-init-hook)
  
  (add-hook 'exwm-manage-finish-hook 
            (lambda () 
              (nhe/setup-window-by-class-name)))
              
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line))))

(use-package exwm-systemtray
  :disabled
  :if nhe/exwm-enabled
  :after (exwm)
  :config
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 35))

(defun nhe/run-xmodmap ()
  (interactive)
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.condig/xmodmap/Xmodmap"))
  
(defun nhe/update-wallpapers ()
  (interactive)
  (start-process-shell-command
    "feh" nil
    (format "feh --bg-scale ~/.config/wallpapers/main.jpg")))
 
(setq nhe/panel-process nil)
(defun nhe/kill-panel ()
  (interactive)
  (when nhe/panel-process
    (ignore-errors
      (kill-process nhe/panel-process)))
  (setq nhe/panel-process nil))

(defun nhe/start-panel ()
  (interactive)
  (nhe/kill-panel)
  (setq nhe/panel-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun nhe/update-screen-layout ()
  (interactive)
  (let ((layout-script "~/.bin/update-screens"))
     (message "Running screen layout script: %s" layout-script)
     (start-process-shell-command "xrandr" nil layout-script)))

(defun nhe/configure-desktop ()
  (interactive)
    ;(nhe/run-xmodmap)
    ;;(nhe/update-screen-layout)
    (run-at-time "2 sec" nil (lambda () (nhe/update-wallpapers))))

(defun nhe/on-exwm-init ()
  (nhe/configure-desktop)
  (nhe/start-panel))

(when nhe/exwm-enabled
  ;; Configure the desktop for first load
  (add-hook 'exwm-init-hook #'nhe/on-exwm-init))

(defalias 'switch-to-buffer-original 'exwm-workspace-switch-to-buffer)

(defun nhe/send-polybar-hook (name number)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" name number)))

(defun nhe/update-polybar-exwm ()
  (nhe/send-polybar-hook "exwm" 1))

(defun nhe/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun nhe/telega-normalize-name (chat-name)
  (let* ((trimmed-name (string-trim-left (string-trim-right chat-name "}") "◀{"))
         (first-name (nth 0 (split-string trimmed-name " "))))
    first-name))

(defun nhe/propertized-to-polybar (buffer-name)
  (if-let* ((text (substring-no-properties buffer-name))
            (fg-face (get-text-property 0 'face buffer-name))
            (fg-color (face-attribute fg-face :foreground)))
    (format "%%{F%s}%s%%{F-}" fg-color (nhe/telega-normalize-name text))
    text))

(add-hook 'exwm-workspace-switch-hook #'nhe/update-polybar-exwm)

(when nhe/exwm-enabled
  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\M-k  ;; Browser list
      ?\C-\     ;; Ctrl+Space
      ?\C-\;))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (defun exwm/run-vimb ()
    (exwm/run-in-background "vimb")
    (exwm-workspace-switch-create 2))

  (exwm/bind-function
    "s-o" 'exwm/run-vimb)

  (exwm/bind-command
    "s-p" "playerctl play-pause"
    "s-[" "playerctl previous"
    "s-]" "playerctl next")

  (use-package desktop-environment
    :after exwm
    :config (desktop-environment-mode)
    :custom
    (desktop-environment-brightness-small-increment "2%+")
    (desktop-environment-brightness-small-decrement "2%-")
    (desktop-environment-brightness-normal-increment "5%+")
    (desktop-environment-brightness-normal-decrement "5%-"))

  ;; This needs a more elegant ASCII banner
  (defhydra hydra-exwm-move-resize (:timeout 4)
    "Move/Resize Window (Shift is bigger steps, Ctrl moves window)"
    ("j" (lambda () (interactive) (exwm-layout-enlarge-window 10)) "V 10")
    ("J" (lambda () (interactive) (exwm-layout-enlarge-window 30)) "V 30")
    ("k" (lambda () (interactive) (exwm-layout-shrink-window 10)) "^ 10")
    ("K" (lambda () (interactive) (exwm-layout-shrink-window 30)) "^ 30")
    ("h" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 10)) "< 10")
    ("H" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 30)) "< 30")
    ("l" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 10)) "> 10")
    ("L" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 30)) "> 30")
    ("C-j" (lambda () (interactive) (exwm-floating-move 0 10)) "V 10")
    ("C-S-j" (lambda () (interactive) (exwm-floating-move 0 30)) "V 30")
    ("C-k" (lambda () (interactive) (exwm-floating-move 0 -10)) "^ 10")
    ("C-S-k" (lambda () (interactive) (exwm-floating-move 0 -30)) "^ 30")
    ("C-h" (lambda () (interactive) (exwm-floating-move -10 0)) "< 10")
    ("C-S-h" (lambda () (interactive) (exwm-floating-move -30 0)) "< 30")
    ("C-l" (lambda () (interactive) (exwm-floating-move 10 0)) "> 10")
    ("C-S-l" (lambda () (interactive) (exwm-floating-move 30 0)) "> 30")
    ("f" nil "finished" :exit t))

  ;; Workspace switching
  (setq exwm-input-global-keys
         `(([?\s-\M-r] . exwm-reset)
           ([?\s-w] . exwm-workspace-switch)
           ([?\s-r] . hydra-exwm-move-resize/body)
           ([?\s-e] . dired-jump)
           ([?\s-E] . (lambda () (interactive) (dired "~")))
           ([?\s-q] . (lambda () (interactive) (kill-buffer)))
           ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
           ,@(mapcar (lambda (i)
                       `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

  (exwm-input-set-key (kbd "<s-return>") 'vterm)
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-m") 'exwm-layout-toggle-fullscreen))
