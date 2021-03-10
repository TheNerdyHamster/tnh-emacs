(defun tnh/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun tnh/exwm-run-shell-command (cmd)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command cmd nil cmd))

(defun tnh/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil "feh --bg-scale ~/Pictures/wallpapers/001.jpg"))

(defun tnh/exwm-init-hook ()
  (exwm-workspace-switch-create 1)

  (vterm)

  (tnh/run-in-background "nm-applet"))

(defun tnh/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun tnh/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun tnh/exwm-configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("discord" (exwm-layout-toggle-mode-line))
    ("Slack" (exwm-layout-toggle-mode-line))
    ("firefox" (exwm-layout-toggle-mode-line))
    ("mpv" (exwm-floating-toggle-floating)
           (exwm-layout-toggle-mode-line))))

(defun tnh/update-displays ()
  (tnh/run-in-background "autorandr -c --force")
  (tnh/set-wallpaper)
  (tnh/set-exwm-workspaces)
  (message "Display config %s"
           (tnh/get-current-display-configuration)))

(defun tnh/set-exwm-workspaces ()
  (setq exwm-randr-workspace-monitor-plist
        (pcase (tnh/get-current-display-configuration)
          ("docked-home" '(1 "eDP1" 0 "DP3"))
          ("mobile" '(0 "eDP1")))))
 ; (exwm-randr-refresh))

(defun tnh/get-current-display-configuration ()
  (string-trim (shell-command-to-string "autorandr --current")))

(use-package exwm
  :demand
  :bind
  (:map exwm-mode-map
    ("C-q" . exwm-input-send-next-key))
  :hook
  (exwm-update-class . tnh/exwm-update-class)
  (exwm-update-title . tnh/exwm-update-title)
  (exwm-manage-finish . tnh/exwm-configure-window-by-class)
  (exwm-init . tnh/exwm-init-hook)
  (exwm-randr-screen-change . tnh/update-displays)
  :custom
  (exwm-input-global-keys
        `((,(kbd "s-!") . tnh/exwm-run-shell-command)
          (,(kbd "s-1") . (lambda () (interactive) (exwm-workspace-switch 0)))
          (,(kbd "s-2") . (lambda () (interactive) (exwm-workspace-switch 1)))
          (,(kbd "s-SPC") . counsel-linux-app)
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-r") . exwm-reset)
          (,(kbd "s-R") . exwm-restart)
          (,(kbd "s-w") . delete-window)
          (,(kbd "s-W") . kill-this-buffer)))
  (exwm-input-prefix-keys
      `,@(mapcar (lambda (vector) (aref vector 0))
              `(,(kbd "C-x")
               ,(kbd "C-u")
               ,(kbd "C-h")
               ,(kbd "C-w")
               ,(kbd "M-x")
               ,(kbd "M-`")
               ,(kbd "M-&")
               ,(kbd "M-:")
               ,(kbd "C-M-j")
               ,(kbd "C-SPC")
               ,@(mapcar (lambda (i) (kbd (format "M-%s" i)))
                         (number-sequence 0 9)))))
  (exwm-input-simulation-keys
   `((,(kbd "M-y") . ,(kbd "C-c"))
     (,(kbd "M-p") . ,(kbd "C-v"))))
  (exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))
  (exwm-workspace-warp-cursor t)
  :config
  (setq exwm-workspace-number 2)
  (tnh/exwm-run-shell-command "xmodmap ~/.emacs.d/exwm/Xmodmap")

  (require 'exwm-randr)
  (exwm-randr-enable)

  (tnh/update-displays)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-enviorment-brightness-small-increment "2%+")
  (desktop-enviorment-brightness-small-decrement "2%-")
  (desktop-enviorment-brightness-normal-increment "5%+")
  (desktop-enviorment-brightness-normal-decrement "5%-"))
