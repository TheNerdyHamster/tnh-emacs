(defun tnh/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

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
    ("Firefox" (exwm-workspace-move-window 2))
    ("discord" (exwm-workspace-move-window 4))
    ("mpv" (exwm-floating-toggle-floating)
           (exwm-layout-toggle-mode-line))))

(use-package exwm
  :config
  (setq exwm-workspace-number 5)

  (add-hook 'exwm-update-class-hook #'tnh/exwm-update-class)

  (add-hook 'exwm-update-title-hook #'tnh/exwm-update-title)

  (add-hook 'exwm-manage-finish-hook #'tnh/exwm-configure-window-by-class)

  (add-hook 'exwm-init-hook #'tnh/exwm-init-hook)

  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  (require 'exwm-randr)
  (exwm-randr-enable)

  (tnh/set-wallpaper)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j 
          ?\C-\ )) ;; C+[SPC]

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (setq exwm-input-global-keys
        `(
          ([?\s-r] . exwm-reset)

          ([?\s-h] . windowmove-left)
          ([?\s-l] . windowmove-right)
          ([?\s-k] . windowmove-up)
          ([?\s-j] . windowmove-down)

          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 0)))

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "s-t") 'exwm-floating-toggle-floating)
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-enviorment-brightness-small-increment "2%+")
  (desktop-enviorment-brightness-small-decrement "2%-")
  (desktop-enviorment-brightness-normal-increment "5%+")
  (desktop-enviorment-brightness-normal-decrement "5%-"))
