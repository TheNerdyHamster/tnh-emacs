;;; tnh-fonts.el -- Emacs font settings
;;; Commentary:
;;; Code:
(set-face-attribute 'default nil
  :family "Iosevka Nerd Font"
  :height 140
  :weight 'normal
  :width 'normal)

;; Also set the 'fixed-pitch' face for consistency in code buffers
(set-face-attribute 'fixed-pitch nil
  :family "Iosevka Nerd Font"
  :height 140
  :weight 'normal
  :width 'normal)

;; Set a different, more readable font for prose
(set-face-attribute 'variable-pitch nil
  :family "SF Pro" ;; Or any other proportional font you like
  :height 150)

(provide 'tnh-fonts)

;;; tnh-fonts.el ends here
