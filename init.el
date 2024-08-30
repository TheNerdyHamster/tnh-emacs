(defgroup tnh-emacs nil
  "User options for TNH Emacs.
The tnh-emacs-pre-custom.el file must exist and be located
in the same directy as the init.el."
  :group 'file)

(defcustom tnh-emacs-load-vterm nil
  "When non-nil, enable vterm package.
This user option must be set in the `tnh-emacs-pre-custom.el' file."
  :group 'tnh-emacs
  :type 'boolean)

(defcustom tnh-emacs-load-which-key nil
  "When non-nil, enable keybinding hints after a short delay.
This user option must be set in the `tnh-emacs-pre-custom.el' file."
  :group 'tnh-emacs
  :type 'boolean)

(defun tnh/display-startup-info ()
  (message "TNH-Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
	(time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'tnh/display-startup-info)

(add-to-list 'load-path (locate-user-emacs-file '"modules"))

;; Load pre-custom file before loading any module.
(load (locate-user-emacs-file "tnh-emacs-pre-custom.el") :no-error :no-messag)

(require 'tnh-emacs-package)
(require 'tnh-emacs-theme)
(require 'tnh-emacs-keys)

(require 'tnh-emacs-core)
(require 'tnh-emacs-interface)

(require 'tnh-emacs-code)
(require 'tnh-emacs-lsp)
(require 'tnh-emacs-term)
(require 'tnh-emacs-dired)
(require 'tnh-emacs-ledger)
(require 'tnh-emacs-custom)
