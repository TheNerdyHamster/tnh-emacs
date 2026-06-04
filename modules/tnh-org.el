;;; tnh-org.el --- Emacs Org mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun tnh/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.09)
                  (org-level-2 . 1.08)
                  (org-level-3 . 1.07)
                  (org-level-4 . 1.06)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.04)
                  (org-level-7 . 1.04)
                  (org-level-8 . 1.04)))
    (set-face-attribute (car face) nil :font "SF Pro" :weight 'regular :height (cdr face)))

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
  (visual-line-mode 1)
  (setq show-trailing-whitespace nil))

(defun tnh/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'tnh/org-babel-tangle-config)))

(use-package org
  :commands (org-capture org-agenda)
  :hook ((org-mode . tnh/org-mode-setup)
         (org-mode . tnh/org-font-setup))
  :custom
  (org-ellipsis " ▾")
  (org-agenda-files '("~/Documents/Org/"))
  (org-hide-emphasis-markers t)
  :config
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-mode . org-modern-agenda))
  :custom
  (org-modern-star '("◉" "○" "●" "○" "●" "○" "●"))
  (org-modern-list '((?- . "•") (?+ . "◦"))))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks t))

(defun tnh/org-mode-visual-fill ()
  (setq visual-fill-column-width 140
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :after org
  :hook (org-mode . tnh/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(provide 'tnh-org)

;;; tnh-org.el ends here
