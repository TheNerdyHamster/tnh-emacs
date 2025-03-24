(use-package origami
  :ensure t
  :bind
  (("C-c f" . origami-recursively-toggle-node)
   ("C-c F" . origami-toggle-all-nodes)))

(provide 'tnh-folding)
