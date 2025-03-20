(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\.html\\'" . markdown-mode)
  :config
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(provide 'tnh-markdown)
