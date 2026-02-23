(use-package zig-mode
  :ensure t
  :mode ("\\.\\(zig\\|zon\\)\\'" . zig-mode)
  :hook ((zig-mode . eglot-ensure)
         (zig-mode . (lambda ()
                       (setq-local tab-width 4)
                       (setq-local indent-tabs-mode nil))))
  :config
  (setq zig-format-on-save t)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((zig-mode zig-ts-mode) . ("zls"))))

  (with-eval-after-load 'tnh-treesitter
    (when (fboundp 'tnh/remap-ts-mode)
      (tnh/remap-ts-mode 'zig-mode 'zig-ts-mode 'zig))))

(provide 'tnh-zig)
