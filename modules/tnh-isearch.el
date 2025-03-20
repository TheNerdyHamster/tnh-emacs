(use-package anzu
  :ensure t
  :hook (global-anzu-mode . after-init-hook)
  :bind
  (([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap query-replace] . anzu-query-replace))
  :config
  (setq anzu-mode-lighter ""))

(with-eval-after-load 'isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

  (defun tnh/isearch-occur ()
    "Invoke `consult-line' from isearch."
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (consult-line query)))

  (define-key isearch-mode-map (kbd "C-o") 'tnh/isearch-occur)
  (define-key isearch-mode-map (kbd "C-c C-o") 'tnh/isearch-occur))

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

(provide 'tnh-isearch)
