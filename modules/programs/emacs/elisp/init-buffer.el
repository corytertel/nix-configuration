;; Buffer management
(with-eval-after-load 'ibuffer
  ;; Minimak binds
  (define-key ibuffer-mode-map (kbd "M-n") nil)
  (define-key ibuffer-mode-map (kbd "M-p") nil)
  (define-key ibuffer-mode-map (kbd "M-k") #'ibuffer-backward-filter-group)
  (define-key ibuffer-mode-map (kbd "M-j") #'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "l") nil)
  (define-key ibuffer-mode-map (kbd "n") nil)
  (define-key ibuffer-mode-map (kbd "p") #'ibuffer-redisplay)
  (define-key ibuffer-mode-map (kbd "k") #'ibuffer-backward-line)
  (define-key ibuffer-mode-map (kbd "j") #'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "M-f a C-M-f") #'ibuffer-do-isearch-regexp)
  (define-key ibuffer-mode-map (kbd "M-f a C-o") #'ibuffer-do-occur)
  (define-key ibuffer-mode-map (kbd "M-f a C-f") #'ibuffer-do-isearch))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(add-hook 'ibuffer-mode-hook (lambda ()
			       (hl-line-mode 1)
			       (setq-local cursor-type nil)))
