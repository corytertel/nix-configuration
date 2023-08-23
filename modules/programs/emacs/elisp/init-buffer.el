;; Buffer management
(with-eval-after-load 'ibuffer
  ;; Minimak binds
  (define-key ibuffer-mode-map (kbd "M-n") nil)
  (define-key ibuffer-mode-map (kbd "M-p") nil)
  (define-key ibuffer-mode-map (kbd "M-t") #'ibuffer-backward-filter-group)
  (define-key ibuffer-mode-map (kbd "C-<up>") #'ibuffer-backward-filter-group)
  (define-key ibuffer-mode-map (kbd "M-h") #'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "C-<down>") #'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "n") nil)
  (define-key ibuffer-mode-map (kbd "p") nil)
  (define-key ibuffer-mode-map (kbd "t") #'ibuffer-backward-line)
  (define-key ibuffer-mode-map (kbd "h") #'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "i") #'describe-mode)
  (define-key ibuffer-mode-map (kbd "M-f a C-M-f") #'ibuffer-do-isearch-regexp)
  (define-key ibuffer-mode-map (kbd "M-f a C-o") #'ibuffer-do-occur)
  (define-key ibuffer-mode-map (kbd "M-f a C-f") #'ibuffer-do-isearch))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(add-hook 'ibuffer-mode-hook (lambda ()
			       (hl-line-mode 1)
			       (setq-local cursor-type nil)))
