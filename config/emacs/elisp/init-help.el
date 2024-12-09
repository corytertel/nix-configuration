;; Help binds
(global-set-key (kbd "C-h h") #'help-for-help)

;; Better help information
(use-package helpful
  :ensure
  :bind
  (([remap describe-command]  . helpful-command)
   ([remap describe-key]      . helpful-key)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable)
   ("C-h V" . describe-face))
  :config
  ;; (add-to-list 'evil-emacs-state-modes 'helpful-mode)
  ;; (add-to-list 'evil-insert-state-modes 'helpful-mode)
  (evil-define-key 'normal 'helpful-mode-map (kbd "q") #'quit-window))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer nil))
