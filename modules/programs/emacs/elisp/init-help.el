(global-set-key (kbd "<C-i>") search-map)
(global-set-key (kbd "<C-i> <C-i>") #'help-for-help)

;; Better help information
(use-package helpful
  :ensure
  :bind
  (([remap describe-command]  . helpful-command)
   ([remap describe-key]      . helpful-key)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable)
   ("<C-i> V" . describe-face)
   ;; Minimak binds
   :map helpful-mode-map
   ("n" . nil)
   ("j" . forward-button)
   ("p" . nil)
   ("k" . backward-button)))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer nil))

;; (use-package hydra)

;; Man keybinds
(with-eval-after-load 'man
  (define-key Man-mode-map "<keymap> C-M-i" nil)
  (define-key Man-mode-map "<keymap> C-M-s" #'backward-button)
  (define-key Man-mode-map "M-n" nil)
  (define-key Man-mode-map "M-j" #'Man-next-manpage)
  (define-key Man-mode-map "M-p" nil)
  (define-key Man-mode-map "M-k" #'Man-previous-manpage)
  (define-key Man-mode-map "n" nil)
  (define-key Man-mode-map "j" #'Man-next-section)
  (define-key Man-mode-map "p" nil)
  (define-key Man-mode-map "k" #'Man-previous-section))
