;; Better help information
(use-package helpful
  :ensure
  :bind
  (([remap describe-command]  . helpful-command)
   ([remap describe-key]      . helpful-key)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable)
   ("C-h V" . describe-face)
   ("C-h <down>" . view-external-packages)
   :map helpful-mode-map
   ("n" . nil)
   ("h" . forward-button)
   ("p" . nil)
   ("t" . backward-button)))

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
  (define-key Man-mode-map "M-e" #'Man-next-manpage)
  (define-key Man-mode-map "M-p" nil)
  (define-key Man-mode-map "M-i" #'Man-previous-manpage)
  (define-key Man-mode-map "n" nil)
  (define-key Man-mode-map "e" #'Man-next-section)
  (define-key Man-mode-map "p" nil)
  (define-key Man-mode-map "i" #'Man-previous-section))
