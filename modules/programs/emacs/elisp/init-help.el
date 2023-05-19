;; Better help information
(use-package helpful
  :after orderless
  :ensure
  :bind
  (([remap describe-command]  . cory/helpful-command)
   ([remap describe-key]      . cory/helpful-key)
   ([remap describe-variable] . cory/helpful-variable)
   ([remap describe-function] . cory/helpful-callable)
   ("C-h V" . cory/describe-face)
   ;; Minimak binds
   :map helpful-mode-map
   ("n" . nil)
   ("e" . forward-button)
   ("p" . nil)
   ("i" . backward-button))
  :config
  (defun cory/helpful-command ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (call-interactively #'helpful-command)))
  (defun cory/helpful-key ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (call-interactively #'helpful-key)))
  (defun cory/helpful-variable ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (call-interactively #'helpful-variable)))
  (defun cory/helpful-callable ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (call-interactively #'helpful-callable)))
  (defun cory/describe-face ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (call-interactively #'describe-face))))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer nil))

(use-package hydra)

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
