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
