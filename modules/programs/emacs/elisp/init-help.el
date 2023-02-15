;; Better help information
(use-package helpful
  :ensure
  :bind
  (([remap describe-command]  . helpful-command)
   ([remap describe-key]      . helpful-key)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable)
   ;; Minimak binds
   :map helpful-mode-map
   ("n" . nil)
   ("e" . forwart-button)
   ("p" . nil)
   ("i" . backward-button)))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer nil))
