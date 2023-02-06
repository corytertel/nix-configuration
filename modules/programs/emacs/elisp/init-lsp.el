;; LSP
(use-package eglot
  :after flymake corfu
  :defer t
  :ensure t

  :hook
  (c-mode . cory/eglot-ensure)
  (c++-mode . cory/eglot-ensure)
  (racket-mode . cory/eglot-ensure)
  (clojure-mode . cory/eglot-ensure)
  (clojurescript-mode . cory/eglot-ensure)
  (clojurec-mode . cory/eglot-ensure)
  ;; (java-mode . cory/eglot-ensure)
  (python-mode . cory/eglot-ensure)
  (eglot-managed-mode . cory/eglot-super-capf)

  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p 2)

  :config
  (define-key eglot-mode-map [remap display-local-help] nil)
  ;; (add-to-list 'eglot-server-programs
  ;;              `(scheme-mode . ("chicken-lsp-server")))
  (add-to-list 'eglot-server-programs
	       `(clojure-mode . ("clojure-lsp")))
  ;; (add-to-list 'eglot-server-programs
  ;;              `(nix-mode . ("nil")))

  (defun cory/eglot-super-capf ()
    (setq-local completion-at-point-functions
		(list (cape-super-capf
		       #'tempel-complete
		       #'eglot-completion-at-point)
		      #'cape-dabbrev
		      #'cape-file)))

  (defun cory/eglot-ensure ()
    "Ensures that `eglot-ensure' is only run during local connections."
    (unless (file-remote-p buffer-file-name)
      (eglot-ensure)))

  :bind (:map eglot-mode-map
	 ("C-c C-a" . eglot-code-actions)
	 ("C-c C-f" . eglot-format-buffer)
	 ("C-c x"   . eglot-rename)))

;; Tree-sitter
;; (use-package tree-sitter
;;   :hook
;;   (c-mode . tree-sitter-setup)
;;   (c++-mode . tree-sitter-setup)
;;   (java-mode . tree-sitter-setup)

;;   :config
;;   (defun tree-sitter-setup ()
;;     (require 'tree-sitter)
;;     (require 'tree-sitter-langs)
;;     (require 'tree-sitter-hl)
;;     (tree-sitter-hl-mode)))

;; (use-package tree-sitter-langs)
