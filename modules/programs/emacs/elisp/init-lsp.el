;; ;; LSP
;; (use-package eglot
;;   :after flymake corfu
;;   :defer t
;;   :ensure t

;;   :hook
;;   (c-mode . cory/eglot-ensure)
;;   (c++-mode . cory/eglot-ensure)
;;   (racket-mode . cory/eglot-ensure)
;;   (clojure-mode . cory/eglot-ensure)
;;   (clojurescript-mode . cory/eglot-ensure)
;;   (clojurec-mode . cory/eglot-ensure)
;;   ;; (java-mode . cory/eglot-ensure)
;;   (python-mode . cory/eglot-ensure)
;;   (eglot-managed-mode . cory/eglot-super-capf)

;;   :custom
;;   (eglot-autoshutdown t)
;;   (eglot-autoreconnect nil)
;;   (eglot-confirm-server-initiated-edits nil)
;;   (eldoc-idle-delay 1)
;;   (eldoc-echo-area-display-truncation-message nil)
;;   (eldoc-echo-area-use-multiline-p 2)

;;   :config
;;   (define-key eglot-mode-map [remap display-local-help] nil)
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              `(scheme-mode . ("chicken-lsp-server")))
;;   (add-to-list 'eglot-server-programs
;; 	       `(clojure-mode . ("clojure-lsp")))
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              `(nix-mode . ("nil")))

;;   (defun cory/eglot-super-capf ()
;;     (setq-local completion-at-point-functions
;; 		(list (cape-super-capf
;; 		       ;; #'tempel-complete
;; 		       (cape-company-to-capf #'company-yasnippet)
;; 		       #'eglot-completion-at-point)
;; 		      #'cape-dabbrev
;; 		      #'cape-file)))

;;   (defun cory/eglot-ensure ()
;;     "Ensures that `eglot-ensure' is only run during local connections."
;;     (unless (file-remote-p buffer-file-name)
;;       (eglot-ensure)))

;;   :bind (:map eglot-mode-map
;; 	 ("C-c C-a" . eglot-code-actions)
;; 	 ("C-c C-f" . eglot-format-buffer)
;; 	 ("C-c x"   . eglot-rename)))

;; Tree-sitter
(use-package tree-sitter
  :after tree-sitter-langs
  :custom-face
  (tree-sitter-hl-face:property         ((t (:slant normal))))
  (tree-sitter-hl-face:method.call      ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.call    ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:operator         ((t (:inherit default))))
  (tree-sitter-hl-face:type.builtin     ((t (:inherit font-lock-type-face))))
  (tree-sitter-hl-face:number           ((t (:inherit highlight-numbers-number))))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;; Lsp-mode
(use-package lsp-mode
  :after corfu flymake
  :commands (lsp lsp-deferred)
  :hook
  ((java-mode
    c-mode
    c++-mode
    python-mode
    clojure-mode
    clojurescipe-mode
    javascript-mode
    typescript-ts-mode)
   . cory/lsp)
  (lsp-mode . (lambda ()
		(company-mode -1)
		(setq-local completion-at-point-functions
			    (list (cape-super-capf
				   (cape-company-to-capf #'company-yasnippet)
				   #'lsp-completion-at-point)
				  #'cape-file))))
  :init
  (setq lsp-keymap-prefix "C-c C-a")
  :bind (:map lsp-mode-map
         ("C-c C-l" . lsp-execute-code-action)
         ("M-." . lsp-find-definition)
         ("M-," . lsp-find-references)
	 ("C-c C-a r" . lsp-rename)
         ("C-c C-a f" . lsp-format-buffer)
         ("C-c C-a g" . lsp-format-region)
         ("C-c C-a a" . lsp-execute-code-action)
         ("C-c C-a r" . lsp-find-references))
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
  :config
  ;; Performance
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-prefer-capf t)
  ;; Don't watch `build' and `.gradle' directories for file changes
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]build$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.gradle$")
  ;; Turn off breadcrumb trail
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; Turn off warnings
  ;; (setq lsp-warn-no-matched-clients nil)

  (defun cory/lsp ()
    "Ensures that `lsp' is only run during local connections."
    (interactive)
    (unless (file-remote-p buffer-file-name)
      (lsp-deferred)
      (company-mode -1))))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :custom-face
  (lsp-ui-sideline-global ((t (:italic t))))
  (lsp-ui-peek-highlight  ((t (:foreground unspecified :background unspecified :inherit isearch))))
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-mode-map
         ("M-?" . lsp-ui-doc-toggle)
         ("C-c C-a d" . lsp-ui-doc-show)
         ("C-c C-a s" . lsp-ui-find-workspace-symbol))
  :config
  (defun lsp-ui-doc-toggle ()
    "Shows or hides lsp-ui-doc popup."
    (interactive)
    (if lsp-ui-doc--bounds
        (lsp-ui-doc-hide)
      (lsp-ui-doc-show)))

  ;; Deactivate most of the annoying "fancy features"
  ;; (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-doc-use-childframe t)
  ;; (setq lsp-ui-doc-include-signature t)
  ;; (setq lsp-ui-doc-position 'at-point)
  ;; (setq lsp-ui-sideline-enable nil)
  ;; (setq lsp-ui-sideline-show-hover nil)
  ;; (setq lsp-ui-sideline-show-symbol nil)

  (setq lsp-ui-doc-position 'right)

  ;; Enable features
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-symbol t))

;;; Debugging

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
