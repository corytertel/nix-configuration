
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

;; Eglot
(use-package eglot
  :after corfu flymake
  :hook
  (( ;; java-mode
    c-mode
    c++-mode
    python-mode
    clojure-mode
    clojurescript-mode
    javascript-mode
    js-mode
    js-jsx-mode
    js-ts-mode
    typescript-ts-mode
    ;; TODO port lsp-pwsh to eglot
    ;; https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-pwsh.el
    ;; powershell-mode
    )
   . cory/eglot)
  (eglot-managed-mode
   . (lambda ()
       (setq-local completion-at-point-functions
		   (list (cape-super-capf
			  (cape-company-to-capf #'company-yasnippet)
			  ;; #'cape-yasnippet
			  #'eglot-completion-at-point)
			 #'cape-file))))
  ;; :custom
  ;; (eglot-autoshutdown t)
  ;; (eglot-autoreconnect nil)
  ;; (eglot-confirm-server-initiated-edits nil)
  ;; (eldoc-idle-delay 1)
  ;; (eldoc-echo-area-display-truncation-message nil)
  ;; (eldoc-echo-area-use-multiline-p 2)
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit highlight))))
  :config
  ;; (define-key eglot-mode-map [remap display-local-help] nil)

  (defun cory/eglot ()
    "Ensures that `eglot' is only run during local connections."
    (interactive)
    (unless (file-remote-p buffer-file-name)
      (eglot-ensure)))

  :bind (:map eglot-mode-map
	 ("C-c C-a" . eglot-code-actions)
	 ("C-c C-f" . eglot-format-buffer)
	 ("C-c x"   . eglot-rename)))

;; ;; Lsp-mode
;; (use-package lsp-mode
;;   :after corfu flymake
;;   :commands (lsp lsp-deferred)
;;   :hook
;;   ((java-mode
;;     c-mode
;;     c++-mode
;;     python-mode
;;     clojure-mode
;;     clojurescript-mode
;;     javascript-mode
;;     js-mode
;;     js-jsx-mode
;;     js-ts-mode
;;     typescript-ts-mode
;;     powershell-mode)
;;    . cory/lsp)
;;   (lsp-mode . (lambda ()
;; 		(setq-local completion-at-point-functions
;; 			    (list (cape-super-capf
;; 				   (cape-company-to-capf #'company-yasnippet)
;; 				   ;; #'cape-yasnippet
;; 				   #'lsp-completion-at-point)
;; 				  #'cape-file))))
;;   :init
;;   (setq lsp-keymap-prefix "C-c C-a")
;;   :bind (:map lsp-mode-map
;;          ("C-c C-l" . lsp-execute-code-action)
;;          ("M-." . lsp-find-definition)
;;          ("M-," . lsp-find-references)
;; 	 ("C-c C-a r" . lsp-rename)
;;          ("C-c C-a f" . lsp-format-buffer)
;;          ("C-c C-a g" . lsp-format-region)
;;          ("C-c C-a a" . lsp-execute-code-action)
;;          ("C-c C-a r" . lsp-find-references)
;; 	 ;; :map global-map
;; 	 ("C-h ." . cory/display-local-help)
;; 	 ("<help> ." . cory/display-local-help)
;; 	 ;; :map help-map
;; 	 ;; ("." . cory/display-local-help)
;; 	 )
;;   ;; :custom
;;   ;; (lsp-log-io nil)
;;   ;; (lsp-enable-folding nil)
;;   ;; (lsp-diagnostics-provider :flymake)
;;   ;; (lsp-enable-snippet nil)
;;   ;; (lsp-completion-enable nil)
;;   ;; (lsp-enable-symbol-highlighting nil)
;;   ;; (lsp-enable-links nil)
;;   ;; (lsp-restart 'auto-restart)
;;   :custom-face
;;   (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
;;   (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
;;   (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
;;   (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
;;   :config
;;   ;; Performance
;;   (setq lsp-prefer-capf t)
;;   ;; Don't watch `build' and `.gradle' directories for file changes
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]build$")
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.gradle$")
;;   ;; Turn off breadcrumb trail
;;   ;; (setq lsp-headerline-breadcrumb-enable nil)
;;   ;; Turn off warnings
;;   ;; (setq lsp-warn-no-matched-clients nil)

;;   (defun cory/lsp ()
;;     "Ensures that `lsp' is only run during local connections."
;;     (interactive)
;;     (unless (file-remote-p buffer-file-name)
;;       (lsp-deferred)))

;;   (defun cory/display-local-help ()
;;     (interactive)
;;     (if (lsp-mode)
;; 	(call-interactively #'lsp-describe-thing-at-point)
;;       (call-interactively #'display-local-help)))

;;   ;; ;; don't ping LSP lanaguage server too frequently
;;   ;; (defvar lsp-on-touch-time 0)
;;   ;; (defadvice lsp-on-change (around lsp-on-change-hack activate)
;;   ;;   ;; don't run `lsp-on-change' too frequently
;;   ;;   (when (> (- (float-time (current-time))
;;   ;;               lsp-on-touch-time) 30) ;; 30 seconds
;;   ;;     (setq lsp-on-touch-time (float-time (current-time)))
;;   ;;     ad-do-it))
;;   )

;;; Debugging
;; (use-package dap-mode
;;   :after lsp-mode
;;   :config (dap-auto-configure-mode))
