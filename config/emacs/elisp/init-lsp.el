
;; Eglot
(use-package eglot
  :after corfu flymake
  :hook
  ((c++-mode
    python-mode
    javascript-mode
    js-mode
    js-jsx-mode
    js-ts-mode
    typescript-ts-mode
    tsx-ts-mode
    nix-mode
    perl-mode
    cperl-mode
    csharp-mode
    powershell-mode)
   . cory/eglot)
  (eglot-managed-mode
   . (lambda ()
       (setq-local completion-at-point-functions
		   (list (cape-capf-super
			  ;; (cape-company-to-capf #'company-yasnippet)
			  #'yasnippet-capf
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

  ;; Add nix lsp
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))

  ;; ;; Add web-mode
  ;; (add-to-list 'eglot-server-programs
  ;;              '((web-mode)
  ;;       	 "typescript-language-server" "--stdio"))

  ;; Add csharp lsp
  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("OmniSharp" "-lsp")))

  ;; Add powershell lsp (PowerShellEditorServices)
  (defvar cory/powershell-lsp-dir
    (expand-file-name (concat user-emacs-directory "PowerShellEditorServices/")))

  ;; Should be idenpotent (I believe it is)
  ;; Requires curl and unzip
  (let ((pwsh-lsp-dir cory/powershell-lsp-dir)
        (src "https://github.com/PowerShell/PowerShellEditorServices/releases/download/v4.1.0/PowerShellEditorServices.zip"))
    (unless (file-exists-p pwsh-lsp-dir)
      (make-directory pwsh-lsp-dir)
      (async-shell-command
       (concat "curl -L " src " > " pwsh-lsp-dir "PowerShellEditorServices.zip" " && "
               "unzip " pwsh-lsp-dir "PowerShellEditorServices.zip -d " pwsh-lsp-dir " && "
               "rm " pwsh-lsp-dir "PowerShellEditorServices.zip"))))

  (let ((start-script
         (concat cory/powershell-lsp-dir
                 "PowerShellEditorServices/Start-EditorServices.ps1")))
    (add-to-list
     'eglot-server-programs
     `(powershell-mode
       . ("pwsh" "-NoLogo" "-NoProfile" "-Command" ,start-script "-Stdio"))))

  ;; Ignore logging for speed
  (fset #'jsonrpc--log-event #'ignore)

  ;; List of language server capabilities:
  ;; (const :tag "Documentation on hover" :hoverProvider)
  ;; (const :tag "Code completion" :completionProvider)
  ;; (const :tag "Function signature help" :signatureHelpProvider)
  ;; (const :tag "Go to definition" :definitionProvider)
  ;; (const :tag "Go to type definition" :typeDefinitionProvider)
  ;; (const :tag "Go to implementation" :implementationProvider)
  ;; (const :tag "Go to declaration" :declarationProvider)
  ;; (const :tag "Find references" :referencesProvider)
  ;; (const :tag "Highlight symbols automatically" :documentHighlightProvider)
  ;; (const :tag "List symbols in buffer" :documentSymbolProvider)
  ;; (const :tag "List symbols in workspace" :workspaceSymbolProvider)
  ;; (const :tag "Execute code actions" :codeActionProvider)
  ;; (const :tag "Code lens" :codeLensProvider)
  ;; (const :tag "Format buffer" :documentFormattingProvider)
  ;; (const :tag "Format portion of buffer" :documentRangeFormattingProvider)
  ;; (const :tag "On-type formatting" :documentOnTypeFormattingProvider)
  ;; (const :tag "Rename symbol" :renameProvider)
  ;; (const :tag "Highlight links in document" :documentLinkProvider)
  ;; (const :tag "Decorate color references" :colorProvider)
  ;; (const :tag "Fold regions of buffer" :foldingRangeProvider)
  ;; (const :tag "Execute custom commands" :executeCommandProvider)
  ;; (const :tag "Inlay hints" :inlayHintProvider)

  ;; Disable some capabilities for speed
  (setq eglot-ignored-server-capabilities
	'(:hoverProvider
	  :signatureHelpProvider
	  :definitionProvider
	  :typeDefinitionProvider
	  :implementationProvider
	  :declarationProvider
	  :referencesProvider
	  :documentHighlightProvider
	  :documentSymbolProvider
	  :workspaceSymbolProvider
	  :codeActionProvider
	  :codeLensProvider
	  :documentFormattingProvider
	  :documentRangeFormattingProvider
	  :documentOnTypeFormattingProvider
	  :renameProvider
	  :documentLinkProvider
	  :colorProvider
	  :foldingRangeProvider
	  :executeCommandProvider
	  :inlayHintProvider))

  :bind (:map eglot-mode-map
	 ("C-c C-a" . eglot-code-actions)
	 ("C-c C-f" . eglot-format-buffer)
	 ("C-c x"   . eglot-rename)))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

;; ;; Lsp-mode
;; (use-package lsp-mode
;;   :after corfu flymake
;;   :commands (lsp lsp-deferred)
;;   :hook
;;   ((java-mode
;;     c-mode
;;     c++-mode
;;     python-mode
;;     javascript-mode
;;     js-mode
;;     js-jsx-mode
;;     js-ts-mode
;;     typescript-ts-mode
;;     powershell-mode)
;;    . cory/lsp)
;;   (lsp-mode . (lambda ()
;; 		(setq-local completion-at-point-functions
;; 			    (list (cape-capf-super
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
