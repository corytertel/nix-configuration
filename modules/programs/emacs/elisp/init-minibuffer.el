;; Minibuffer completion
(use-package vertico
  :config
  (recentf-mode t)
  (vertico-mode t))

(use-package vertico-posframe
  :after vertico
  :config (vertico-posframe-mode 1))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Icons in minibuffer
(use-package all-the-icons-completion
  :disabled t
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; Minibuffer visual menu
(use-package consult
  :init
  (setq consult-preview-key nil)
  :bind
  (("C-c f"       . consult-recent-file)
   ("C-x p s"     . consult-ripgrep) ; for use with project.el
   ;; ;; ("C-s"         . consult-line)
   ;; ("C-s"         . consult-line-multi)
   ;; ("C-S-s"       . consult-focus-lines)
   ;; ("C-c i"       . consult-imenu)
   ;; ("C-c t"       . gtags-find-tag)
   ("C-x b"       . consult-buffer)
   ;; ("C-c x"       . consult-complex-command)
   ;; ("C-c e"       . consult-flymake)

   ("C-x C-k C-k" . consult-kmacro)
   ("M-y"         . consult-yank-pop)
   ("M-g g"       . consult-goto-line)
   ("M-g M-g"     . consult-goto-line)
   ("M-g f"       . consult-flymake)
   ("M-g i"       . consult-imenu)
   ("M-s o"       . consult-line)
   ;; ("M-s L"       . consult-line-multi)
   ("M-s u"       . consult-focus-lines)
   ("M-s g"       . consult-grep)
   ("M-s M-g"     . consult-grep)
   ("C-x C-SPC"   . consult-global-mark)
   ("C-x M-:"     . consult-complex-command)
   ;; ("C-c n"       . consult-org-agenda)
   :map comint-mode-map
   ("C-c h" . consult-history)
   :map dired-mode-map
   ("O" . consult-file-externally)
   :map help-map
   ("a" . consult-apropos)
   :map minibuffer-local-map
   ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package consult-eglot
  :after consult eglot
  ;; :bind (:map eglot-mode-map
  ;; 	 ("C-M-." . consult-eglot-symbols))
  ;; FIXME bind isn't binding
  :config
  (define-key eglot-mode-map [remap xref-find-apropos] 'consult-eglot-symbols))

(use-package marginalia
  :after vertico
  :ensure t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode))

