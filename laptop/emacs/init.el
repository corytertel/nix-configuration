
;; Disables the startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips
(set-fringe-mode 10) ; Gives some breathing room

(setq visible-bell nil) ; Disables the visible bell

;; Sets the font. Height is a percentage
(set-face-attribute 'default nil
		    :family "JetBrains Mono" :weight 'light :height 100)
(set-face-attribute 'bold nil
		    :family "JetBrains Mono" :weight 'regular)
(set-face-attribute 'italic nil
		    :family "Victor Mono" :weight 'semilight :slant 'italic :height 100)
(set-fontset-font t 'unicode
		  (font-spec :name "JetBrainsMono NF" :size 16) nil)
(set-fontset-font t '(#xe000 . #xffdd)
		  (font-spec :name "JetBrainsMono NF" :size 12) nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		cider-repl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Auto pairs
(electric-pair-mode)

;; Scroll conservatively
;;(setq scroll-conservatively 101)

;; Show empty whitespace
(global-whitespace-mode)
(setq whitespace-style '(face trailing tabs lines empty big-indent))

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package swiper)

;; Basic Keybind
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Define a binding just for a certain mode
;;(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

;; Declare keybinds in a more consise way
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer cory/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (cory/leader-keys
    ;; Search (M-x alternative)
    "SPC" '(counsel-M-x :which-key "M-x")

    ;; Buffers
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(counsel-switch-buffer :which-key "switch buffer")
    "bd" '(kill-buffer :which-key "kill-buffer")
    "be" '(eval-buffer :which-key "eval buffer")
    ;;"bn" '(centaur-tabs-forward :which-key "next buffer")
    ;;"bp" '(centaur-tabs-backward :which-key "previous buffer")
    "bg" '(centaur-tabs-counsel-switch-group :which-key "switch group")

    ;; Files
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find-file")

    ;; Git
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")

    ;; Language
    "l"  '(:ignore t :which-key "language")

    ;; Operations
    "o"  '(:ignore t :which-key "operations")
    "or" '(replace-string :which-key "replace string")
    "oc" '(comment-or-uncomment-region :which-key "comment region")

    ;; Projects
    "p" '(projectile-command-map :which-key "projects")

    ;; Toggles
    "t"  '(:ignore t :which-key "toggles")
    "ts" '(hydra-text-scale/body :which-key ":scale-text")
    "tt" '(counsel-load-theme :which-key "choose theme")

    ;; Windows
    "w"  '(:ignore t :which-key "windows")
    "wd" '(evil-window-delete :which-key "delete window")
    ))

(defun cory/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  special-mode
		  term-mode
		  cider-repl-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  :hook (evil-mode . cory/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))
(when (require 'evil-collection nil t)
  (evil-collection-init))

(evil-mode)

(use-package undo-tree)
(global-undo-tree-mode)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 40)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.0))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package helpful
  :ensure
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; (use-package magit
;;   :commands (magit-status magit-get-current-branch)
;;   :custom
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 48)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-plain-icons nil)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-adjust-buffer-order t)
  (setq centaur-tabs-label-fixed-length 10) ; 0 is dynamic
  (setq centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "JetBrains Mono" 100)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-mode t)
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :bind
  ;; Vim-like tab changing
  (:map evil-normal-state-map
	("g t" . centaur-tabs-forward)
	("g T" . centaur-tabs-backward))
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))


;;; Clojure
(use-package clojure-mode)
(use-package cider)
(use-package clojure-mode-extra-font-locking)
(use-package paredit)

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))
            (rainbow-delimiters-mode)))

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; Define keybindings just for clojure-mode
;; (define-key clojure-mode-map (kbd "SPC l c") 'cider-jack-in)

;; Clojure-mode specific keybindings
;; (add-hook 'clojure-mode-hook
;; 	  '(cory/leader-keys
;; 	     "lc" '(cider-jack-in-clj :which-key "cider jack in")
;; 	     "lk" '(cider-load-buffer :which-key "load buffer")))

(use-package lsp-mode)
(use-package lsp-treemacs)
(use-package company)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ;; lsp-enable-indentation nil ; uncomment to use cider identation instead of lsp
      ;; lsp-enable-completion-at-point-nil ; uncomment to use cider completion instead of lsp
      )

;;; C++
(use-package yasnippet)
(yas-global-mode 1)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(use-package modern-cpp-font-lock
  :ensure t)
(modern-c++-font-lock-global-mode t)

(use-package cpp-auto-include)

(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
     (let ((file (file-name-nondirectory buffer-file-name)))
       (format "%s -o %s %s"
           (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
           (file-name-sans-extension file)
           file)))
    (compile compile-command)))

;;(global-set-key [f9] 'code-compile)

;;; Other Modes
(use-package haskell-mode)
(use-package nix-mode)

;; Theme
;; (use-package modus-themes
;;   :ensure
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;;         modus-themes-region '(bg-only no-extend))

;;   ;; Load the theme files before enabling a theme
;;   (modus-themes-load-themes)
;;   :config
;;   ;; Load the theme of your choice:
;;   (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
;;   :bind ("<f11>" . modus-themes-toggle))

(setq custom-safe-themes t) ; Treat all themes as safe
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'black-nord)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("42c0370f0d2e1c4776f372e91fc514977d0b0c14077954a1f229e6a630e08fe6" "c8cd8b9393a75a99556a2bb1b5dda053973b93a53ba7f6cf4fbad6b28ed1d039" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(evil-magit magit paredit clojure-mode-extra-font-locking counsel-projectile projectile yasnippet which-key use-package undo-tree rainbow-delimiters parchment-theme nix-mode modern-cpp-font-lock lsp-treemacs ivy-rich helpful haskell-mode general flycheck evil doom-modeline cpp-auto-include counsel company command-log-mode cider centaur-tabs auto-complete))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
