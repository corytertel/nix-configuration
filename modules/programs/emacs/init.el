;;; init.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;;; Code:

;;
;; --- GARBAGE COLLECTION ---
;;

;; Taken from https://gitlab.com/rycee/nur-expressions/blob/master/hm-modules/emacs-init.nix
(defun hm/reduce-gc ()
  "Reduce the frequency of garbage collection."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun hm/restore-gc ()
  "Restore the frequency of garbage collection."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

;; Make GC more rare during init, while minibuffer is active, and
;; when shutting down. In the latter two cases we try doing the
;; reduction early in the hook.
(hm/reduce-gc)
(add-hook 'minibuffer-setup-hook #'hm/reduce-gc -50)
(add-hook 'kill-emacs-hook #'hm/reduce-gc -50)

;; But make it more regular after startup and after closing minibuffer.
(add-hook 'emacs-startup-hook #'hm/restore-gc)
(add-hook 'minibuffer-exit-hook #'hm/restore-gc)

;; Avoid unnecessary regexp matching while loading .el files.
(defvar hm/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun hm/restore-file-name-handler-alist ()
  "Restore the \'file-name-handler-alist\' variable."
  (setq file-name-handler-alist hm/file-name-handler-alist)
  (makunbound 'hm/file-name-handler-alist))
(add-hook 'emacs-startup-hook #'hm/restore-file-name-handler-alist)

;;
;; -- PACKAGE SETUP ---
;;

;; For Nix
(require 'package)

;; optional. makes impure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; For non Nix Setups
;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;; 			 ("org" . "https://orgmode.org/elpa/")
;; 			 ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

;;
;; --- VISUALS ---
;;

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips
(set-fringe-mode 10) ; Gives some breathing room

;; Setting the font
(set-face-attribute 'default nil :family "VictorMono Nerd Font Mono")

;; Don't unload fonts when not in use
(setq inhibit-compacting-font-caches t)

;; Theme
(setq custom-safe-themes t) ; Treat all themes as safe
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(add-hook 'emacs-startup-hook (lambda () (load-theme 'plain-light t)))

;; Display Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		vterm-mode-hook
		cider-repl-mode-hook
		racket-repl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Window dividers
(setq window-divider-default-right-width 3)
(let ((color (face-background 'mode-line)))
  (dolist (face '(window-divider-first-pixel
		  window-divider-last-pixel
		  window-divider))
    (set-face-foreground face color)))
(window-divider-mode 1)

;; Icons
(use-package all-the-icons
  :ensure t)

;; Modeline
(use-package smart-mode-line
  :config
  (setq sml/theme 'cory)
  (sml/setup))

(use-package rich-minority
  :config
  (rich-minority-mode 1)
  (setf rm-blacklist ""))

;; Add padding to the sides
(require 'frame)
(setq-default default-frame-alist
	      (append (list
		       '(internal-border-width . 20)
		       ;; '(left-fringe . 0)
		       ;; '(right-fringe . 0)
		       '(tool-bar-lines . 0)
		       '(menu-bar-lines . 0)
		       '(line-spacing . 0.075)
		       '(vertical-scroll-bars . nil))))
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)
(add-hook 'before-make-frame-hook 'window-divider-mode)

;; Make the cursor a bar
(setq-default cursor-type 'bar)

;; Beacon
(use-package beacon
  :config
  (beacon-mode 1))

;; Visual feedback on yank/kill
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil))

;;
;; --- WINDOW MANAGEMENT
;;

(setq focus-follows-mouse t
      mouse-autoselect-window t)

(defun split-and-follow-below ()
  "Open a new window vertically."
  (interactive)
  (split-window-below)
  (other-window 1)
  (consult-buffer))

(defun split-and-follow-right ()
  "Open a new window horizontally."
  (interactive)
  (split-window-right)
  (other-window 1)
  (consult-buffer))

(defun kill-all-buffers-and-windows ()
  "Kill all buffers and windows."
  (interactive)
  (when (yes-or-no-p "Really kill all buffers and windows? ")
    (save-some-buffers)
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)))

(defun previous-window ()
  "Reverse direction of `other-window'."
  (other-window -1))

(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x S-o") 'previous-window)
(global-set-key (kbd "C-x M-o") 'hydra-window-resize/body)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-and-follow-below)
(global-set-key (kbd "C-x 3") 'split-and-follow-right)
(global-set-key (kbd "C-x 4 q") 'kill-all-buffers-and-windows)
(global-set-key (kbd "C-c b") 'balance-windows)

;;
;; --- IDE Features (LSP, Completion, etc) ---
;;

(setq visible-bell nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default fill-column 80
	      left-margin-width 1
	      sentence-end-double-space nil
	      lisp-backquote-indentation nil
	      blink-cursor-blinks 1
	      fast-but-imprecise-scrolling t
	      auto-save-interval 60
	      kill-do-not-save-duplicates t
	      bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t)

(global-so-long-mode 1)
(save-place-mode 1)

;; don't back up files
(setq make-backup-files nil)

(use-package hippie-exp
  :ensure nil
  :bind
  ;; ([remap dabbrev-expand] . hippie-expand)
  (("\M- " . hippie-expand))
  :commands (hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line)))

;; (use-package popper
;;   :ensure t
;;   :config
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           "\\*Async Shell Command\\*"
;; 	  "\\*eldoc\\*"
;; 	  "\\*Ibuffer\\*"
;; 	  "\\*vc-git"
;; 	  "\\*Help\\*"
;; 	  "\\*RE-Builder\\*$"
;; 	  flymake-diagnostics-buffer-mode
;; 	  calendar-mode
;; 	  help-mode
;; 	  compilation-mode
;; 	  eshell-mode
;; 	  vterm-mode))
;;   (popper-mode)
;;   (popper-echo-mode)
;;   :bind* ("C-\\" . popper-toggle-type)
;;   ("C-+" . popper-toggle-latest))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer nil))

;; LSP
(use-package eglot
  :after flycheck company
  :defer t
  :ensure t

  :hook
  (nix-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (racket-mode . eglot-ensure)
  (clojure-mode . eglot-ensure)
  (clojurescript-mode . eglot-ensure)
  (clojurec-mode . eglot-ensure)
  (java-mode . eglot-ensure)

  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p 2)

  :config
  (define-key eglot-mode-map [remap display-local-help] nil)

  :bind (:map eglot-mode-map
	 ("C-c C-a" . eglot-code-actions)
	 ("C-c C-f" . eglot-format-buffer)))

;; Completion
(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
   ([return] . nil)
   ("RET" . nil)
   ("TAB" . company-complete-selection)
   ([tab] . company-complete-selection)
   ("C-f" . company-complete-selection)
   ("S-TAB" . company-select-previous)
   ([backtab] . company-select-previous)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ;; :map eglot-mode-map
   ;; ("<tab>" . company-indent-or-complete-common)
   )
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-maximum-width 100
        company-tooltip-minimum-width 20
	;; Allow me to keep typing even if company disapproves.
        company-require-match nil)
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;; Autocompletion for shell
(use-package company-shell
  :hook ((sh-mode shell-mode) . sh-mode-init)
  :config
  (defun sh-mode-init ()
    (setq-local company-backends
		'((company-shell
                   company-shell-env
                   company-files
                   company-dabbrev-code
                   company-capf
                   company-yasnippet)))))

;; (use-package company-emoji
;;   :config (add-to-list 'company-backends 'company-emoji))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package company-nixos-options
  :config
  (add-to-list 'company-backends 'company-nixos-options))

;; Minibuffer completion
(use-package vertico
  :init
  ;; (use-package orderless
  ;;   :commands (orderless)
  ;;   :custom (completion-styles '(orderless flex)))

  (use-package consult
    :init
    (setq consult-preview-key nil)
    :bind
    ("C-c f" . consult-recent-file)
    ("C-c s" . consult-ripgrep)
    ("C-c l" . consult-line)
    ("C-c i" . consult-imenu)
    ("C-c t" . gtags-find-tag)
    ("C-x b" . consult-buffer)
    ("C-c x" . consult-complex-command)
    (:map comint-mode-map
     ("C-c C-l" . consult-history)))
  :config
  (recentf-mode t)
  (vertico-mode t))

(use-package consult-company
  :bind (:map company-mode-map
	 ([completion-at-point] . consult-company)))

(use-package consult-eglot
  :bind ("C-c C-s" . consult-eglot-symbols))

(use-package consult-flycheck
  :bind ("C-c e" . consult-flycheck))

(use-package marginalia
  :after vertico
  :ensure t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode))

;; Search
(define-key isearch-mode-map (kbd "TAB") 'isearch-toggle-symbol)
(define-key isearch-mode-map (kbd "M-q") 'isearch-query-replace)

(use-package repeat
  :ensure nil
  :bind (:map isearch-mode-map
	 ("<down>" . #'isearch-repeat-forward)
	 ("<up>" . #'isearch-repeat-backward)))

;; Undo
(use-package undo-tree
  :defer 1
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode))

;; Visual Keybinding Info
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.00000001))

(use-package helpful
  :ensure
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Hydras
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("n" text-scale-increase "in")
  ("p" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(defhydra hydra-window-resize (:timeout 4)
  "resize window"
  ("p" shrink-window 5 "shrink vertically")
  ("n" enlarge-window 5 "enlarge vertically")
  ("b" shrink-window-horizontally 5 "shrink horizontally")
  ("f" enlarge-window-horizontally 5 "enlarge horizontally")
  ("q" nil "finished" :exit t))

;; Project Management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Git Management
(use-package magit
  :bind (("C-c g s" . magit-status))
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

;; Ligatures and Indicators
(use-package pretty-mode
  :config
  (add-hook 'prog-mode-hook 'pretty-mode))

;; Automatically remove trailing whitespace if user put it there
(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :config (setq ws-butler-keep-whitespace-before-point nil))

;; Indenting
(use-package aggressive-indent
  :config
  (electric-indent-mode 0)
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; Word wrapping
(global-visual-line-mode 1)
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Turn ^L into pretty lines
(use-package page-break-lines
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show empty whitespace
(global-whitespace-mode)
;; (setq whitespace-style '(face trailing tabs lines empty big-indent))
(setq whitespace-style '(face trailing tabs lines empty))

;; Use hex mode for binary files
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.dat\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.exe\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.o\\'" . hexl-mode))

;; Highlight and navigate TODO keywords
(use-package hl-todo
  :config (global-hl-todo-mode))

;; Visual feedback on some operations like yank, kill, undo
(use-package goggles
  :config (goggles-mode))

;; DOESN'T EXIST
;; Show the name of the current function definition in the modeline
;; (use-package which-func
;;  :config (which-function-mode 1))

;; Smartparens
(use-package smartparens
  :defer 1
  :hook ((
          emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode
	  racket-mode racket-repl-mode hy-mode prolog-mode go-mode cc-mode
	  python-mode typescript-mode json-mode javascript-mode ;java-mode
          ) . smartparens-strict-mode)
  ;; :hook (prog-mode . smartparens-strict-mode)
  :bind (:map smartparens-mode-map
         ;; This is the paredit mode map minus a few key bindings
         ;; that I use in other modes (e.g. M-?)
         ("C-M-f" . sp-forward-sexp) ;; navigation
         ("C-M-b" . sp-backward-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-n" . sp-up-sexp)
         ;; ("C-w" . whole-line-or-region-sp-kill-region)
         ("M-s" . sp-splice-sexp) ;; depth-changing commands
         ("M-r" . sp-splice-sexp-killing-around)
         ("M-(" . sp-wrap-round)
         ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("C-(" . sp-backward-slurp-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)
         ("M-S" . sp-split-sexp) ;; misc
         ("M-j" . sp-join-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)

  ;; Don't insert annoying colon after Python def
  (setq sp-python-insert-colon-in-function-definitions nil)

  ;; Always highlight matching parens
  (show-smartparens-global-mode +1)

  ;; Blink matching parens
  (setq blink-matching-paren t)

  (defun whole-line-or-region-sp-kill-region (prefix)
    "Call `sp-kill-region' on region or PREFIX whole lines."
    (interactive "*p")
    (whole-line-or-region-wrap-beg-end 'sp-kill-region prefix))

  ;; Create keybindings to wrap symbol/region in pairs
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
       (interactive "P")
       (sp-wrap-with-pair ,s)))
  (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
  (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
  (define-key prog-mode-map (kbd "M-{") (prelude-wrap-with "{"))
  (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))
  (define-key prog-mode-map (kbd "M-'") (prelude-wrap-with "'"))
  (define-key prog-mode-map (kbd "M-`") (prelude-wrap-with "`"))

  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "[" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "(" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))

  ;; Don't include semicolon ; when slurping
  (add-to-list 'sp-sexp-suffix '(java-mode regexp ""))

  ;; use smartparens-mode everywhere
  (smartparens-global-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-all-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
	 ("C-M-<" . mc/mark-all-in-region-regexp)
	 ("C-M->" . mc/edit-lines)
         :map mc/keymap
         ("C-x v" . mc/vertical-align-with-space)
         ("C-x n" . mc-hide-unmatched-lines-mode))
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

  (with-eval-after-load 'multiple-cursors-core
    ;; Immediately load mc list, otherwise it will show as
    ;; changed as empty in my git repo
    (mc/load-lists)

    (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
    (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
    (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this)))

;; Move text
(use-package move-text
  :bind (([(control shift up)]   . move-text-up)
         ([(control shift down)] . move-text-down)
         ([(meta shift up)]      . move-text-up)
         ([(meta shift down)]    . move-text-down)
	 ("C-S-n" . move-text-down)
	 ("C-S-p" . move-text-up)))

;; Copy text as Discord/GitHub/etc formatted code
(use-package copy-as-format
  :bind
  (("C-c c c" . copy-as-format)
   ("C-c c g" . copy-as-format-github)
   ("C-c c t" . copy-as-format-markdown-table)
   ("C-c c m" . copy-as-format-markdown)
   ("C-c c o" . copy-as-format-org-mode)
   ("C-c c d" . copy-as-format-slack)
   ("C-c c v" . org-copy-visible))
  :config
  (setq copy-as-format-default "slack")
  (defun copy-as-format--markdown-table (text _multiline)
    (s-replace "--+--" "--|--" text))
  (add-to-list 'copy-as-format-format-alist '("markdown-table" copy-as-format--markdown-table)))

;; Tramp
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")

  ;; Only for debugging slow tramp connections
  ;;(setq tramp-verbose 7)

  ;; Skip version control for tramp files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; Use ControlPath from .ssh/config
  (setq tramp-ssh-controlmaster-options "")

  ;; Backup tramp files like local files and don't litter the remote
  ;; file system with my emacs backup files
  (setq tramp-backup-directory-alist backup-directory-alist)

  ;; See https://www.gnu.org/software/tramp/#Ad_002dhoc-multi_002dhops
  ;; For all hosts, except my local one, first connect via ssh, and then apply sudo -u root:
  (dolist (tramp-proxies '((nil "\\`root\\'" "/ssh:%h:")
                           ((regexp-quote (system-name)) nil nil)
                           ("localhost" nil nil)
                           ("blif\\.vpn" nil nil)
                           ("skor-pi" nil nil)
                           ;; Add tramp proxy for atomx user
                           (nil "atomx" "/ssh:%h:")))
    (add-to-list 'tramp-default-proxies-alist tramp-proxies)))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer 1
  :bind (("M-n" . flycheck-next-error)
	 ("M-p" . flycheck-previous-error))
  :init
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00111100
              #b01111110
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b11111111
              #b01111110
              #b00111100))

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
      :fringe-face 'flycheck-fringe-info))
  :config
  ;; Only check buffer when mode is enabled or buffer is saved.
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; Enable flycheck in all eligible buffers.
  (global-flycheck-mode))

;;
;; --- MODE CONFIGURATION ---
;;

;;; Lisps

;; Nicer elisp regex syntax highlighting
(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode) . easy-escape-minor-mode))

;; From: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
- `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
- an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
- a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

;;; Elisp

;; subr-x
(put 'if-let   'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)

;; emacs-lisp-mode
(defvar eval-print-as-comment-prefix ";;=> ")

(defun eval-print-as-comment (&optional arg)
  (interactive "P")
  (let ((start (point)))
    (eval-print-last-sexp arg)
    (save-excursion
      (goto-char start)
      (save-match-data
        (re-search-forward "[[:space:]\n]*" nil t)
        (insert eval-print-as-comment-prefix)))))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-;") 'eval-print-as-comment)

;;; Clojure
(use-package clojure-mode
  :defer t
  :interpreter ("bb" . clojure-mode)
  :config
  ;; Eval top level forms inside comment forms instead of the comment form itself
  (setq clojure-toplevel-inside-comment-form t)
  ;; Indent fn-traced and defn-traced the same as a regular defn.
  ;; The macros are for re-frame-10x tracing.
  (put-clojure-indent 'fn-traced :defn)
  (put-clojure-indent 'defn-traced :defn))

(use-package cider
  :bind (:map cider-mode-map
         ("M-?" . cider-maybe-clojuredocs)
         :map cider-repl-mode-map
         ("M-?" . cider-doc))
  :hook (((cider-mode cider-repl-mode) . cider-company-enable-fuzzy-completion)
         (cider-mode . eldoc-mode))
  :config
  (defun cider-maybe-clojuredocs (&optional arg)
    "Like `cider-doc' but call `cider-clojuredocs' when invoked with prefix arg in `clojure-mode'."
    (interactive "P")
    (if (and arg (eq major-mode 'clojure-mode))
        (cider-clojuredocs arg)
      (cider-doc)))

  ;; Location of the jdk sources. In Arch Linux package `openjdk-src'
  (setq cider-jdk-src-paths "/usr/lib/jvm/java-11-openjdk/lib/src.zip")

  (require 's)

  ;; Inject reveal middleware in cider-jack-in when the `:reveal' alias is set
  (defun cider-cli-global-options-contains-reveal? (&rest _)
    (and cider-clojure-cli-global-options
       (s-contains? ":reveal" cider-clojure-cli-global-options)))
  (add-to-list 'cider-jack-in-nrepl-middlewares
               '("vlaaad.reveal.nrepl/middleware" :predicate cider-cli-global-options-contains-reveal?))

  ;; Inject shadowcljs nrepl middleware in cider-jack-in when the `:cljs' alias is set
  (defun cider-cli-global-options-contains-cljs? (&rest _)
    (and cider-clojure-cli-global-options
       (s-contains? ":cljs" cider-clojure-cli-global-options)))
  (add-to-list 'cider-jack-in-nrepl-middlewares
               '("shadow.cljs.devtools.server.nrepl/middleware" :predicate cider-cli-global-options-contains-cljs?))


  ;; jack-in for babashka
  (defun cider-jack-in-babashka ()
    "Start an babashka nREPL server for the current project and connect to it."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (process-filter (lambda (proc string)
                             "Run cider-connect once babashka nrepl server is ready."
                             (when (string-match "Started nREPL server at .+:\\([0-9]+\\)" string)
                               (cider-connect-clj (list :host "localhost"
                                                        :port (match-string 1 string)
                                                        :project-dir default-directory)))
                             ;; Default behavior: write to process buffer
                             (internal-default-process-filter proc string))))
      (set-process-filter
       (start-file-process "babashka" "*babashka*" "bb" "--nrepl-server" "0")
       process-filter)))

  ;; Store more items in repl history (default 500)
  (setq cider-repl-history-size 2000)
  ;; When loading the buffer (C-c C-k) save first without asking
  (setq cider-save-file-on-load t)
  ;; Don't show cider help text in repl after jack-in
  (setq cider-repl-display-help-banner nil)
  ;; Don't focus repl after sending somehint to there from another buffer
  (setq cider-switch-to-repl-on-insert nil)
  ;; Eval automatically when insreting in the repl (e..g. C-c C-j d/e) (unless called with prefix)
  (setq cider-invert-insert-eval-p t)
  ;; Don't focus error buffer when error is thrown
  (setq cider-auto-select-error-buffer nil)
  ;; Don't focus inspector after evaluating something
  (setq cider-inspector-auto-select-buffer nil)
  ;; Display context dependent info in the eldoc where possible.
  (setq cider-eldoc-display-context-dependent-info t)
  ;; Don't pop to the REPL buffer on connect
  ;; Create and display the buffer, but don't focus it.
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  ;; Just use symbol under point and don't prompt for symbol in e.g. cider-doc.
  (setq cider-prompt-for-symbol nil))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (dolist (magit-require '(("csv" . "clojure.data.csv")
                           ("edn" . "clojure.edn")
                           ;; ("http" . "clj-http.client")
                           ("reagent" . "reagent.core")
                           ("re-frame" . "re-frame.core")))
    (add-to-list 'cljr-magic-require-namespaces magit-require)))

(use-package flycheck-clj-kondo
  :after (flycheck clojure-mode))

;; (use-package ob-clojure
;;   :after ob
;;   :config
;;   (setq org-babel-clojure-backend 'cider))

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
(add-hook 'clojure-mode-hook
	  '(cory/leader-keys
	    ","  '(:ignore t :which-key "clojure")
	    ",c" '(cider-jack-in-clj :which-key "cider jack in")
	    ",k" '(cider-load-buffer :which-key "load buffer")))

(setq read-process-output-max (* 1024 1024))

;;; C++
(use-package yasnippet)
(yas-global-mode 1)

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
           (if  (equal (file-name-extension file) "cpp") "clang++" "clang" )
           (file-name-sans-extension file)
           file)))
    (compile compile-command)))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

(use-package irony
  :hook (((c++-mode c-mode objc-mode) . irony-mode-on-maybe)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (defun irony-mode-on-maybe ()
    ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: solidity-mode
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1))))

(use-package company-irony
  :after irony
  :config (add-to-list 'company-backends 'company-irony))

(use-package irony-eldoc
  :hook (irony-mode))

;;(global-set-key [f9] 'code-compile)

;;; Racket
(use-package racket-mode
  :bind
  (:map racket-mode-map
   ("C-c C-r" . racket-run)
   :map racket-repl-mode-map
   ("C-c C-r" . racket-run))
  :config
  (defun setup-racket-eldoc ()
    (eldoc-mode +1)
    (setq eldoc-documentation-function #'racket-xp-eldoc-function))

  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook      #'setup-racket-eldoc)
  (add-hook 'racket-mode-hook      #'racket-xp-mode))

;; (use-package flymake-racket)
(use-package dr-racket-like-unicode)
;; (use-package bracketed-paste)

;; (use-package geiser)
;; (use-package geiser-racket)

;;; Java

;; For groovy and gradle support
(use-package groovy-mode :defer t)

;; Viewing Java Class files
(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'javap-handler
               (and (eq inhibit-file-name-operation operation)
                  inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (java-mode)
        (current-buffer))))
   ((javap-handler-real op args))))

(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

;;; Latex
;; (use-package latex-preview-pane)

;;; Other Modes
(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook
  (nix-mode . subword-mode)
  (nix-mode . nix-prettify-mode))

(use-package fvwm-mode)

;;
;; --- TERMINALS ---
;;

;;; Terminal

;; (use-package term
;;   :config
;;   (setq explicit-shell-file-name "zsh")
;;   ;;(setq explicit-zsh-args '())
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; BROKEN
;; (use-package eterm-256color
;;   :hook (term-mode . eterm-256color-mode))

;; Use local Emacs instance as $EDITOR (e.g. in `git commit' or `crontab -e')
(use-package with-editor
  :hook ((shell-mode eshell-mode vterm-mode term-exec) . with-editor-export-editor))

;;; Eshell

;; Don't print the welcome banner and
;; use native 'sudo', system sudo asks for password every time.
(require 'em-tramp)
(setq eshell-modules-list
      '(eshell-alias
        eshell-basic
        eshell-cmpl
        eshell-dirs
        eshell-glob
        eshell-hist
        eshell-ls
        eshell-pred
        eshell-prompt
        eshell-script
        eshell-term
        eshell-tramp
        eshell-unix))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(defun cory/configure-eshell ()
  "Eshell configuration that will run the first time eshell launches."
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(add-hook 'eshell-first-time-mode-hook 'cory/configure-eshell)

;; Use vterm for visual commands
(use-package eshell-vterm
  :load-path "site-lisp/eshell-vterm"
  :demand t
  :after eshell
  :config
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual))

;; Eshell's zoxide
(use-package eshell-z
  ;; :hook (eshell-mode . eshell-z)
  ;; :config
  ;; (defalias 'eshell/cd 'eshell-z)
  )

;; One prompt at all times
(use-package eshell-fixed-prompt
  :hook (eshell-mode . eshell-fixed-prompt-mode))

;; Syntax highlighting
(use-package eshell-syntax-highlighting
  :ensure t
  :config
  ;; Enable in all future ehell buffers
  (eshell-syntax-highlighting-global-mode +1))

;; Eshell auto-complete
;; `company-mode' backend to provide eshell history suggestion
(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

;; Eshell toggling
;; (use-package eshell-toggle
;;   :bind
;;   (("C-`" . eshell-toggle)
;;    ("C-c C-t" . eshell-toggle))
;;   :config
;;   (setq eshell-toggle-size-fraction 2
;; 	eshell-toggle-window-side 'below
;; 	eshell-toggle-use-projectile-root nil
;; 	eshell-toggle-run-command nil))

;; Eshell up
(use-package eshell-up
  :config
  (defalias 'eshell/up 'eshell-up)
  (defalias 'eshell/pk 'eshell-up-peek))

;; Eshell help
(use-package esh-help
  :ensure t
  :defer t
  :config
  (setup-esh-help-eldoc))

;; Show last status in fringe
;; (use-package eshell-fringe-status
;;   :hook (eshell-mode . eshell-fringe-status-mode)
;;   :config
;;   (define-fringe-bitmap 'efs-line-bitmap
;;     (vector #b00111100
;;             #b01111110
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b11111111
;;             #b01111110
;;             #b00111100))
;;   (setq eshell-fringe-status-success-bitmap 'efs-line-bitmap)
;;   (setq eshell-fringe-status-failure-bitmap 'efs-line-bitmap))

;; Info (from Emacs wiki)
(defun eshell/info (subject)
  "Read the Info manual on SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (let ((node-exists (ignore-errors (Info-menu subject))))
      (if node-exists
          0
        ;; We want to switch back to *eshell* if the requested
        ;; Info manual doesn't exist.
        (switch-to-buffer buf)
        (eshell-print (format "There is no Info manual on %s.\n"
                              subject))
        1))))

;; Less/More (from Emacs wiki)
(defun eshell-view-file (file)
  "A version of `view-file' which properly respects the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        (buffer (find-file-noselect file)))
    (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window
	     (list (window-buffer) (window-start)
                   (+ (window-point)
                      (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file.  \"less +42 foo\" will go to line 42 in the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell-view-file file)
          (goto-line line))
      (eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

;; Delete backup files (from Emacs wiki)
(defun eshell/rmb ()
  "Delete files matching pattern \".*~\" and \"*~\"."
  (eshell/rm (directory-files "." nil "^\\.?.*~$" nil)))

;; Running programs in a term-mode buffer
;; (with-eval-after-load 'esh-opt
;;   (setq eshell-destroy-buffer-when-process-dies t)
;;   (setq eshell-visual-commands '("htop" "zsh" "vim")))

;; Vterm
(use-package vterm
  :ensure t
  ;; :bind ("C-c C-t" . vterm-other-window)
  :bind (:map vterm-mode-map
	 ("C-c C-t" . nil))
  :commands (vterm))

;; (use-package multi-vterm
;;   :ensure t
;;   :bind
;;   ("C-c C-t" . multi-vterm-dedicated-toggle))

;;
;; --- GENERAL KEYBINDS ---
;;

;; Basic Keybind
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-M-s") 'project-search)

;;
;; --- MISC ---
;;

;; Use UTF-8 Encoding
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Always confirm closing Emacs
(setq confirm-kill-emacs #'yes-or-no-p)

;; Replace "yes or no" prompts with "y or n" prompts
(defalias 'yes-or-no-p #'y-or-n-p
  "Use `y-or-n-p' instead of a yes/no prompt.")

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; init.el ends here
