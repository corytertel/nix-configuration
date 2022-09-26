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
(set-face-attribute 'default nil :family "Victor Mono")
;; Set fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Victor Mono")
;; Set variable pitch face
(set-face-attribute 'variable-pitch nil :font "Oxygen-Sans")

;; Don't unload fonts when not in use
(setq inhibit-compacting-font-caches t)

;; Theme
(setq custom-safe-themes t) ; Treat all themes as safe
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
;; (add-hook 'emacs-startup-hook (lambda () (load-theme 'plain-light t)))
(add-hook 'emacs-startup-hook (lambda () (load-theme 'plain-dark t)))

;; Display Line numbers
;; (column-number-mode)
;; (global-display-line-numbers-mode t)

;; Disable line numbers for some modes
;; (dolist (mode '(org-mode-hook
;; 		term-mode-hook
;; 		shell-mode-hook
;; 		eshell-mode-hook
;; 		vterm-mode-hook
;; 		cider-repl-mode-hook
;; 		racket-repl-mode-hook
;; 		geiser-repl-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Window dividers
(setq window-divider-default-right-width 3)
(let ((color (face-background 'mode-line)))
  (dolist (face '(window-divider-first-pixel
		  window-divider-last-pixel
		  window-divider))
    (set-face-foreground face color)))
(window-divider-mode 1)

;; Icons
;; (use-package all-the-icons
;;   :ensure t)

;; Modeline
;; (use-package smart-mode-line
;;   :config
;;   (setq sml/theme 'cory)
;;   (sml/setup))

;; (use-package rich-minority
;;   :config
;;   (rich-minority-mode 1)
;;   (setf rm-blacklist ""))

(defface my-narrow-face
  '((t (:foreground "#141404" :background "#ed8f23")))
  "Todo/fixme highlighting."
  :group 'faces)

(defface my-read-only-face
  '((t (:foreground "#141404" :background "#1f8c35")))
  "Read-only buffer highlighting."
  :group 'faces)

(defface my-modified-face
  '((t (:foreground "#d8d8d8" :background "#e60909")))
  "Modified buffer highlighting."
  :group 'faces)

(setq-default
 mode-line-format
 '("  "
   (:eval (let ((str (if buffer-read-only
                         (if (buffer-modified-p) "%%*" "%%%%")
                       (if (buffer-modified-p) "**" "--"))))
            (if buffer-read-only
                (propertize str 'face 'my-read-only-face)
              (if (buffer-modified-p)
                  (propertize str 'face 'my-modified-face)
                str))))
   (list 'line-number-mode "  ")
   (:eval (when line-number-mode
            (let ((str "L%l"))
              (if (/= (buffer-size) (- (point-max) (point-min)))
                  (propertize str 'face 'my-narrow-face)
                str))))
   "  %p"
   (list 'column-number-mode "  C%c")
   "  " mode-line-buffer-identification
   "  " mode-line-modes))

(use-package moody
  :custom
  (moody-mode-line-height 40)
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :config (minions-mode))

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

;; Smooth pixel scrolling
(pixel-scroll-mode 1)

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

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x S-o") 'previous-window)
(global-set-key (kbd "C-x M-o") 'hydra-window-resize/body)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-and-follow-below)
(global-set-key (kbd "C-x 3") 'split-and-follow-right)
(global-set-key (kbd "C-x 4 q") 'kill-all-buffers-and-windows)
(global-set-key (kbd "C-c b") 'balance-windows)
(global-set-key (kbd "<f1>") 'switch-to-last-buffer)
(global-set-key (kbd "C-<f1>") 'switch-to-last-buffer)
(global-set-key (kbd "M-<f1>") 'switch-to-last-buffer)
(global-set-key (kbd "C-M-<f1>") 'switch-to-last-buffer)
(global-set-key (kbd "<f2>") 'other-window)
(global-set-key (kbd "C-<f2>") 'other-window)
(global-set-key (kbd "M-<f2>") 'other-window)
(global-set-key (kbd "C-M-<f2>") 'other-window)

(use-package yequake
  :custom
  (yequake-frames
   '(("eshell"
      (width . 0.5)
      (height . 0.5)
      (left . 0.5)
      (top . 0.5)
      (alpha . 0.7)
      (buffer-fns . ("*eshell*<1>"))
      (frame-parameters . ((undecorated . t))))
     ("org-capture"
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.7)
      (buffer-fns . (yequake-org-capture))
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)))))))

;;
;; --- IDE Features (LSP, Completion, etc) ---
;;

(setq visible-bell nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default left-margin-width 1
	      sentence-end-double-space nil
	      lisp-backquote-indentation nil
	      blink-cursor-blinks 10
	      fast-but-imprecise-scrolling t
	      auto-save-interval 60
	      kill-do-not-save-duplicates t
	      bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t)

;; (global-so-long-mode 1)
(save-place-mode 1)

;; don't back up files
(setq make-backup-files nil)

;; Minimap
;; (use-package minimap
;;   :custom
;;   (minimap-window-location ')
;;   :config
;;   (minimap-mode))

;; Expansion
(use-package hippie-exp
  :ensure nil
  :bind
  ([remap dabbrev-expand] . hippie-expand)
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

;; Popups
;; (use-package popper
;;   :ensure t
;;   :config
;;   (setq popper-reference-buffers
;;         '(;; "\\*Messages\\*"
;;           "Output\\*$"
;; 	  "\\*eldoc\\*"
;; 	  "\\*Help\\*"
;; 	  flymake-diagnostics-buffer-mode
;; 	  calendar-mode
;; 	  help-mode
;; 	  compilation-mode
;; 	  eshell-mode
;; 	  vterm-mode))
;;   (popper-mode)
;;   ;; (popper-echo-mode)
;;   :bind
;;   (("C-`" . popper-toggle-latest)
;;    ("C-~" . popper-toggle-type)
;;    ("M-`" . popper-cycle))
;;   :custom
;;   ;; (popper-group-function #'popper-group-by-project) ; project.el projects
;;   (popper-window-height (lambda (win)
;; 			  (fit-window-to-buffer
;; 			   win
;; 			   (frame-height)
;; 			   30))))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer nil))

;; LSP
(use-package eglot
  :after flymake corfu
  :defer t
  :ensure t

  :hook
  ;; (nix-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (racket-mode . eglot-ensure)
  (clojure-mode . eglot-ensure)
  (clojurescript-mode . eglot-ensure)
  (clojurec-mode . eglot-ensure)
  (scheme-mode . eglot-ensure)
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
  (add-to-list 'eglot-server-programs
               `(scheme-mode . ("chicken-lsp-server")))

  :bind (:map eglot-mode-map
	 ("C-c C-a" . eglot-code-actions)
	 ("C-c f" . eglot-format-buffer)))

;; Completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-prefix 1)            ; Enable auto completion
  (corfu-auto-delay 0.0)           ; Enable auto completion
  (corfu-quit-at-boundary t)
  (corfu-echo-documentation t)     ; Enable auto documentation in the minibuffer
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)

  :init
  ;; Need to recreate the map in order to preserve movement keys
  ;; Don't touch my movement keys!!
  ;; TAB cycles through completion options
  (setq corfu-map
	(let ((map (make-sparse-keymap)))
	  (define-key map [remap completion-at-point] #'corfu-complete)
	  (define-key map [remap keyboard-escape-quit] #'corfu-quit)
	  (define-key map (kbd "C-g") #'corfu-quit)
	  (define-key map [down] #'corfu-next)
	  (define-key map [up] #'corfu-previous)
	  (define-key map [tab] #'corfu-next)
	  (define-key map [backtab] #'corfu-previous)
	  (define-key map (kbd "TAB") #'corfu-next)
	  (define-key map (kbd "S-TAB") #'corfu-previous)
	  ;; (define-key map [return] #'corfu-insert)
	  ;; (define-key map (kbd "RET") #'corfu-insert)
	  (define-key map (kbd "M-l") 'corfu-info-location)
	  (define-key map (kbd "C-h") 'corfu-info-documentation)
	  (define-key map (kbd "M-SPC") #'corfu-insert-separator)
	  map))
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto t)
              (corfu-mode))))

;; Templates takes advantage of emacs's tempo
;; (use-package tempel
;;   :ensure t
;;   :defer 10
;;   :hook ((prog-mode text-mode) . tempel-setup-capf)
;;   :bind (("M-+" . tempel-insert) ;; Alternative tempel-expand
;;          :map tempel-map
;;          ([remap keyboard-escape-quit] . tempel-done)
;;          :map corfu-map
;;          ("C-M-i" . tempel-expand))
;;   :init
;;   ;; Setup completion at point
;;   (defun tempel-setup-capf ()
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-expand
;; 			completion-at-point-functions))))

;; Documentation popup
(use-package corfu-doc
  :ensure t
  :bind (:map corfu-map
	 ([remap corfu-info-documentation] . corfu-doc-toggle)
	 ("M-p" . corfu-doc-scroll-down)
	 ("M-n" . corfu-doc-scroll-up))
  :custom
  (corfu-doc-auto nil) ; Disable auto-documentation
  (corfu-doc-max-width 60)
  (corfu-doc-max-height 30)
  :hook
  (corfu-mode . corfu-doc-mode))

;; Icons for corfu
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t) ; Use icons labels
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Minibuffer completion
(use-package vertico
  :config
  (recentf-mode t)
  (vertico-mode t))

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

;; Completion matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator "[ \\]"))

;; Minibuffer visual menu
(use-package consult
  :init
  (setq consult-preview-key nil)
  :bind
  ("C-c r" . consult-recent-file)
  ("C-x p s" . consult-ripgrep) ; for use with project.el
  ("C-s" . consult-line)
  ("C-c i" . consult-imenu)
  ;; ("C-c t" . gtags-find-tag)
  ("C-x b" . consult-buffer)
  ("C-c x" . consult-complex-command)
  ("C-c e" . consult-flymake)
  (:map comint-mode-map
   ("C-c h" . consult-history)))

(use-package consult-eglot
  :bind (;; ("C-c v" . xref-find-references-and-replace)
	 ;; :map eglot-mode-map
	 ([remap xref-find-apropos] . consult-eglot-symbols) ; "C-M-."
	 ;; ([remap xref-find-references-and-replace] . eglot-rename)
	 ("C-c v" . eglot-rename)))

(use-package marginalia
  :after vertico
  :ensure t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-=" . embark-act)         ;; pick some comfortable binding
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d" . dragon-drop)
   ("U"   . 0x0-upload-file)
   :map embark-region-map
   ("U"   . 0x0-dwim))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (defun search-in-source-graph (text))
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drag-and-drop " file))))

;; Templates
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

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
  :init
  (which-key-mode)
  (which-key-enable-god-mode-support)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.00000001))

;; Better help information
(use-package helpful
  :ensure
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Hydras
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "Scales text."
  ("n" text-scale-increase "in")
  ("p" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(defhydra hydra-window-resize (:timeout 4)
  "Resizes window."
  ("p" shrink-window 5 "shrink vertically")
  ("n" enlarge-window 5 "enlarge vertically")
  ("b" shrink-window-horizontally 5 "shrink horizontally")
  ("f" enlarge-window-horizontally 5 "enlarge horizontally")
  ("q" nil "finished" :exit t))

;; Project Management
(use-package project)

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
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Turn ^L into pretty lines
(use-package page-break-lines
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show empty whitespace
(setq whitespace-style '(face trailing tabs lines empty))
(add-hook 'prog-mode-hook 'whitespace-mode)

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

;; Smartparens
(use-package smartparens
  :defer 1
  :hook ((
          emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode
	  racket-mode racket-repl-mode scheme-mode geiser-repl-mode
	  hy-mode prolog-mode go-mode cc-mode
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
         ("M-<right>" . sp-forward-slurp-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("M-<left>" . sp-forward-barf-sexp)
         ("C-(" . sp-backward-slurp-sexp)
         ("M-S-<left>" . sp-backward-slurp-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("M-S-<right>" . sp-backward-barf-sexp)
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
	 ("C-M-n" . move-text-down)
	 ("C-M-p" . move-text-up)))

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

;; Syntax checking
(use-package flymake
  :ensure t
  :hook
  (prog-mode . flymake-mode)

  :bind
  (:map flymake-mode-map
   ("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error))

  :init
  ;; Disable legacy diagnostic functions as some have bugs (mainly haskell)
  (setq flymake-proc-ignored-file-name-regexps '("\\.l?hs\\'"))
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  :config
  (define-fringe-bitmap 'cory-exclamation-mark
    (vector #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000111
            #b00000010
            #b00000000
            #b00000000
            #b00000111
            #b00000111
            #b00000111))

  (define-fringe-bitmap 'cory-double-exclamation-mark
    (vector #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b11100111
            #b01000010
            #b00000000
            #b00000000
            #b11100111
            #b11100111
            #b11100111))

  (setq flymake-note-bitmap '(cory-exclamation-mark compilation-info)
	flymake-warning-bitmap '(cory-exclamation-mark compilation-warning)
	flymake-error-bitmap '(cory-double-exclamation-mark compilation-error)))

(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config
  (setq flymake-diagnostic-at-point-error-prefix nil)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package flymake-racket
  :ensure t
  :commands (flymake-racket-add-hook)
  :init
  (add-hook 'racket-mode-hook #'flymake-racket-add-hook))

(use-package flymake-kondor
  :ensure t
  :hook
  (clojure-mode . flymake-kondor-setup)
  (clojurescript-mode . flymake-kondor-setup)
  (clojurec-mode . flymake-kondor-setup))

;; (use-package flymake-joker
;;   :config
;;   (add-hook 'clojure-mode-hook #'flymake-joker-clj-enable)
;;   (add-hook 'clojurescript-mode-hook #'flymake-joker-cljs-enable)
;;   (add-hook 'clojure-mode-hook #'flymake-mode))

;; Display help messages automatically in echo area
(setq help-at-pt-timer-delay 0.1)
(setq help-at-pt-display-when-idle '(flymake-diagnostic))

;; Workspaces
(use-package eyebrowse
  :init (eyebrowse-mode t))

;; Sidebar (Project Explorer)
(use-package dired-sidebar :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom
  (dired-sidebar-subtree-line-prefix "   ")
  (dired-sidebar-theme 'nerd)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font t)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (set-face-attribute 'dired-sidebar-face nil :inherit 'variable-pitch))

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

;;; TODO Clean up clojure config
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
  :hook ( ;; ((cider-mode cider-repl-mode) . cider-company-enable-fuzzy-completion)
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

;;; Racket
(use-package racket-mode
  :mode "\\.rkt\\'"
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

(use-package dr-racket-like-unicode)
;; (use-package bracketed-paste)

;;; Common Lisp

(use-package sly
  :commands (sly sly-connect)
  :init
  (setq sly-symbol-completion-mode nil)
  (setq inferior-lisp-program "sbcl")
  ;; (setq sly-default-lisp 'roswell)
  ;; (setq ros-config (concat user-emacs-directory
  ;;                          "ros-conf.lisp"))
  ;; (setq sly-lisp-implementations
  ;;       `((sbcl ("sbcl") :coding-system utf-8-unix)
  ;;         (ccl ("ccl") :coding-system utf-8-unix)
  ;;         (ecl ("ecl") :coding-system utf-8-unix)
  ;;         (roswell ("ros" "-Q" "-l" ,ros-config "run"))
  ;;         (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
  ;;               :coding-system utf-8-unix)))
  (setq sly-lisp-implementations
        `((sbcl ("sbcl") :coding-system utf-8-unix)))

  ;; (defun qlot-sly ()
  ;;   "Start a sly repl using qlot at the projects root"
  ;;   (interactive)
  ;;   (let ((dir (cdr (project-current))))
  ;;     (if (cd dir)
  ;;         (sly 'qlot)
  ;;       (error (format "Failed to cd to %s" dir)))))

  ;; (defun sly-critique-defun ()
  ;;   "Lint this file with lisp-critic"
  ;;   (interactive)
  ;;   ;; (sly-eval-async '(ql:quickload :lisp-critic))
  ;;   (let ((form (apply #'buffer-substring-no-properties
  ;;                      (sly-region-for-defun-at-point))))
  ;;     (sly-eval-async
  ;;      `(cl:format  "~a" (list ,(read form)))
  ;;      nil (sly-current-package))))

  ;; (defun sly-critique-file ()
  ;;   "Lint this file with lisp-critic"
  ;;   (interactive)
  ;;   (sly-eval-async '(ql:quickload :lisp-critic))
  ;;   (sly-eval-async `(lisp-critic:critique ,(buffer-file-name))))
  )

(use-package sly-asdf
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sly-repl-ansi-color
  :config
  (push 'sly-repl-ansi-color sly-contribs))

(use-package common-lisp-snippets)

(use-package lisp-extra-font-lock)

;;; Chicken Scheme

(custom-set-variables '(scheme-program-name "csi -R r7rs"))

(add-to-list 'auto-mode-alist
             '("\\.egg\\'" . scheme-mode))
(add-to-list 'auto-mode-alist
             '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist
             '("\\.sld\\'" . scheme-mode))
(add-hook 'scheme-mode-hook
          (lambda ()
            (setq open-paren-in-column-0-is-defun-start t)))

(defun scheme-module-indent (state indent-point normal-indent) 0)

(defun scheme-indent-hook ()
  (put 'module 'scheme-indent-function 'scheme-module-indent)
  (put 'define-library 'scheme-indent-function 'scheme-module-indent)
  (put 'define-module 'scheme-indent-function 'scheme-module-indent))

(add-hook 'scheme-mode-hook 'scheme-indent-hook)

(use-package geiser
  :hook
  (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementations '(chicken)))

(use-package geiser-chicken)

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

;; (use-package company-irony
;;   :after irony
;;   :config (add-to-list 'company-backends 'company-irony))

(use-package irony-eldoc
  :hook (irony-mode))

;;(global-set-key [f9] 'code-compile)

;;; Java

(use-package eglot-java
  :init
  (eglot-java-init))

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

(use-package fvwm-mode
  ;; :mode "config"
  :hook (fvwm-mode . subword-mode))

;;
;; --- TERMINALS ---
;;

;;; Terminal

;; (use-package term
;;   :config
;;   (setq explicit-shell-file-name "zsh")
;;   ;;(setq explicit-zsh-args '())
;;   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; Use local Emacs instance as $EDITOR (e.g. in `git commit' or `crontab -e')
(use-package with-editor
  :hook ((shell-mode eshell-mode vterm-mode term-exec) . with-editor-export-editor))

;; Enhanced shell completion
;; (use-package pcmpl-args) ; slow?

;;; Eshell

(setenv "PAGER" "eshell/less")

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
	eshell-scroll-to-bottom-on-input t)

  (setq-local completion-in-region-function #'consult-completion-in-region))

(add-hook 'eshell-first-time-mode-hook 'cory/configure-eshell)

;; Eshell popup
;; (add-to-list 'display-buffer-alist
;;              '("\`\eshell\\(?:<[[:digit:]]+>\)?\'"
;;                (display-buffer-in-side-window (side . bottom))))

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
;; (use-package esh-autosuggest
;;   :ensure t
;;   :hook (eshell-mode . esh-autosuggest-mode))

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
(global-set-key (kbd "C-c s") 'replace-string)
(global-set-key (kbd "C-c w") 'woman)

;; FIXME
(defun kill-ring-save-and-comment (BEG END)
  "Save the region to the kill ring, then comment it out."
  (kill-ring-save BEG END)
  (comment-region BEG END))
(global-set-key (kbd "M-#") 'kill-ring-save-and-comment)

(use-package god-mode
  :custom
  (god-mode-alist
   '((nil . "C-")
     ("." . "M-")
     (">" . "C-M-")))
  (god-exempt-major-modes nil) ; for god-mode all
  (god-exempt-predicates nil)  ; for god-mode-all
  :config
  (defun my-god-mode-update-mode-line ()
    (cond
     (god-local-mode
      (set-face-attribute 'mode-line nil
                          :foreground "#141404"
                          :background "#ed8f23")
      ;; (set-face-attribute 'mode-line-inactive nil
      ;;                     :foreground "#141404"
      ;;                     :background "#ed9063"))
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "#ffffff"
                          :background "#5e3608"))
     (t
      ;; (set-face-attribute 'mode-line nil
      ;; 			  :foreground "#141404"
      ;; 			  :background "#cccccc")
      ;; (set-face-attribute 'mode-line-inactive nil
      ;; 			  :foreground "#141404"
      ;; 			  :background "#ffffff")
      (set-face-attribute 'mode-line nil
			  :foreground "#ffffff"
			  :background "#4d4d4d")
      (set-face-attribute 'mode-line-inactive nil
			  :foreground "#ffffff"
			  :background "#1a1a1a")
      )))
  (add-hook 'post-command-hook 'my-god-mode-update-mode-line)

  (global-set-key (kbd "<escape>") #'god-mode-all)
  (global-set-key (kbd "C-x C-0") #'delete-window)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-and-follow-below)
  (global-set-key (kbd "C-x C-3") #'split-and-follow-right)
  (define-key god-local-mode-map (kbd "z") #'repeat))

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Keybinds for Russian Layout
;; The keybinds map to be in the same location on the board
(global-set-key (kbd "C-н") #'next-line) ;; C-n
(global-set-key (kbd "C-р") #'previous-line) ;; C-p
(global-set-key (kbd "C-с") #'forward-char) ;; C-f
(global-set-key (kbd "C-т") #'backward-char) ;; C-b
(global-set-key (kbd "M-с") #'forward-word) ;; M-f
(global-set-key (kbd "M-т") #'backward-word) ;; M-b
(global-set-key (kbd "C-л") #'consult-line) ;; C-s
(global-set-key (kbd "C-д") #'recenter-top-bottom) ;; C-l
(global-set-key (kbd "C-к") #'sp-delete-char) ;; C-d
(global-set-key (kbd "C-е") #'beginning-of-visual-line) ;; C-a
(global-set-key (kbd "C-о") #'end-of-visual-line) ;; C-e
(global-set-key (kbd "C-х") #'sp-kill-hybrid-sexp) ;; C-k
(global-set-key (kbd "C-з") #'sp-kill-region) ;; C-w
(global-set-key (kbd "C-б") #'keyboard-quit) ;; C-g
(global-set-key (kbd "C-п") #'yank) ;; C-y
(global-set-key (kbd "C-ж") #'scroll-up-command) ;; C-v
(global-set-key (kbd "M-ж") #'scroll-down-command) ;; M-v
(global-set-key (kbd "M-ё") #'execute-extended-command) ;; M-x

;;
;; --- ORG MODE ---
;;

(use-package org
  :defer
  :hook
  (org-mode . (lambda ()
		(org-indent-mode)
		(variable-pitch-mode)
		;; (auto-fill-mode)
		(visual-line-mode)))

  :bind
  (("C-c o a" . org-agenda-list)
   ("C-c o A" . org-agenda)
   ("C-c o g" . consult-org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o r" . org-refile))

  :custom
  (org-ellipsis " ▼")
  (org-hide-emphasis-markers t)
  (org-agenda-files '("~/Code/Org/Tasks.org"
		      "~/Code/Org/School.org"
		      "~/Code/Org/Homework.org"))
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-on-weekday nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-pretty-entities t)
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-return-follows-link t)
  (org-latex-compiler "lualatex")
  (org-preview-latex-default-process 'dvisvgm)
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-agenda-current-time-string "← now ----------")
  (org-agenda-timegrid-use-ampm 1) ;; 12-hour clock
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (org-refile-targets
   '(("Archive.org" :maxlevel . 1)
     ("Tasks.org" :maxlevel . 1)))
  (org-tag-alist
   '((:startgroup)
     ;; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)))

  ;; Configure custom agenda views
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
	     ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

     ("n" "Next Tasks"
      ((todo "NEXT"
	     ((org-agenda-overriding-header "Next Tasks")))))

     ("W" "Work Tasks" tags-todo "+work")

     ;; Low-effort next actions
     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files)))

     ("w" "Workflow Status"
      ((todo "WAIT"
	     ((org-agenda-overriding-header "Waiting on External")
	      (org-agenda-files org-agenda-files)))
       (todo "REVIEW"
	     ((org-agenda-overriding-header "In Review")
	      (org-agenda-files org-agenda-files)))
       (todo "PLAN"
	     ((org-agenda-overriding-header "In Planning")
	      (org-agenda-todo-list-sublevels nil)
	      (org-agenda-files org-agenda-files)))
       (todo "BACKLOG"
	     ((org-agenda-overriding-header "Project Backlog")
	      (org-agenda-todo-list-sublevels nil)
	      (org-agenda-files org-agenda-files)))
       (todo "READY"
	     ((org-agenda-overriding-header "Ready for Work")
	      (org-agenda-files org-agenda-files)))
       (todo "ACTIVE"
	     ((org-agenda-overriding-header "Active Projects")
	      (org-agenda-files org-agenda-files)))
       (todo "COMPLETED"
	     ((org-agenda-overriding-header "Completed Projects")
	      (org-agenda-files org-agenda-files)))
       (todo "CANC"
	     ((org-agenda-overriding-header "Cancelled Projects")
	      (org-agenda-files org-agenda-files)))))))

  (org-capture-templates
   `(("t" "Tasks / Projects")
     ("tt" "Task" entry (file+olp "~/Code/Org/Tasks.org" "Inbox")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

     ("j" "Journal Entries")
     ("jj" "Journal" entry
      (file+olp+datetree "~/Code/Org/Journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
      :clock-in :clock-resume
      :empty-lines 1)
     ("jm" "Meeting" entry
      (file+olp+datetree "~/Code/Org/Journal.org")
      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)

     ("w" "Workflows")
     ("we" "Checking Email" entry (file+olp+datetree "~/Code/Org/Journal.org")
      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

     ("m" "Metrics Capture")
     ("mw" "Weight" table-line (file+headline "~/Code/Org/Metrics.org" "Weight")
      "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  :config
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent nil   :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :hook
  (text-mode   . visual-fill-column-mode)
  (prog-mode   . visual-fill-column-mode)
  (conf-mode   . visual-fill-column-mode)
  ;; (term-mode   . visual-fill-column-mode)
  ;; (eshell-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

;; Drag and drop
(use-package org-download
  :commands (org-mode org-download-clipboard)
  :custom
  (org-download-screenshot-method "flameshot gui -s --raw > %s")
  :bind ("<f8>" . org-download-screenshot))

;; Spelling
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package frog-menu
  :custom
  ;; Need to redefine keys to account for custom keyboard layout
  (frog-menu-avy-keys (append (string-to-list "aoehsfnbid")
			      (string-to-list "ulrptgy")
			      (string-to-list "xcjzvwmqk")
			      (string-to-list (upcase "aoehsfnbid"))
			      (string-to-list (upcase "ulrptgy"))
			      (string-to-list (upcase "xcjzvwmqk"))
			      (number-sequence ?, ?@)))
  :config
  (defun frog-menu-flyspell-correct (candidates word)
    "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
    (let* ((corrects (if flyspell-sort-corrections
			 (sort candidates 'string<)
                       candidates))
           (actions `(("C-s" "Save word"         (save    . ,word))
                      ("C-a" "Accept (session)"  (session . ,word))
                      ("C-b" "Accept (buffer)"   (buffer  . ,word))
                      ("C-c" "Skip"              (skip    . ,word))))
           (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                    ispell-dictionary
                                                    "default")))
           (res      (frog-menu-read prompt corrects actions)))
      (unless res
	(error "Quit"))
      res))

  (setq flyspell-correct-interface #'frog-menu-flyspell-correct))

;;
;; --- MISC ---
;;

;; Disable startup screen
(setq inhibit-startup-message t)

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

;; PDFs
(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; Emacs run launcher
(defun emacs-run-launcher ()
  "A frame to launch desktop applications."
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
		    (minibuffer . only)
		    (width . 100)
		    (height . 11)))
    (unwind-protect
	(app-launcher-run-app)
      (delete-frame))))

;; Edit anything with emacs
(use-package emacs-everywhere)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(70 . 70))
(add-to-list 'default-frame-alist '(alpha . (70 . 70)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(70 . 70) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;; init.el ends here
