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
;; --- ASYNC ---
;;

;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

;;
;; --- KEYBINDING FIX ---
;;
(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])
(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (define-key input-decode-map [?\C-m] [C-m])
	    (define-key input-decode-map [?\C-i] [C-i])))

;; Now:
;; (equal (kbd "TAB") (kbd "C-i"))   ; -> t
;; (equal (kbd "TAB") (kbd "<C-i>")) ; -> nil
;; (equal (kbd "RET") (kbd "C-m"))   ; -> t
;; (equal (kbd "RET") (kbd "<C-m>")) ; -> nil

;;
;; --- VISUALS ---
;;

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips

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
;; (add-hook 'emacs-startup-hook (lambda () (load-theme 'plain-dark t)))
(add-hook 'emacs-startup-hook (lambda () (load-theme 'plain-light t)))

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

;; Buffer state in modeline
(defface modeline-narrow-face
  '((t (:foreground "#141404" :background "#ed8f23")))
  "Todo/fixme highlighting."
  :group 'faces)

(defface modeline-read-only-face
  '((t (:foreground "#141404" :background "#9feaae")))
  "Read-only buffer highlighting."
  :group 'faces)

(defface modeline-modified-face
  '((t (:foreground "#d8d8d8" :background "#e60909")))
  "Modified buffer highlighting."
  :group 'faces)

(setq-default
 mode-line-format
 '("  "
   (:eval (when (bound-and-true-p meow-mode) (meow-indicator)))
   " "
   (:eval (let ((str (if buffer-read-only
                         (if (buffer-modified-p) "%%*" "%%%%")
                       (if (buffer-modified-p) "**" "--"))))
            (if buffer-read-only
                (propertize str 'face 'modeline-read-only-face)
              (if (buffer-modified-p)
                  (propertize str 'face 'modeline-modified-face)
                str))))
   (list 'line-number-mode "  ")
   (:eval (when line-number-mode
            (let ((str "L%l"))
              (if (/= (buffer-size) (- (point-max) (point-min)))
                  (propertize str 'face 'modeline-narrow-face)
                str))))
   "  %p"
   (list 'column-number-mode "  C%c")
   "  " mode-line-buffer-identification
   "  " mode-line-modes
   (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))))

(use-package moody
  :custom
  ;; (moody-mode-line-height (* (aref (font-info (face-font 'mode-line)) 2) 1.5))
  (moody-mode-line-height 40)
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :config (minions-mode))

;; (defvar +smart-file-name-cache nil)

;; (defun +shorten-long-path (path)
;;   (let ((paths (split-string path "/")))
;;     (if (< (length paths) 3)
;;         path
;;       (string-join (reverse (let ((rpaths (reverse paths)))
;;                               (-concat
;;                                (-take 2 rpaths)
;;                                (->> (-drop 2 rpaths)
;;                                   (--map (if (> (length it) 1)
;;                                              (substring it 0 1)
;;                                            it))))))
;;                    "/"))))

;; (defun +smart-file-name ()
;;   "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
;; This function is slow, so we have to use cache."
;;   (let ((vc-dir (vc-root-dir))
;;         (bfn (buffer-file-name (current-buffer))))
;;     (cond
;;      ((and bfn vc-dir)
;;       (+shorten-long-path (file-relative-name bfn vc-dir)))
;;      (bfn bfn)
;;      (t (buffer-name)))))

;; (defun +smart-file-name-cached ()
;;   (if (eq (buffer-name) (car +smart-file-name-cache))
;;       (cdr +smart-file-name-cache)
;;     (let ((file-name (+smart-file-name)))
;;       (setq +smart-file-name-cache
;;             (cons (buffer-name) file-name))
;;       file-name)))

;; (defun +format-mode-line ()
;;   (let* ((lhs '((:eval (when (bound-and-true-p meow-mode) (meow-indicator)))
;; 		(:eval " L%l C%C")
;; 		(:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))))
;;          (rhs '((:eval (+smart-file-name-cached))
;;                 " "
;;                 (:eval mode-name)))
;;          (ww (window-width))
;;          (lhs-str (format-mode-line lhs))
;;          (rhs-str (format-mode-line rhs))
;;          (rhs-w (string-width rhs-str)))
;;     (format "%s%s%s"
;;             lhs-str
;;             (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
;;             rhs-str)))

;; (setq-default mode-line-format '((:eval (+format-mode-line))))
;; (setq-default header-line-format nil)

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
;; (setq-default cursor-type 'bar)
(setq-default cursor-type 'hollow)

;; Beacon
(use-package beacon
  :config
  (beacon-mode 1))

;; Visual feedback on yank/kill
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil))

;; Super smooth scrolling
;; (setq scroll-step            1
;;       scroll-conservatively  10000)
;; (setq next-screen-context-lines 5)

;; Smooth scrolling
;; (setq mouse-wheel-scroll-amount '(5 ((shift) . 5) ((control) . nil)))
;; (setq mouse-wheel-progressive-speed nil)

;; Smooth pixel scrolling
;; (pixel-scroll-mode 1)

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

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                    (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                    (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

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
;; (setq make-backup-files nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

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
  ;; (scheme-mode . eglot-ensure)
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
  ;; (add-to-list 'eglot-server-programs
  ;;              `(scheme-mode . ("chicken-lsp-server")))
  (add-to-list 'eglot-server-programs
               `(clojure-mode . ("clojure-lsp")))
  ;; (add-to-list 'eglot-server-programs
  ;;              `(nix-mode . ("nil")))

  :bind (:map eglot-mode-map
	 ("C-c C-a" . eglot-code-actions)
	 ("C-c f" . eglot-format-buffer)))

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
  (global-corfu-mode)
  (corfu-history-mode))

;; Add extensions
(use-package cape
  :ensure t
  :custom
  (cape-dict-file "~/.local/share/dict/words")
  :bind
  (("C-c p i" . cape-ispell)
   ("C-c p w" . cape-dict)
   ("C-c p d" . cape-dabbrev)
   ("C-c p l" . cape-line))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)

  :config
  ;; (setq-local completion-at-point-functions
  ;;             (list (cape-super-capf #'cape-dabbrev #'cape-dict #'cape-keyword #'cape-symbol)))

  ;; Add dictionary just to text modes
  (add-hook 'text-mode-hook (lambda ()
			      (setq-local completion-at-point-functions
			                  (cons #'cape-dict
						completion-at-point-functions))))

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
  ;; (completion-styles '(orderless basic))
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator "[ \\]"))

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
  (completion-in-region-function #'consult-completion-in-region)
  (add-hook 'completion-setup-hook #'hl-line-mode))

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
  (("<C-i>" . embark-act) ; pick some comfortable binding
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d" . dragon-drop))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  (embark-quit-after-action nil)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (defun search-in-source-graph (text))
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
				 (concat "dragon-drag-and-drop " file))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Templates
;; (use-package tempel
;;   ;; Require trigger prefix before template name when completing.
;;   ;; :custom
;;   ;; (tempel-trigger-prefix "<")

;;   :hook ((prog-mode text-mode) . tempel-setup-capf)

;;   :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
;;          ("M-*" . tempel-insert)
;; 	 :map tempel-map
;; 	 ("C-<tab>" . tempel-next)
;; 	 ("C-S-<tab>" . tempel-previous)
;; 	 ([remap keyboard-escape-quit] . tempel-done))

;;   :init
;;   Setup completion at point
;;   (defun tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-complete
;;                       completion-at-point-functions)))

;;   ;; Optionally make the Tempel templates available to Abbrev,
;;   ;; either locally or globally. `expand-abbrev' is bound to C-x '.
;;   ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;;   ;; (global-tempel-abbrev-mode)
;;   )

;; Undo
;; (use-package undo-tree
;;   :defer 1
;;   :diminish undo-tree-mode
;;   :commands (global-undo-tree-mode)
;;   :config
;;   (setq undo-tree-visualizer-relative-timestamps t
;;         undo-tree-visualizer-timestamps t
;;         undo-tree-enable-undo-in-region t)
;;   (global-undo-tree-mode))

;; Visual Keybinding Info
(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :custom
  ;; (which-key-idle-delay 0.00000001)
  (which-key-idle-delay 1.0))

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
  :bind (("C-c g s" . magit-status)
	 :map magit-stash-mode-map
	 ("W" . magit-toggle-whitespace))
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-options)
	(magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh)))

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

(use-package crux
  :bind (([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
	 ("C-c u" . crux-view-url)
	 ("C-c e" . crux-eval-and-replace)
	 ("C-x 4 t" . crux-transpose-windows)
	 ("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c D" . crux-duplicate-and-comment-current-line-or-region)
	 ("C-c k" . crux-kill-other-buffers)
	 ("C-^" . crux-top-join-line)
	 ("C-k" . crux-kill-and-join-forward-2)
	 ([remap kill-whole-line]. crux-kill-whole-line)
         ("C-a"   . crux-move-beginning-of-line))
  :config
  ;; TODO need to detect when the point is at the beginning of indentation
  (defun crux-kill-and-join-backward ()
    (interactive)
    (if (and (bolp) (not (eolp)))
	(delete-indentation)
      (kill-line 0)
      (indent-according-to-mode)))

  (defun crux-kill-and-join-forward-2 (&optional arg)
    (interactive "P")
    (if (< (prefix-numeric-value arg) 0)
	(crux-kill-and-join-backward)
      (crux-kill-and-join-forward))))

;; better comment-dwim
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Smartparens
(use-package smartparens
  :defer 1
  :hook ((
          emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode
	  racket-mode racket-repl-mode scheme-mode geiser-repl-mode json-mode
          ) . smartparens-strict-mode)
  :bind (:map smartparens-mode-map
         ;; This is the paredit mode map minus a few key bindings
         ;; that I use in other modes (e.g. M-?)
         ("C-M-f" . sp-forward-sexp) ;; navigation
         ("C-M-b" . sp-backward-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-n" . sp-up-sexp)
         ("M-S" . sp-splice-sexp) ;; depth-changing commands
         ("M-R" . sp-splice-sexp-killing-around)
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
         ("M-j" . sp-join-sexp)
	 )
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
  (add-to-list 'sp-sexp-suffix '(c-mode regexp ""))
  (add-to-list 'sp-sexp-suffix '(c++-mode regexp ""))
  (add-to-list 'sp-sexp-suffix '(nix-mode regexp ""))

  ;; use smartparens-mode everywhere
  (smartparens-global-mode))

;; Smart-region: Smart region selection
;; Smart region guesses what you want to select by one command:
;; - If you call this command multiple times at the same position,
;;   it expands the selected region (with `er/expand-region’).
;; - Else, if you move from the mark and call this command,
;;   it selects the region rectangular (with `rectangle-mark-mode’).
;; - Else, if you move from the mark and call this command at the same column as
;;   mark, it adds a cursor to each line (with `mc/edit-lines’).

(use-package expand-region
  :defer t)

(use-package smart-region
  ;; C-SPC is smart-region
  :bind (([remap set-mark-command] . smart-region)))

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

;; Phi search
(use-package phi-search)

;; Visual regex replacement
(use-package visual-regexp
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ;; for multiple-cursors
   ("C-c M" . vr/mc-mark)))

;; Move text
(use-package move-text
  :bind (([(control shift up)]   . move-text-up)
         ([(control shift down)] . move-text-down)
         ([(meta shift up)]      . move-text-up)
         ([(meta shift down)]    . move-text-down)
	 ("C-S-n" . move-text-down)
	 ("C-S-p" . move-text-up)))

;; Jump around
(use-package avy
  :ensure t
  :bind
  (;; ("M-g g" . avy-goto-line)
   ;; ("M-g c" . avy-goto-char-in-line)
   ;; ("M-g m" . avy-move-line)
   ("<C-m>" . avy-goto-char-timer)
   ("C-S-m" . avy-pop-mark)
   ("M-SPC" . avy-goto-end-of-line)
   ("M-S-SPC" . avy-goto-line)
   ("C-M-s" . isearch-forward-other-window)
   ("C-M-r" . isearch-backward-other-window)
   :map isearch-mode-map
   ("<C-m>" . avy-isearch))

  :custom
  ;; (setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
  ;;                     ?a ?s ?d ?f ?g ?h ?j
  ;;                     ?k ?l ?' ?x ?c ?v ?b
  ;;                     ?n ?, ?/))
  ;; (avy-keys (append (string-to-list "aoehsfb")
  ;; 		    (string-to-list "ulrpdc")
  ;; 		    (string-to-list "qjvg")
  ;; 		    (string-to-list (upcase "aoehsfb"))
  ;; 		    (string-to-list (upcase "ulrpdc"))
  ;; 		    (string-to-list (upcase "qjvg"))
  ;; 		    (number-sequence ?, ?')))
  (avy-keys (nconc (number-sequence ?a ?z)
		   (number-sequence ?A ?Z)))
  (avy-timeout-seconds 0.25)

  :config

  ;; Most of the below was copied from and/or inspired by
  ;; https://github.com/xl666/avy-conf/blob/main/avy.org

  ;; Need to use C- now because all letters of the alphabet are taken
  ;; Rebind default avy actions
  (setf (alist-get ?\C-y avy-dispatch-alist) 'avy-action-yank
	(alist-get ?\M-w avy-dispatch-alist) 'avy-action-copy
	(alist-get ?\C-k avy-dispatch-alist) 'avy-action-kill-move
	(alist-get (kbd "<C-m>") avy-dispatch-alist) 'avy-action-teleport)

  ;; Avy helper functions for both generic and complex avy actions
  (defun avy-generic-command-action (action-f)
    "Excecutes action-f at point and stays"
    (save-excursion
      (goto-char pt)
      (funcall action-f))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-generic-command-action-no-stay (action-f)
    "Excecutes action-f at point and returns to original position"
    (goto-char pt)
    (funcall action-f)
    t)

  ;;; Actions from "Avy can do anything"

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char (+ 1 pt)))
  (setf (alist-get 67108896 avy-dispatch-alist) 'avy-action-mark-to-char) ; C-SPC

  (defun avy-action-helpful (pt)
    (avy-generic-command-action #'helpful-at-point))
  (setf (alist-get ?\C-h avy-dispatch-alist) 'avy-action-helpful)

  (defun avy-action-flyspell (pt)
    (avy-generic-command-action #'flyspell-auto-correct-word))
  (setf (alist-get 67108923 avy-dispatch-alist) 'avy-action-flyspell) ; C-;

  (defun avy-action-kill-whole-line (pt)
    (avy-generic-command-action #'kill-whole-line))
  (setf (alist-get (kbd "C-M-k") avy-dispatch-alist) 'avy-action-kill-whole-line)

  (defun avy-action-copy-whole-line (pt)
    (avy-generic-command-action (lambda () (cl-destructuring-bind (start . end)
					  (bounds-of-thing-at-point 'line)
					(copy-region-as-kill start end)))))
  (setf (alist-get (kbd "C-M-w") avy-dispatch-alist) 'avy-action-copy-whole-line)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank)) t)
  (setf (alist-get (kbd "C-M-y") avy-dispatch-alist) 'avy-action-yank-whole-line)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (setf (alist-get (kbd "C-M-t") avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun avy-action-define (pt)
    (avy-generic-command-action #'dictionary-search-dwim))
  (setf (alist-get (kbd "C-=") avy-dispatch-alist) 'avy-action-define)

  (defun avy-action-embark (pt)
    (unwind-protect (avy-generic-command-action #'embark-act)) t)
  (setf (alist-get (kbd "<C-i>") avy-dispatch-alist) 'avy-action-embark)

  ;;; New behavior

  ;; Open org link (only relevant for org files)
  (defun avy-action-open-at-point (pt)
    (goto-char pt)
    (org-open-at-point)
    t)
  (setf (alist-get ?\C-o avy-dispatch-alist) 'avy-action-open-at-point)

  ;; Clone line below
  (defun avy-action-clone-line (pt)
    (goto-char pt)
    (move-beginning-of-line 1)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end))
    (yank)
    (indent-for-tab-command)
    t)
  (setf (alist-get ?\C-l avy-dispatch-alist) 'avy-action-clone-line)

  ;;; Regions

  ;; - The idea is to be able to act in arbitrary regions without the need of manually marking a region
  ;; - It relies on two basic operations:
  ;;   - First mark the beginning of the region with a set-point action
  ;;   - Then apply a region action selecting the end of the region
  ;; - Region actions are the same as Original actions but for regions
  ;; - Region actions take the original code of avy actions as much as possible
  ;; - A necessary hack is to simulate region selection instead of using direct functions like `copy-region-as-kill' as those functions do not allow to manipulate regions if parenthesis or other syntax elements are not balanced. This has a weird behavior in modes like emacs-lisp so I’m not sure if it is a syntax problem

  ;; set-point-action
  (defun avy-action-mark-point (pt)
    "Sets a point for other commands"
    (setq my-avy-point pt)
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    (message "Point set!"))
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-mark-point)

  ;; Common region functions
  (defun avy--quick-mark-region (pt)
    "Intermediate function to mark regions, used in region actions"
    (when (> my-avy-point pt)
      (progn
	(setf aux pt)
	(setf pt my-avy-point)
	(setf my-avy-point aux)))
    (goto-char my-avy-point)
    (set-mark my-avy-point)
    (activate-mark)
    (goto-char (+ 1 pt))
    (setq my-avy-point nil))

  (defun avy--return-point-region-action ()
    "Makes sure that the point returns to its original place even if it is in another window"
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat))))

  (defun avy--check-for-region-errors ()
    "Cheks if set point action was previously made, cleans action otherwise"
    (progn (message "No point set")
           (avy--return-point-region-action)
           nil))

  ;; Region actions
  (defun avy-action-copy-region (pt)
    "Copy region and stays"
    (if my-avy-point
	(progn
          (save-excursion
            (avy--quick-mark-region pt)
            (call-interactively 'kill-ring-save))
          (avy--return-point-region-action)
          (message "Copied: %s" (current-kill 0))
          t)
      (avy--check-for-region-errors)))
  (setf (alist-get ?\M-W avy-dispatch-alist) 'avy-action-copy-region)

  (defun avy-action-yank-region (pt)
    "Yank region and stays"
    (avy-action-copy-region pt)
    (yank)
    t)
  (setf (alist-get 33554457 avy-dispatch-alist) 'avy-action-yank-region) ; C-Y

  (defun avy-action-kill-region-move (pt)
    "Kills a region and moves"
    (if my-avy-point
	(progn
          (avy--quick-mark-region pt)
          (call-interactively 'kill-region)
          (message "Killed: %s" (current-kill 0))
          (point)
          t)
      (avy--check-for-region-errors)))
  (setf (alist-get 33554443 avy-dispatch-alist) 'avy-action-kill-region-move) ; C-K

  (defun avy-action-teleport-region (pt)
    "Teleports an arbitrary region using my-avy-point"
    (if my-avy-point
	(progn
	  (save-excursion
            (avy--quick-mark-region pt)
            (call-interactively 'kill-region))
	  (select-window
	   (cdr
            (ring-ref avy-ring 0)))
	  (yank)
	  t)
      (avy--check-for-region-errors)))
  (setf (alist-get 33554452 avy-dispatch-alist) 'avy-action-teleport-region) ; C-T

  ;;; Quick char actions
  ;; For some modes it is useful to have a shortcut for a common character, for example parenthesis in emacs-lisp

  ;; Basic funcion
  (defun avy-goto-quick-char (char &optional arg)
    "Simulates char press for filtering"
    (interactive (list char
                       current-prefix-arg))
    (avy-with avy-goto-char
      (avy-jump

       (regexp-quote (string char)))))

  ;; `emacs-lisp-mode'
  (defun avy-goto-parenthesis ()
    "Filter avy selecton with open parenthesis"
    (interactive)
    (avy-goto-quick-char 40)) ;; (
  (define-key emacs-lisp-mode-map (kbd "S-SPC") 'avy-goto-parenthesis)

  ;;; TODO Auto actions and compounds

  ;;; LSP

  ;; (defun avy-action-lsp-help (pt)
  ;;   (avy-generic-command-action #'lsp-describe-thing-at-point))
  ;; (setf (alist-get 16777320 avy-dispatch-alist) 'avy-action-lsp-help) ; H-h

  (defun avy-action-lsp-goto-definition (pt)
    (avy-generic-command-action-no-stay #'xref-find-definitions))
  (setf (alist-get (kbd "M-.") avy-dispatch-alist) 'avy-action-lsp-goto-definition) ; M-.

  (defun avy-action-lsp-goto-references (pt)
    (avy-generic-command-action-no-stay #'xref-find-references))
  (setf (alist-get (kbd "M-?") avy-dispatch-alist) 'avy-action-lsp-goto-references) ; M-?

  (defun avy-action-lsp-rename (pt)
    (avy-generic-command-action
     (lambda () (call-interactively 'eglot-rename))))
  (setf (alist-get 16777330 avy-dispatch-alist) 'avy-action-lsp-rename) ; C-r

  ;;; Functions

  ;; Avy + Isearch
  ;; Isearch in other windows
  (defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
	(let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

  (defun isearch-backward-other-window (prefix)
    "Function to isearch-backward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
	(let ((next (if prefix 1 -1)))
          (other-window next)
          (isearch-backward)
          (other-window (- next))))))

  ;; Dictionary search dwim
  (defun dictionary-search-dwim (&optional arg)
    "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
    (interactive "P")
    (if arg
	(dictionary-search nil)
      (if (use-region-p)
          (dictionary-search (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
	(if (thing-at-point 'word)
            (dictionary-lookup-definition)
          (dictionary-search-dwim '(4))))))

  ;; Show help in avy dispatch with ?
  (defun avy-show-dispatch-help ()
    (let* ((len (length "avy-action-"))
           (fw (frame-width))
           (raw-strings (mapcar
			 (lambda (x)
			   (format "%2s: %-19s"
				   (propertize
				    (char-to-string (car x))
				    'face 'aw-key-face)
				   (substring (symbol-name (cdr x)) len)))
			 avy-dispatch-alist))
           (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
           (strings-len (length raw-strings))
           (per-row (floor fw max-len))
           display-strings)
      (cl-loop for string in raw-strings
               for N from 1 to strings-len do
               (push (concat string " ") display-strings)
               (when (= (mod N per-row) 0) (push "\n" display-strings)))
      (message "%s" (apply #'concat (nreverse display-strings))))))

(use-package avy-zap
  :bind
  (("M-z" . avy-zap-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))

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
   ("M-n"     . flymake-goto-next-error)
   ("M-p"     . flymake-goto-prev-error)
   ("M-g n"   . flymake-goto-next-error)
   ("M-g p"   . flymake-goto-prev-error)
   ("M-g M-n" . flymake-goto-next-error)
   ("M-g M-p" . flymake-goto-prev-error)
   ("M-g d"   . flymake-show-buffer-diagnostics)
   ("M-g M-d" . flymake-show-project-diagnostics))

  :init
  ;; Disable legacy diagnostic functions as some have bugs (mainly haskell)
  (setq flymake-proc-ignored-file-name-regexps '("\\.l?hs\\'"))
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  :config

  (define-fringe-bitmap 'cory-info-mark
    (vector #b0000001111000000
	    #b0000111111110000
	    #b0001110000111000
	    #b0011000000001100
	    #b0110000000000110
	    #b0110000000000110
	    #b1100000000000011
	    #b1100000000000011
	    #b1100000000000011
	    #b1100000000000011
	    #b0110000000000110
	    #b0110000000000110
	    #b0011000000001100
	    #b0001100000011000
	    #b0000110000110000
	    #b0000110000110000
	    #b0000111111110000
	    #b0000011111100000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000011111100000
	    #b0000011111100000
	    #b0000001111000000
	    #b0000001111000000)
    23
    16
    'center)

  (define-fringe-bitmap 'cory-warning-mark
    (vector #b0000000110000000
	    #b0000000110000000
	    #b0000001111000000
	    #b0000001111000000
	    #b0000011001100000
	    #b0000011001100000
	    #b0000110000110000
	    #b0000110110110000
	    #b0001100110011000
	    #b0001100110011000
	    #b0011000110001100
	    #b0011000000001100
	    #b0110000110000110
	    #b0110000110000110
	    #b1100000000000011
	    #b1111111111111111)
    16
    16
    'center)

  (define-fringe-bitmap 'cory-error-mark
    (vector #b0011000000001100
	    #b0111100000011110
	    #b1100110000110011
	    #b1100011001100011
	    #b0110001111000110
	    #b0011000110001100
	    #b0001100000011000
	    #b0000110000110000
	    #b0000110000110000
	    #b0001100000011000
	    #b0011000110001100
	    #b0110001111000110
	    #b1100011001100011
	    #b1100110000110011
	    #b0111100000011110
	    #b0011000000001100)
    16
    16
    'center)

  (setq flymake-note-bitmap '(cory-info-mark compilation-info)
	flymake-warning-bitmap '(cory-warning-mark compilation-warning)
	flymake-error-bitmap '(cory-error-mark compilation-error)))

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
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

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

;; Snippets
(use-package yasnippet
  :config
  ;; Don't touch TAB!!!

  ;; The active keymap while a snippet expansion is in progress.
  (setq yas-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-<tab>")   (yas-filtered-definition 'yas-next-field-or-maybe-expand))
	  (define-key map (kbd "C-M-<tab>") (yas-filtered-definition 'yas-prev-field))
	  (define-key map (kbd "C-g")   (yas-filtered-definition 'yas-abort-snippet))
	  (define-key map (kbd "C-d")   (yas-filtered-definition yas-maybe-skip-and-clear-field))
	  (define-key map (kbd "DEL")   (yas-filtered-definition yas-maybe-clear-field))
	  map))

  ;; The keymap used when `yas-minor-mode' is active.
  (setq yas-minor-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-<tab>") yas-maybe-expand)
	  (define-key map "\C-c&\C-s" 'yas-insert-snippet)
	  (define-key map "\C-c&\C-n" 'yas-new-snippet)
	  (define-key map "\C-c&\C-v" 'yas-visit-snippet-file)
	  map))

  (yas-global-mode 1))

(use-package common-lisp-snippets)

(use-package clojure-snippets)

(use-package java-snippets)

(use-package gitignore-snippets
  :config (gitignore-snippets-init))

;; Code folding
;; (dolist (mode '(c-mode-common-hook
;; 		emacs-lisp-mode-hook
;; 		lisp-mode-hook
;; 		clojure-mode-hook
;; 		clojurescript-mode-hook
;; 		clojurec-mode-hook
;; 		java-mode-hook
;; 		perl-mode-hook
;; 		sh-mode-hook
;; 		nix-mode-hook))
;;   (add-hook mode 'hs-minor-mode))
;; (global-set-key (kbd "C-+") 'hs-toggle-hiding)

(use-package origami
  :bind
  (("C-=" . origami-toggle-node)
   ("C-+" . origami-show-only-node)
   ;; ("C-+" . origami-recursively-toggle-node)
   )
  :config
  (global-origami-mode))

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

  ;; Location of the jdk sources for NixOS
  (setq cider-jdk-src-paths "/etc/profiles/per-user/cory/lib/openjdk/lib/src.zip")

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
;; (custom-set-variables '(scheme-program-name "scheme"))

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
  (geiser-active-implementations '(chicken))
  ;; (geiser-active-implementations '(chez))
  :config
  ;; Add geiser capfs to capf
  (add-hook 'scheme-mode-hook
	    (lambda ()
	      (setq-local completion-at-point-functions
			  (append geiser-capf--capfs
				  completion-at-point-functions)))))

(use-package geiser-chicken)

;; (use-package geiser-chez)

;;; C++
(use-package modern-cpp-font-lock
  :ensure t)
(modern-c++-font-lock-global-mode t)

(use-package cpp-auto-include)

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

;; (use-package irony
;;   :hook (((c++-mode c-mode objc-mode) . irony-mode-on-maybe)
;;          (irony-mode . irony-cdb-autosetup-compile-options))
;;   :config
;;   (defun irony-mode-on-maybe ()
;;     ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: solidity-mode
;;     (when (member major-mode irony-supported-major-modes)
;;       (irony-mode 1))))

;; (use-package irony-eldoc
;;   :hook (irony-mode . irony-eldoc))

(use-package srefactor
  :bind
  (:map c-mode-map
   ("C-c C-r" . srefactor-refactor-at-point)
   :map c++-mode-map
   ("C-c C-r" . srefactor-refactor-at-point))
  :config
  (semantic-mode 1))

;; (defun code-compile ()
;;   (interactive)
;;   (unless (file-exists-p "Makefile")
;;     (set (make-local-variable 'compile-command)
;; 	 (let ((file (file-name-nondirectory buffer-file-name)))
;; 	   (format "%s -o %s %s"
;; 		   (if  (equal (file-name-extension file) "cpp") "clang++" "clang" )
;; 		   (file-name-sans-extension file)
;; 		   file)))
;;     (compile compile-command)))

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
  :hook ((shell-mode-hook eshell-mode-hook term-exec-hook vterm-exec-hook)
         . with-editor-export-editor)
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command)))

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

;; Eshell fish completion
(use-package fish-completion
  :config
  (when (and (executable-find "fish")
           (require 'fish-completion nil t))
    (global-fish-completion-mode)))

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

;;; Search functions

(require 's)

(defun cory/visual-isearch-forward ()
  (interactive)
  (consult-line)
  (beginning-of-line)
  (let ((string (car (s-split " " (car consult--line-history)))))
    (isearch-resume string nil nil t string t)))

(defun cory/visual-isearch-backward ()
  (interactive)
  (consult-line)
  (end-of-line)
  (let ((string (car (s-split " " (car consult--line-history)))))
    (isearch-resume string nil nil nil string t)))

(defun cory/search-forward-dwim ()
  (interactive)
  ;; Are we using multiple cursors?
  (cond ((and (boundp 'multiple-cursors-mode)
	    multiple-cursors-mode
	    (fboundp  'phi-search))
         (call-interactively 'phi-search))
        ;; Are we defining a macro?
        (defining-kbd-macro
          (call-interactively 'isearch-forward))
        ;; Fall back to isearch.
        (t
         ;; If region is active, prepopulate the isearch term.
	 (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
	     (let ((region (buffer-substring-no-properties (mark) (point))))
               (deactivate-mark)
               (isearch-resume region nil nil t region nil))
	   (cory/visual-isearch-forward)))))

(defun cory/search-backward-dwim ()
  (interactive)
  ;; Are we using multiple cursors?
  (cond ((and (boundp 'multiple-cursors-mode)
            multiple-cursors-mode
            (fboundp  'phi-search-backward))
         (call-interactively 'phi-search-backward))
        ;; Are we defining a macro?
        (defining-kbd-macro
          (call-interactively 'isearch-backward))
        ;; Fall back to isearch.
        (t
         ;; If region is active, prepopulate the isearch term.
         (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
             (let ((region (buffer-substring-no-properties (mark) (point))))
               (deactivate-mark)
               (isearch-resume region nil nil nil region nil))
           (cory/visual-isearch-backward)))))

;;; Scroll functions

(defun cory/scroll-down-half-page ()
  "scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
	  ((= ln lmax) (recenter (window-end)))
	  (t (progn
               (move-to-window-line -1)
               (recenter))))))

(defun cory/scroll-up-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
	  ((= ln lmax) (move-to-window-line nil))
	  (t (progn
               (move-to-window-line 0)
               (recenter))))))

(put 'cory/scroll-down-half-page 'scroll-command t)
(put 'cory/scroll-up-half-page 'scroll-command t)

;; Basic Keybind

;; Swap "C-h" and "C-x", so it's easier to type on Dvorak layout
;; (keyboard-translate (kbd "C-h") (kbd "C-x"))
;; (keyboard-translate (kbd "C-x") (kbd "C-h"))

(dolist (pair '(("C-x k"   kill-this-buffer)
		;; ("C-#"     comment-or-uncomment-region)
		;; ("C-c s"   replace-string)
		("C-c w"   woman)
		("C-x u"   undo-only)
		("C-/"     undo-only)
		("C-z"     undo-only)
		("C-S-z"   undo-redo)
		("C-x C-u" undo-redo)
		("C-?"     undo-redo)
		("C-'"     repeat)
		("C-s"     cory/search-forward-dwim)
		("C-r"     cory/search-backward-dwim)
		("C-v"     cory/scroll-down-half-page)
		("M-v"     cory/scroll-up-half-page)))
  (global-set-key (kbd (car pair)) (cadr pair)))

(defun repeaters-define-maps (rlist)
  "Define an arbitrary number of repeater maps.
Maps are defined based on the lists passed through RLIST, a
quoted list containing ‘repeat-map’ definitions.  Each definition
is itself a list containing the following items:
NAME is a string designating the unique portion of the
repeat-map’s name (to be constructed into the form
‘repeaters-NAME-rep-map’ as the name of the symbol for the map).
One or more command ENTRIES made up of the following:
    The COMMAND’s symbol;
    One or more string representations of KEY-SEQUENCES which
    may be used to invoke the command when the ‘repeat-map’ is
    active;
    Optionally, the KEYWORD ‘:exitonly’ may follow the key sequences.
A single map definition may include any number of these command
entry constructs.
If a command construct ends with the ‘:exitonly’ keyword, the map
can invoke the command, but the command will *not* invoke that
map.
However, if the keyword is omitted, the command will bring up the
‘repeat-map’ whenever it is called using one of the keysequences
given in the ‘repeat-map’.  A given command may only store a
single map within its ‘repeat-map’ property, although a command
can be called from multiple repeat-maps.
Taking advantage of this fact, one may chain related repeat-maps
together in sequence."
  (while rlist
    (let* ((block (pop rlist))
           (mapname (concat "repeaters-" (pop block) "-rep-map")))
      (set (intern mapname)
           (let ((map (make-sparse-keymap))
                 (thing (pop block)))
             (while block
               (let ((thingnext (pop block)))
                 (while (stringp thingnext)
                   (define-key map (kbd thingnext) thing)
                   (setq thingnext (pop block)))
                 (if (eq thingnext :exitonly)
                     (setq thing (pop block))
                   (progn (put thing 'repeat-map (intern mapname))
                          (setq thing thingnext)))))
             map)))))

(defvar repeaters-maps
  '(("buffer-switch"
     previous-buffer                   "C-x C-<left>" "C-x <left>" "C-<left>" "<left>" "p"
     next-buffer                       "C-x C-<right>" "C-x <right>" "C-<right>" "<right>" "n")

    ("calendar-nav"
     calendar-forward-day              "C-f" "f"
     calendar-backward-day             "C-b" "b"
     calendar-forward-week             "C-n" "n"
     calendar-backward-week            "C-p" "p"
     calendar-forward-month            "M-}" "}" "]"
     calendar-backward-month           "M-{" "{" "["
     calendar-forward-year             "C-x ]"
     calendar-backward-year            "C-x [")

    ("char-line-nav"
     backward-char                     "C-b" "b"
     forward-char                      "C-f" "f"
     next-line                         "C-n" "n"
     previous-line                     "C-p" "p")

    ("defun-nav"
     beginning-of-defun                "C-M-a" "M-a" "a" "ESC M-a"
     end-of-defun                      "C-M-e" "M-e" "e" "ESC M-e")

    ("del-char"
     delete-char                       "C-d" "d")

    ("sexp-nav"
     backward-sexp                     "C-M-b" "b" "ESC M-b"
     forward-sexp                      "C-M-f" "f" "ESC M-f")

    ("paragraph-nav"
     backward-paragraph                "C-<up>" "<up>" "M-{" "M-[" "{" "["
     forward-paragraph                 "C-<down>" "<down>" "M-}" "M-]" "}" "]")

    ("sentence-nav"
     backward-sentence                 "M-a" "a"
     forward-sentence                  "M-e" "e"
     back-to-indentation               "M-m" "m"                     :exitonly)

    ("in-line-nav"
     move-end-of-line                  "C-a" "a"
     move-end-of-line                  "C-e" "e")

    ("page-nav"
     backward-page                     "C-x [" "["
     forward-page                      "C-x ]" "]")

    ("list-nav"
     backward-list                     "C-M-p" "p" "ESC M-p"
     forward-list                      "C-M-n" "n" "ESC M-n"
     backward-up-list                  "C-M-<up>" "C-M-u" "<up>" "u" "ESC M-u"
     down-list                         "C-M-<down>" "C-M-d" "<down>" "d" "ESC M-d")

    ("error-nav"
     next-error                        "C-x `" "`" "M-g M-n" "M-g n" "n"
     previous-error                    "M-g M-p" "M-p" "p")

    ("mid-top-bottom-move"
     recenter-top-bottom               "C-l" "l"
     move-to-window-line-top-bottom    "M-r" "r"
     back-to-indentation               "M-m" "m"                     :exitonly)

    ("fix-case"
     upcase-word                       "M-u" "u"

     ;; Easy way to manually set title case
     downcase-word                     "M-l" "l" "d"
     capitalize-word                   "M-c" "c")

    ("kill-word"
     kill-word                         "M-d" "M-<delete>" "d")

    ("kill-line"
     kill-line                         "C-k" "k")

    ("kill-sentence"
     kill-sentence                     "M-k" "k"
     backward-kill-sentence            "C-x DEL" "DEL")

    ("kill-sexp"
     kill-sexp                         "C-M-k" "k" "ESC M-k")

    ;; Yank same text repeatedly with “C-y y y y”...
    ("yank-only"
     yank                              "C-y" "y"
     yank-pop                          "M-y" "n"                     :exitonly)

    ;; Cycle through the kill-ring with “C-y n n n”...
    ;; You can reverse direction too “C-y n n C-- n n”
    ("yank-popping"
     yank-pop                          "M-y" "y" "n")

    ("kmacro-cycle"
     kmacro-cycle-ring-next            "C-x C-k C-n" "C-n" "n"
     kmacro-cycle-ring-previous        "C-x C-k C-p" "C-p" "p")

    ("tab-bar-nav"
     tab-next                          "C-x t o" "o" "n"
     tab-previous                      "C-x t O" "O" "p")

    ("transpose-chars"
     transpose-chars                    "C-t" "t")

    ("transpose-words"
     transpose-words                   "M-t" "t")

    ("transpose-sexps"
     transpose-sexps                   "C-M-t" "t" "ESC M-t")

    ("transpose-lines"
     transpose-lines                   "C-x C-t" "t")

    ;; M-< for beginning-of-buffer brings up this map, since you can
    ;; only scroll a buffer up when at its beginning.
    ("scroll-up"
     scroll-up-command                 "C-v" "v"
     beginning-of-buffer               "M-<" "<"
     end-of-buffer                     "M->" ">"                     :exitonly
     scroll-down-command               "M-v"                         :exitonly)

    ;; M-> for end-of buffer brings up this map, since you can only
    ;; scroll a buffer down when at its end.
    ("scroll-down"
     scroll-down-command               "M-v" "v"
     end-of-buffer                     "M->" ">"
     beginning-of-buffer               "M-<" "<"                     :exitonly
     scroll-up-command                 "C-v"                         :exitonly)

    ("scroll-otherwin"
     scroll-other-window               "C-M-v" "v" "ESC M-v"
     beginning-of-buffer-other-window  "M-<home>" "<"
     end-of-buffer-other-window        "M-<end>" ">"                 :exitonly
     scroll-other-window-down          "C-M-S-v" "M-v" "ESC M-V" "V" :exitonly)

    ("scroll-otherwin-down"
     scroll-other-window-down          "C-M-S-v" "M-v" "v" "ESC M-V" "V"
     end-of-buffer-other-window        "M-<end>" ">"
     beginning-of-buffer-other-window  "M-<home>" "<"                :exitonly
     scroll-other-window               "C-M-v" "C-v" "ESC M-v"       :exitonly)

    ("scroll-sideways"
     scroll-left                       "C-x <" "<"
     scroll-right                      "C-x >" ">")

    ("hippie-exp"
     ;; For navigating through expansion candidates. You can revert
     ;; to the original string by prefixing the next hippie-expand
     ;; invocation with universal-argument (“C-u /”).
     hippie-expand                     "M-/" "/")

    ("search-nav"
     isearch-repeat-forward            "C-s" "s" "C-M-s" "ESC M-s"
     isearch-repeat-backward           "C-r" "r" "C-M-r" "ESC M-r"
     isearch-exit                      "<enter>" "<return>" "RET"    :exitonly)

    ("undo-only-redo"
     undo-only                         "C-x u" "C-_" "_" "C-/" "/"
     undo-redo                         "C-?" "?" "r")

    ;; Repeat Maps for Org-Mode
    ("org-nav"
     org-backward-heading-same-level   "C-c C-b" "C-b" "b"
     org-forward-heading-same-level    "C-c C-f" "C-f" "f"
     org-previous-visible-heading      "C-c C-p" "C-p" "p"
     org-next-visible-heading          "C-c C-n" "C-n" "n"
     outline-up-heading                "C-c C-u" "C-u" "u")

    ("org-editing"
     org-metadown                      "M-<down>" "<down>"
     org-metaup                        "M-<up>" "<up>"
     org-demote-subtree                "C->" ">"
     org-promote-subtree               "C-<" "<")

    ("org-task"
     org-todo                          "C-c C-t" "C-t" "t"
     org-priority                      "C-c ," ","
     org-time-stamp                    "C-c ." "."
     org-schedule                      "C-c C-s" "C-s" "s"
     org-deadline                      "C-c C-d" "C-d" "d")

    ("word-nav"
     backward-word                     "M-b" "b"
     forward-word                      "M-f" "f")

    ("set-mark"
     smart-region                      "C-SPC" "SPC"))

  "List of lists containing repeater-map definitions.
This must be in the form required by the
‘repeaters-define-maps’ function.")

(repeaters-define-maps repeaters-maps)
(setq repeat-exit-key "g"
      repeat-exit-timeout 30)
(repeat-mode)

;; God mode
;; (use-package god-mode
;;   :custom
;;   (god-mode-alist
;;    '((nil . "C-")
;;      ("." . "M-")
;;      (">" . "C-M-")))
;;   (god-exempt-major-modes nil) ; for god-mode all
;;   (god-exempt-predicates nil)  ; for god-mode-all
;;   :config
;;   (defun my-god-mode-update-mode-line ()
;;     (cond
;;      (god-local-mode
;;       (set-face-attribute 'mode-line nil
;; 			  :foreground "#141404"
;; 			  :background "#ed8f23")
;;       ;; (set-face-attribute 'mode-line-inactive nil
;;       ;;                     :foreground "#141404"
;;       ;;                     :background "#ed9063"))
;;       (set-face-attribute 'mode-line-inactive nil
;; 			  :foreground "#ffffff"
;; 			  :background "#5e3608"))
;;      (t
;;       ;; (set-face-attribute 'mode-line nil
;;       ;; 			  :foreground "#141404"
;;       ;; 			  :background "#cccccc")
;;       ;; (set-face-attribute 'mode-line-inactive nil
;;       ;; 			  :foreground "#141404"
;;       ;; 			  :background "#ffffff")
;;       (set-face-attribute 'mode-line nil
;; 			  :foreground "#ffffff"
;; 			  :background "#4d4d4d")
;;       (set-face-attribute 'mode-line-inactive nil
;; 			  :foreground "#ffffff"
;; 			  :background "#1a1a1a")
;;       )))
;;   (add-hook 'post-command-hook 'my-god-mode-update-mode-line)

;;   (global-set-key (kbd "<escape>") #'god-mode-all)
;;   (global-set-key (kbd "C-x C-0") #'delete-window)
;;   (global-set-key (kbd "C-x C-1") #'delete-other-windows)
;;   (global-set-key (kbd "C-x C-2") #'split-and-follow-below)
;;   (global-set-key (kbd "C-x C-3") #'split-and-follow-right)
;;   (define-key god-local-mode-map (kbd "z") #'repeat))

;; Meow
(use-package meow
  :disabled t
  :custom
  (meow-keypad-meta-prefix 0)
  (meow-keypad-ctrl-meta-prefix 0)
  (meow-keypad-start-keys '())
  (meow-expand-hint-counts '((word . 10)
			     (line . 10)
			     (block . 10)
			     (find . 10)
			     (till . 10)))
  :config
  (defconst meow-cheatsheet-layout-dvorak-emacs
    '((<TLDE> "`"	"~")
      (<AE01> "9"	"!")
      (<AE02> "7"	"@")
      (<AE03> "1"	"#")
      (<AE04> "3"	"$")
      (<AE05> "5"	"%")
      (<AE06> "4"	"^")
      (<AE07> "2"	"&")
      (<AE08> "0"	"*")
      (<AE09> "6"	"[")
      (<AE10> "8"	"]")
      (<AE11> "/"	"?")
      (<AE12> "="	"+")
      (<AD01> "'"	"\"")
      (<AD02> ","	"<")
      (<AD03> "."	">")
      (<AD04> "u"	"U")
      (<AD05> "y"	"Y")
      (<AD06> "c"	"C")
      (<AD07> "d"	"D")
      (<AD08> "p"	"P")
      (<AD09> "r"	"R")
      (<AD10> "l"	"L")
      (<AD11> "("	"{")
      (<AD12> ")"	"}")
      (<AC01> "a"	"A")
      (<AC02> "o"	"O")
      (<AC03> "e"	"E")
      (<AC04> "h"	"H")
      (<AC05> "i"	"I")
      (<AC06> "t"	"T")
      (<AC07> "b"	"B")
      (<AC08> "n"	"N")
      (<AC09> "F"	"F")
      (<AC10> "s"	"S")
      (<AC11> "-"	"_")
      (<AB01> ";"	":")
      (<AB02> "q"	"Q")
      (<AB03> "j"	"J")
      (<AB04> "k"	"K")
      (<AB05> "x"	"X")
      (<AB06> "g"	"G")
      (<AB07> "m"	"M")
      (<AB08> "w"	"W")
      (<AB09> "v"	"V")
      (<AB10> "z"	"Z")
      (<BKSP> "\\"	"|")))

  (defun meow-C-x ()
    "Pulls up the meow keypad with C-x already pressed."
    (interactive)
    (meow-keypad-start-with "C-x"))

  (defun meow-C-h ()
    "Pulls up the meow keypad with C-h already pressed."
    (interactive)
    (meow-keypad-start-with "C-h"))

  (defun meow-C-c ()
    "Pulls up the meow keypad with C-c already pressed."
    (interactive)
    (meow-keypad-start-with "C-c"))

  (defun meow-eshell-toggle ()
    "Toggle eshell popup."
    (interactive)
    (meow--cancel-selection)
    (meow--execute-kbd-macro "C-`"))

  (defun meow-keypad-meta ()
    "Enter keypad state with meta already pressed."
    (interactive)
    (setq this-command last-command)
    (setq meow--keypad-previous-state (meow--current-state))
    (meow--switch-state 'keypad)
    (setq overriding-local-map meow-keypad-state-keymap
          overriding-terminal-local-map nil
	  meow--use-meta t)
    (meow--keypad-display-message))

  (defun meow-keypad-ctrl-meta ()
    "Enter keypad state with control and meta already pressed."
    (interactive)
    (setq this-command last-command)
    (setq meow--keypad-previous-state (meow--current-state))
    (meow--switch-state 'keypad)
    (setq overriding-local-map meow-keypad-state-keymap
          overriding-terminal-local-map nil
	  meow--use-both t)
    (meow--keypad-display-message))

  (defvar meow--kbd-fold "C-="
    "KBD macro for command `origami-toggle-node'.")

  (defvar meow--kbd-cycle "C-+"
    "KBD macro for command `origami-recursively-toggle-node'.")

  (defun meow-fold ()
    "Collapse the code block under the point."
    (interactive)
    (meow--execute-kbd-macro meow--kbd-fold))

  (defun meow-cycle ()
    "Cycle through collapsing every code block."
    (interactive)
    (meow--execute-kbd-macro meow--kbd-cycle))

  (defvar meow--kbd-redo "C-?"
    "KBD macro for command `redo'.")

  (defun meow-undo-dwim ()
    "If a selection is active, undoes on the selection.
Else, undoes on the buffer."
    (interactive)
    (meow--execute-kbd-macro meow--kbd-undo))

  (defun meow-redo-dwim ()
    "If a selection is active, redoes on the selection.
Else, redoes on the buffer."
    (interactive)
    (meow--execute-kbd-macro meow--kbd-redo))

  (defun meow-change-dwim ()
    "If there is no selection, then `meow-append', else `meow-change'."
    (interactive)
    (if (region-active-p)
	(meow-change)
      (meow-insert)))

  (defun meow-activate-select ()
    "Activate char selection, then move left."
    (interactive)
    (if (region-active-p)
	(thread-first
          (meow--make-selection '(expand . char) (mark) (point))
          (meow--select))
      (thread-first
	(meow--make-selection '(expand . char) (point) (point))
	(meow--select))))

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak-emacs)
  ;; (setq meow-cursor-type-beacon '(bar . 2))
  ;; (setq meow-cursor-type-insert '(bar . 2))
  ;; (setq meow-cursor-type-normal '(bar . 2))
  ;; (setq meow-cursor-type-default '(bar . 2))
  ;; (setq meow-cursor-type-motion '(bar . 2))
  ;; (setq meow-cursor-type-keypad '(bar . 2))
  ;; (setq meow-cursor-type-region-cursor '(bar . 2))

  ;; What is a good ergonomic command layout?
  ;; - Keys that are often used together, should not be arranged on same fingers, or in two non-adjacent rows of one hand.
  ;; - Keys that are often pressed continuously, should not be arranged for little fingers
  ;; - Balance the frequency of use of left and right hand
  ;; - Easy to remember

  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . crux-move-beginning-of-line)
   '("A" . meow-open-above)
   '("b" . meow-left)
   '("B" . meow-left-expand)
   '("c" . meow-C-c)
   '("C" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . end-of-line)
   '("E" . meow-open-below)
   '("f" . meow-right)
   '("F" . meow-right-expand)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-C-h)
   '("i" . meow-save)
   '("I" . meow-sync-grab)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-back-word)
   '("M" . meow-back-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("S" . meow-visit)
   '("t" . meow-till)
   '("T" . meow-find)
   '("u" . meow-undo-dwim)
   '("U" . meow-redo-dwim)
   '("v" . meow-next-word)
   '("V" . meow-next-symbol)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-C-x)
   '("X" . meow-M-x)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   '("z" . meow-pop-selection)
   ;; '("'" . repeat)
   '("SPC" . meow-activate-select)
   '("'" . meow-keypad-meta)
   '("\"" . meow-keypad-ctrl-meta)
   '("<escape>" . meow-insert)
   '("C-<escape>" . meow-append)
   '("#" . meow-comment)
   '("(" . meow-backward-slurp)
   '(")" . meow-forward-slurp)
   '("{" . meow-backward-barf)
   '("}" . meow-forward-barf)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("`" . meow-eshell-toggle)
   '("=" . meow-fold)
   '("+" . meow-cycle))

  ;; use << and >> to select to bol/eol
  (add-to-list 'meow-char-thing-table '(?> . line))
  (add-to-list 'meow-char-thing-table '(?< . line))

  ;; start `srefactor-ui-menu-mode' in motion mode
  (add-to-list 'meow-mode-state-list '(srefactor-ui-menu-mode . motion))

  (global-set-key (kbd "C-x C-0") #'delete-window)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-and-follow-below)
  (global-set-key (kbd "C-x C-3") #'split-and-follow-right)

  ;; (meow-global-mode 1)
  )

;;; Cory's Custom Emacs Keybinds

(delete-selection-mode 1)

;; These keybinds were inspired by meow. Most of these functions come from meow
;; and have been modified to fit this config.
;; All credit goes to the creators of meow for inspiration and these functions.

;; Joins all selected lines into one line
;; (global-set-key (kbd "M-j")
;; 		(lambda ()
;;                   (interactive)
;;                   (join-line -1)))

(defun cory/next-line-expand ()
  (interactive)
  (unless (region-active-p)
    (set-mark-command nil))
  (next-line))

(defun cory/previous-line-expand ()
  (interactive)
  (unless (region-active-p)
    (set-mark-command nil))
  (previous-line))

(defun cory/backward-char-expand ()
  (interactive)
  (unless (region-active-p)
    (set-mark-command nil))
  (backward-char))

(defun cory/forward-char-expand ()
  (interactive)
  (unless (region-active-p)
    (set-mark-command nil))
  (forward-char))

(defun cory/kill (beg end &optional region)
  "If region is active, then kills the region.
Else if at the end of the line, joins the next line.
Else, kills the rest of the line."
  (interactive (progn
                 (let ((beg (mark))
                       (end (point)))
                   (unless (and beg end)
                     (user-error "The mark is not set now, so there is no region"))
                   (list beg end 'region))))
  (let ((select-enable-clipboard nil))
    (when (not buffer-read-only)
      (if (region-active-p)
	  (progn
	    (when (and (and (region-active-p)
			(<= (mark) (point)))
		     (< (point) (point-max)))
	      (forward-char 1))
	    (kill-region beg end region))
	(if (and (eolp) (not (bolp)))
	    (delete-indentation 1)
	  (kill-line nil))))))

(defun cory/second-sel-set-string (string)
  (cond
   ((second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (goto-char (overlay-start mouse-secondary-overlay))
      (delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
      (insert string)))
   ((markerp mouse-secondary-start)
    (with-current-buffer (marker-buffer mouse-secondary-start)
      (goto-char (marker-position mouse-secondary-start))
      (insert string)))))

(defun cory/second-sel-get-string ()
  (when (second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (buffer-substring-no-properties
       (overlay-start mouse-secondary-overlay)
       (overlay-end mouse-secondary-overlay)))))

(defun cory/second-sel-buffer ()
  (and (overlayp mouse-secondary-overlay)
     (overlay-buffer mouse-secondary-overlay)))

(defun cory/grab ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (if (region-active-p)
      (secondary-selection-from-region)
    (progn
      (delete-overlay mouse-secondary-overlay)
      (setq mouse-secondary-start (make-marker))
      (move-marker mouse-secondary-start (point))))
  (deactivate-mark t))

(defun cory/swap-grab ()
  "Swap region and secondary selection."
  (interactive)
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (region-str (when (region-active-p) (buffer-substring-no-properties rbeg rend)))
         (sel-str (cory/second-sel-get-string))
         (next-marker (make-marker)))
    (when region-str (delete-region rbeg rend))
    (when sel-str (insert sel-str))
    (move-marker next-marker (point))
    (cory/second-sel-set-string (or region-str ""))
    (when (overlayp mouse-secondary-overlay)
      (delete-overlay mouse-secondary-overlay))
    (setq mouse-secondary-start next-marker)
    (deactivate-mark t)))

(defun cory/sync-grab ()
  "Sync secondary selection with current region."
  (interactive)
  (when (region-active-p)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (region-str (buffer-substring-no-properties rbeg rend))
           (next-marker (make-marker)))
      (move-marker next-marker (point))
      (cory/second-sel-set-string region-str)
      (when (overlayp mouse-secondary-overlay)
	(delete-overlay mouse-secondary-overlay))
      (setq mouse-secondary-start next-marker)
      (deactivate-mark t))))

(defun cory/make-selection (type mark pos &optional expand)
  "Make a selection with TYPE, MARK and POS.

The direction of selection is MARK -> POS."
  (if (and (region-active-p) expand)
      (let ((orig-mark (mark))
            (orig-pos (point)))
        (if (< mark pos)
            (list type (min orig-mark orig-pos) pos)
          (list type (max orig-mark orig-pos) pos)))
    (list type mark pos)))

(defun cory/select (selection &optional backward)
  "Mark the SELECTION."
  (let ((sel-type (car selection))
        (mark (cadr selection))
        (pos (caddr selection)))
    (goto-char (if backward mark pos))
    (if (not sel-type)
	(progn
          (deactivate-mark)
          (message "No previous selection.")
          (deactivate-mark t))
      (push-mark (if backward pos mark) t t))))

(defun cory/join-forward ()
  (let (mark pos)
    (save-mark-and-excursion
      (goto-char (line-end-position))
      (setq pos (point))
      (when (re-search-forward "[[:space:]\n\r]*" nil t)
        (setq mark (point))))
    (when pos
      (thread-first
        (cory/make-selection '(expand . join) pos mark)
        (cory/select)))))

(defun cory/join-backward ()
  (let* (mark
         pos)
    (save-mark-and-excursion
      (back-to-indentation)
      (setq pos (point))
      (goto-char (line-beginning-position))
      (while (looking-back "[[:space:]\n\r]" 1 t)
        (forward-char -1))
      (setq mark (point)))
    (thread-first
      (cory/make-selection '(expand . join) mark pos)
      (cory/select))))

(defun cory/join-both ()
  (let* (mark
         pos)
    (save-mark-and-excursion
      (while (looking-back "[[:space:]\n\r]" 1 t)
        (forward-char -1))
      (setq mark (point)))
    (save-mark-and-excursion
      (while (looking-at "[[:space:]\n\r]")
        (forward-char 1))
      (setq pos (point)))
    (thread-first
      (cory/make-selection '(expand . join) mark pos)
      (cory/select))))

(defun cory/join (arg)
  "Select the indentation between this line to the non empty previous line.

Will create selection with type (select . join)

Prefix:
with NEGATIVE ARGUMENT, forward search indentation to select.
with UNIVERSAL ARGUMENT, search both side."
  (interactive "P")
  (cond
   ((equal '(4) arg)
    (cory/join-both))
   ((< (prefix-numeric-value arg) 0)
    (cory/join-forward))
   (t
    (cory/join-backward))))

(defun cory/line (n &optional expand)
  "Select the current line, eol is not included.

Create selection with type (expand . line).
For the selection with type (expand . line), expand it by line.
For the selection with other types, cancel it.

Prefix:
numeric, repeat times.
"
  (interactive "p")
  (let* ((orig (mark t))
         (n (if (and (region-active-p)
		   (> (mark) (point)))
                (- n)
	      n))
         (forward (> n 0)))
    (cond
     ((region-active-p)
      (let (p)
        (save-mark-and-excursion
          (forward-line n)
          (goto-char
           (if forward
               (setq p (line-end-position))
             (setq p (line-beginning-position)))))
        (thread-first
          (cory/make-selection '(expand . line) orig p expand)
          (cory/select))))
     (t
      (let ((m (if forward
                   (line-beginning-position)
                 (line-end-position)))
            (p (save-mark-and-excursion
                 (if forward
                     (progn
                       (forward-line (1- n))
                       (line-end-position))
                   (progn
                     (forward-line (1+ n))
                     (when (string-match-p
			    "^ *$" (buffer-substring-no-properties
				    (line-beginning-position)
				    (line-end-position)))
		       (backward-char 1))
                     (line-beginning-position))))))
        (thread-first
          (cory/make-selection '(expand . line) m p expand)
          (cory/select)))))))

(defun cory/goto-line ()
  "Goto line, recenter and select that line.

This command will expand line selection."
  (interactive)
  (consult-goto-line)
  (recenter))

(defun cory/push-search (search)
  (unless (string-equal search (car regexp-search-ring))
    (add-to-history 'regexp-search-ring search regexp-search-ring-max)))

(defun cory/search (arg)
  " Search and select with the car of current `regexp-search-ring'.

If the contents of selection doesn't match the regexp, will push it to `regexp-search-ring' before searching.

To search backward, use \\[negative-argument]."
  (interactive "P")
  ;; Test if we add current region as search target.
  (when (and (region-active-p)
           (let ((search (car regexp-search-ring)))
             (or (not search)
                (not (string-match-p
                    (format "^%s$" search)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))))
    (cory/push-search (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))))
  (when-let ((search (car regexp-search-ring)))
    (let ((reverse (xor (< (prefix-numeric-value arg) 0) (and (region-active-p)
							    (> (mark) (point)))))
          (case-fold-search nil))
      (if (or (if reverse
                 (re-search-backward search nil t 1)
               (re-search-forward search nil t 1))
             ;; Try research from buffer beginning/end
             ;; if we are already at the last/first matched
             (save-mark-and-excursion
               ;; Recalculate search indicator
               (goto-char (if reverse (point-max) (point-min)))
               (if reverse
                   (re-search-backward search nil t 1)
                 (re-search-forward search nil t 1))))
          (let* ((m (match-data))
                 (marker-beg (car m))
                 (marker-end (cadr m))
                 (beg (if reverse (marker-position marker-end) (marker-position marker-beg)))
                 (end (if reverse (marker-position marker-beg) (marker-position marker-end))))
            (thread-first
              (cory/make-selection '(select . visit) beg end)
              (cory/select))
            (if reverse
                (message "Reverse search: %s" search)
              (message "Search: %s" search))
            (let ((overlays (overlays-at (1- (point))))
		  ov expose)
	      (while (setq ov (pop overlays))
		(if (and (invisible-p (overlay-get ov 'invisible))
		       (setq expose (overlay-get ov 'isearch-open-invisible)))
		    (funcall expose ov)))))
        (message "Searching %s failed" search)))))

(defun cory/visit-point (text reverse)
  "Return the point of text for visit command.
Argument TEXT current search text.
Argument REVERSE if selection is reversed."
  (let ((func (if reverse #'re-search-backward #'re-search-forward))
        (func-2 (if reverse #'re-search-forward #'re-search-backward))
        (case-fold-search nil))
    (save-mark-and-excursion
      (or (funcall func text nil t 1)
         (funcall func-2 text nil t 1)))))

(defun cory/prompt-symbol-and-words (prompt beg end)
  "Completion with PROMPT for symbols and words from BEG to END."
  (let ((completions))
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+\\_>" end t)
        (let ((result (match-string-no-properties 0)))
          (when (>= (length result) 1)
            (push (cons result (format "\\_<%s\\_>" (regexp-quote result))) completions)))))
    (setq completions (delete-dups completions))
    (let ((selected (completing-read prompt completions nil nil)))
      (or (cdr (assoc selected completions))
         (regexp-quote selected)))))

(defun cory/visit (arg)
  "Read a regexp from minibuffer, then search and select it.

The input will be pushed into `regexp-search-ring'.  So
\\[meow-search] can be used for further searching with the same condition.

A list of occurred regexps will be provided for completion, the regexps will
be sanitized by default. To display them in raw format, set
`meow-visit-sanitize-completion' to nil.

To search backward, use \\[negative-argument]."
  (interactive "P")
  (let* ((reverse arg)
         (pos (point))
         (text (cory/prompt-symbol-and-words
                (if arg "Visit backward: " "Visit: ")
                (point-min) (point-max)))
         (cory/visit-point (cory/visit-point text reverse)))
    (if cory/visit-point
        (let* ((m (match-data))
               (marker-beg (car m))
               (marker-end (cadr m))
               (beg (if (> pos cory/visit-point) (marker-position marker-end) (marker-position marker-beg)))
               (end (if (> pos cory/visit-point) (marker-position marker-beg) (marker-position marker-end))))
          (thread-first
            (cory/make-selection '(select . visit) beg end)
            (cory/select))
          (cory/push-search text)
	  (let ((overlays (overlays-at (1- (point))))
		ov expose)
	    (while (setq ov (pop overlays))
	      (if (and (invisible-p (overlay-get ov 'invisible))
		     (setq expose (overlay-get ov 'isearch-open-invisible)))
		  (funcall expose ov)))))
      (message "Visit: %s failed" text))))

(defun cory/mark-word (n)
  "Mark current word under cursor.

A expandable word selection will be created. `meow-next-word' and
`meow-back-word' can be used for expanding.

The content of selection will be quoted to regexp, then pushed into
`regexp-search-ring' which be read by `meow-search' and other commands.

This command will also provide highlighting for same occurs.

Use negative argument to create a backward selection."
  (interactive "p")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (cory/make-selection '(expand . word) beg end)
        (cory/select (< n 0)))
      (let ((search (format "\\<%s\\>" (regexp-quote (buffer-substring-no-properties beg end)))))
        (cory/push-search search)))))

(defun cory/mark-symbol (n)
  "Mark current symbol under cursor.

This command works similar to `cory/mark-word'."
  (interactive "p")
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (cory/make-selection '(expand . word) beg end)
        (cory/select (< n 0)))
      (let ((search (format "\\_<%s\\_>" (regexp-quote (buffer-substring-no-properties beg end)))))
        (cory/push-search search)))))

(defun cory/backward-symbol (arg)
  (interactive "p")
  (forward-symbol
   (if arg (* -1 arg) -1)))

(defun cory/block (arg)
  "Mark the block or expand to parent block."
  (interactive "P")
  (let ((ra (region-active-p))
        (back (xor (and (region-active-p)
		      (> (mark) (point)))
		   (< (prefix-numeric-value arg) 0)))
        (depth (car (syntax-ppss)))
        (orig-pos (point))
        p m)
    (save-mark-and-excursion
      (while (and (if back (re-search-backward "\\s(" nil t) (re-search-forward "\\s)" nil t))
                (or (save-mark-and-excursion
		     (nth 3 (syntax-ppss)))
                   (if ra (>= (car (syntax-ppss)) depth) (> (car (syntax-ppss)) depth)))))
      (when (and (if ra (< (car (syntax-ppss)) depth) (<= (car (syntax-ppss)) depth))
               (not (= (point) orig-pos)))
        (setq p (point))
        (when (ignore-errors (forward-list (if back 1 -1)))
          (setq m (point)))))
    (when (and p m)
      (thread-first
        (cory/make-selection '(expand . block) m p)
        (cory/select)))))

(defun cory/to-block (arg)
  "Expand to next block.

Will create selection with type (expand . block)."
  (interactive "P")
  ;; We respect the direction of block selection.
  (let ((back (or (and (region-active-p)
		    (> (mark) (point)))
		 (< (prefix-numeric-value arg) 0)))
        (depth (car (syntax-ppss)))
        (orig-pos (point))
        p m)
    (save-mark-and-excursion
      (while (and (if back (re-search-backward "\\s(" nil t) (re-search-forward "\\s)" nil t))
                (or (save-mark-and-excursion
		     (nth 3 (syntax-ppss)))
                   (> (car (syntax-ppss)) depth))))
      (when (and (= (car (syntax-ppss)) depth)
               (not (= (point) orig-pos)))
        (setq p (point))
        (when (ignore-errors (forward-list (if back 1 -1)))
          (setq m (point)))))
    (when (and p m)
      (thread-first
        (cory/make-selection '(expand . block) orig-pos p t)
        (cory/select)))))

(defun cory/quit-window-or-buffer ()
  "Quit current window or buffer."
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
      (delete-window)
    (previous-buffer)))

(defun cory/replace-selection ()
  "Replace current selection with yank.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (when (region-active-p)
    (let ((select-enable-clipboard nil))
      (when (not buffer-read-only)
	(when-let ((s (string-trim-right (current-kill 0 t) "\n")))
          (delete-region (region-beginning) (region-end))
          (insert s))))))

(defvar cory/char-thing-table
  '((?r . round)
    (?s . square)
    (?c . curly)
    (?g . string)
    (?e . symbol)
    (?w . window)
    (?b . buffer)
    (?p . paragraph)
    (?l . line)
    (?d . defun)
    (?. . sentence)))

(defvar cory/selection-directions
  '((inner . forward)
    (bounds . backward)
    (beginning . backward)
    (end . forward)))

(defun cory/thing-prompt (prompt-text)
  (read-char prompt-text))

(defun cory/thing-get-direction (cmd)
  (or
   (alist-get cmd cory/selection-directions)
   'forward))

(defun cory/bounds-of-symbol ()
  (when-let (bounds (bounds-of-thing-at-point 'symbol))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (save-mark-and-excursion
        (goto-char end)
        (if (not (looking-at-p "\\s)"))
            (while (looking-at-p " \\|,")
              (goto-char (cl-incf end)))
          (goto-char beg)
          (while (looking-back " \\|," 1)
            (goto-char (cl-decf beg))))
        (cons beg end)))))

(defun cory/bounds-of-string-1 ()
  "Return the bounds of the string under the cursor.

The thing `string' is not available in Emacs 27.'"
  (if (version< emacs-version "28")
      (when (cory/in-string-p)
        (let (beg end)
          (save-mark-and-excursion
            (while (cory/in-string-p)
              (backward-char 1))
            (setq beg (point)))
          (save-mark-and-excursion
            (while (cory/in-string-p)
              (forward-char 1))
            (setq end (point)))
          (cons beg end)))
    (bounds-of-thing-at-point 'string)))

(defun cory/inner-of-symbol ()
  (bounds-of-thing-at-point 'symbol))

(defun cory/bounds-of-string (&optional inner)
  (when-let (bounds (cory/bounds-of-string-1))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (cons
       (save-mark-and-excursion
         (goto-char beg)
         (funcall (if inner #'skip-syntax-forward #'skip-syntax-backward) "\"|")
         (point))
       (save-mark-and-excursion
         (goto-char end)
         (funcall (if inner #'skip-syntax-backward #'skip-syntax-forward) "\"|")
         (point))))))

(defun cory/inner-of-string ()
  (cory/bounds-of-string t))

(defun cory/inner-of-window ()
  (cons (window-start) (window-end)))

(defun cory/inner-of-line ()
  (cons (save-mark-and-excursion (back-to-indentation) (point))
        (line-end-position)))

;;; Registry

(defvar cory/thing-registry nil
  "Thing registry.

This is a plist mapping from thing to (inner-fn . bounds-fn).
Both inner-fn and bounds-fn returns a cons of (start . end) for that thing.")

(defun cory/thing-register-helper (thing inner-fn bounds-fn)
  "Register INNER-FN and BOUNDS-FN to a THING."
  (setq cory/thing-registry
        (plist-put cory/thing-registry
                   thing
                   (cons inner-fn bounds-fn))))

(defun cory/thing-syntax-function (syntax)
  (cons
   (save-mark-and-excursion
     (when (use-region-p)
       (goto-char (region-beginning)))
     (skip-syntax-backward (cdr syntax))
     (point))
   (save-mark-and-excursion
     (when (use-region-p)
       (goto-char (region-end)))
     (skip-syntax-forward (cdr syntax))
     (point))))

(defun cory/thing-regexp-function (b-re f-re near)
  (let ((beg (save-mark-and-excursion
               (when (use-region-p)
                 (goto-char (region-beginning)))
               (when (re-search-backward b-re nil t)
                 (if near (match-end 0) (point)))))
        (end (save-mark-and-excursion
               (when (use-region-p)
                 (goto-char (region-end)))
               (when (re-search-forward f-re nil t)
                 (if near (match-beginning 0) (point))))))
    (when (and beg end)
      (cons beg end))))

(defun cory/thing-parse-pair-search (push-token pop-token back near)
  (let* ((search-fn (if back #'re-search-backward #'re-search-forward))
         (match-fn (if back #'match-end #'match-beginning))
         (cmp-fn (if back #'> #'<))
         (push-next-pos nil)
         (pop-next-pos nil)
         (push-pos (save-mark-and-excursion
                     (when (funcall search-fn push-token nil t)
                       (setq push-next-pos (point))
                       (if near (funcall match-fn 0) (point)))))
         (pop-pos (save-mark-and-excursion
                    (when (funcall search-fn pop-token nil t)
                      (setq pop-next-pos (point))
                      (if near (funcall match-fn 0) (point))))))
    (cond
     ((and (not pop-pos) (not push-pos))
      nil)
     ((not pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     ((not push-pos)
      (goto-char pop-next-pos)
      (cons 'pop pop-pos))
     ((funcall cmp-fn push-pos pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     (t
      (goto-char pop-next-pos)
      (cons 'pop pop-pos)))))

(defun cory/thing-pair-function (push-token pop-token near)
  (let* ((found nil)
         (depth  0)
         (beg (save-mark-and-excursion
                (prog1
                    (let ((case-fold-search nil))
                      (while (and (<= depth 0)
                                  (setq found (cory/thing-parse-pair-search push-token pop-token t near)))
                        (let ((push-or-pop (car found)))
                          (if (eq 'push push-or-pop)
                              (cl-incf depth)
                            (cl-decf depth))))
                      (when (> depth 0) (cdr found)))
                  (setq depth 0
                        found nil))))
         (end (save-mark-and-excursion
                (let ((case-fold-search nil))
                  (while (and (>= depth 0)
                              (setq found (cory/thing-parse-pair-search push-token pop-token nil near)))
                    (let ((push-or-pop (car found)))
                      (if (eq 'push push-or-pop)
                          (cl-incf depth)
                        (cl-decf depth))))
                  (when (< depth 0) (cdr found))))))
    (when (and beg end)
      (cons beg end))))


(defun cory/thing-make-syntax-function (x)
  (lambda () (cory/thing-syntax-function x)))

(defun cory/thing-make-regexp-function (x near)
  (let* ((b-re (cadr x))
         (f-re (caddr x)))
    (lambda () (cory/thing-regexp-function b-re f-re near))))

(defun cory/thing-make-pair-function (x near)
  (let* ((push-token (let ((tokens (cadr x)))
                       (string-join (mapcar #'regexp-quote tokens) "\\|")))
         (pop-token (let ((tokens (caddr x)))
                      (string-join (mapcar #'regexp-quote tokens) "\\|"))))
    (lambda () (cory/thing-pair-function push-token pop-token near))))

(defun cory/thing-parse-multi (xs near)
  (let ((chained-fns (mapcar (lambda (x) (cory/thing-parse x near)) xs)))
    (lambda ()
      (let ((fns chained-fns)
            ret)
        (while (and fns (not ret))
          (setq ret (funcall (car fns))
                fns (cdr fns)))
        ret))))

(defun cory/thing-parse (x near)
  (cond
   ((functionp x)
    x)
   ((symbolp x)
    (lambda () (bounds-of-thing-at-point x)))
   ((equal 'syntax (car x))
    (cory/thing-make-syntax-function x))
   ((equal 'regexp (car x))
    (cory/thing-make-regexp-function x near))
   ((equal 'pair (car x))
    (cory/thing-make-pair-function x near))
   ((listp x)
    (cory/thing-parse-multi x near))
   (t
    (lambda ()
      (message "THING definition broken")
      (cons (point) (point))))))

(defun cory/thing-register (thing inner bounds)
  "Register a THING with INNER and BOUNDS.

Argument THING should be symbol, which specified in `cory/char-thing-table'.
Argument INNER and BOUNDS support following expressions:

  EXPR ::= FUNCTION | SYMBOL | SYNTAX-EXPR | REGEXP-EXPR
         | PAIRED-EXPR | MULTI-EXPR
  SYNTAX-EXPR ::= (syntax . STRING)
  REGEXP-EXPR ::= (regexp STRING STRING)
  PAIRED-EXPR ::= (pair TOKENS TOKENS)
  MULTI-EXPR ::= (EXPR ...)
  TOKENS ::= (STRING ...)

FUNCTION is a function receives no arguments, return a cons which
  the car is the beginning of thing, and the cdr is the end of
  thing.

SYMBOL is a symbol represent a builtin thing.

  Example: url

    (cory/thing-register 'url 'url 'url)

SYNTAX-EXPR contains a syntax description used by `skip-syntax-forward'

  Example: non-whitespaces

    (cory/thing-register 'non-whitespace
                         '(syntax . \"^-\")
                         '(syntax . \"^-\"))

  You can find the description for syntax in current buffer with
  \\[describe-syntax].

REGEXP-EXPR contains two regexps, the first is used for
  beginning, the second is used for end. For inner/beginning/end
  function, the point of near end of match will be used.  For
  bounds function, the point of far end of match will be used.

  Example: quoted

    (cory/thing-register 'quoted
                         '(regexp \"`\" \"`\\\\|'\")
                         '(regexp \"`\" \"`\\\\|'\"))

PAIR-EXPR contains two string token lists. The tokens in first
  list are used for finding beginning, the tokens in second list
  are used for finding end.  A depth variable will be used while
  searching, thus only matched pair will be found.

  Example: do/end block

    (cory/thing-register 'do/end
                         '(pair (\"do\") (\"end\"))
                         '(pair (\"do\") (\"end\")))"
  (let ((inner-fn (cory/thing-parse inner t))
        (bounds-fn (cory/thing-parse bounds nil)))
    (cory/thing-register-helper thing inner-fn bounds-fn)))

(cory/thing-register 'round
                     '(pair ("(") (")"))
                     '(pair ("(") (")")))

(cory/thing-register 'square
                     '(pair ("[") ("]"))
                     '(pair ("[") ("]")))

(cory/thing-register 'curly
                     '(pair ("{") ("}"))
                     '(pair ("{") ("}")))

(cory/thing-register 'paragraph 'paragraph 'paragraph)

(cory/thing-register 'sentence 'sentence 'sentence)

(cory/thing-register 'buffer 'buffer 'buffer)

(cory/thing-register 'defun 'defun 'defun)

(cory/thing-register 'symbol #'cory/inner-of-symbol #'cory/bounds-of-symbol)

(cory/thing-register 'string #'cory/inner-of-string #'cory/bounds-of-string)

(cory/thing-register 'window #'cory/inner-of-window #'cory/inner-of-window)

(cory/thing-register 'line #'cory/inner-of-line 'line)

(defun cory/parse-inner-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch cory/char-thing-table)))
    (cory/parse-range-of-thing (cdr ch-to-thing) t)))

(defun cory/parse-bounds-of-thing-char (ch)
  (when-let ((ch-to-thing (assoc ch cory/char-thing-table)))
    (cory/parse-range-of-thing (cdr ch-to-thing) nil)))

(defun cory/parse-range-of-thing (thing inner)
  "Parse either inner or bounds of THING. If INNER is non-nil then parse inner."
  (when-let (bounds-fn-pair (plist-get cory/thing-registry thing))
    (if inner
        (funcall (car bounds-fn-pair))
      (funcall (cdr bounds-fn-pair)))))

(defun cory/beginning-of-thing (thing)
  "Select to the beginning of THING."
  (interactive (list (cory/thing-prompt "Beginning of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (cory/thing-get-direction 'beginning)))
          (bounds (cory/parse-inner-of-thing-char thing)))
      (when bounds
        (thread-first
          (cory/make-selection '(select . transient)
                               (if back (point) (car bounds))
                               (if back (car bounds) (point)))
          (cory/select))))))

(defun cory/end-of-thing (thing)
  "Select to the end of THING."
  (interactive (list (cory/thing-prompt "End of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (cory/thing-get-direction 'end)))
          (bounds (cory/parse-inner-of-thing-char thing)))
      (when bounds
        (thread-first
          (cory/make-selection '(select . transient)
                               (if back (cdr bounds) (point))
                               (if back (point) (cdr bounds)))
          (cory/select))))))

(defun cory/select-range (back bounds)
  (when bounds
    (thread-first
      (cory/make-selection '(select . transient)
			   (if back (cdr bounds) (car bounds))
			   (if back (car bounds) (cdr bounds)))
      (cory/select))))

(defun cory/inner-of-thing (thing)
  "Select inner (excluding delimiters) of THING."
  (interactive (list (cory/thing-prompt "Inner of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (cory/thing-get-direction 'inner)))
          (bounds (cory/parse-inner-of-thing-char thing)))
      (cory/select-range back bounds))))

(defun cory/bounds-of-thing (thing)
  "Select bounds (including delimiters) of THING."
  (interactive (list (cory/thing-prompt "Bounds of: ")))
  (save-window-excursion
    (let ((back (equal 'backward (cory/thing-get-direction 'bounds)))
          (bounds (cory/parse-bounds-of-thing-char thing)))
      (cory/select-range back bounds))))

(defmacro defun-beginning-of (name char)
  `(defun ,name ()
     (interactive)
     (cory/beginning-of-thing ,char)))

(defmacro defun-end-of (name char)
  `(defun ,name ()
     (interactive)
     (cory/end-of-thing ,char)))

(defmacro defun-inner-of (name char)
  `(defun ,name ()
     (interactive)
     (cory/inner-of-thing ,char)))

(defmacro defun-bounds-of (name char)
  `(defun ,name ()
     (interactive)
     (cory/bounds-of-thing ,char)))

(defun-beginning-of cory/beginning-of-parens    ?r)
(defun-beginning-of cory/beginning-of-brackets  ?s)
(defun-beginning-of cory/beginning-of-braces    ?c)
(defun-beginning-of cory/beginning-of-string    ?g)
(defun-beginning-of cory/beginning-of-symbol    ?e)
(defun-beginning-of cory/beginning-of-window    ?w)
(defun-beginning-of cory/beginning-of-buffer    ?b)
(defun-beginning-of cory/beginning-of-paragraph ?p)
(defun-beginning-of cory/beginning-of-braces    ?l)
(defun-beginning-of cory/beginning-of-defun     ?d)
(defun-beginning-of cory/beginning-of-sentence  ?.)

(defun-end-of cory/end-of-parens    ?r)
(defun-end-of cory/end-of-brackets  ?s)
(defun-end-of cory/end-of-braces    ?c)
(defun-end-of cory/end-of-string    ?g)
(defun-end-of cory/end-of-symbol    ?e)
(defun-end-of cory/end-of-window    ?w)
(defun-end-of cory/end-of-buffer    ?b)
(defun-end-of cory/end-of-paragraph ?p)
(defun-end-of cory/end-of-braces    ?l)
(defun-end-of cory/end-of-defun     ?d)
(defun-end-of cory/end-of-sentence  ?.)

(defun-inner-of cory/inner-of-parens    ?r)
(defun-inner-of cory/inner-of-brackets  ?s)
(defun-inner-of cory/inner-of-braces    ?c)
(defun-inner-of cory/inner-of-string    ?g)
(defun-inner-of cory/inner-of-symbol    ?e)
(defun-inner-of cory/inner-of-window    ?w)
(defun-inner-of cory/inner-of-buffer    ?b)
(defun-inner-of cory/inner-of-paragraph ?p)
(defun-inner-of cory/inner-of-braces    ?l)
(defun-inner-of cory/inner-of-defun     ?d)
(defun-inner-of cory/inner-of-sentence  ?.)

(defun-bounds-of cory/bounds-of-parens    ?r)
(defun-bounds-of cory/bounds-of-brackets  ?s)
(defun-bounds-of cory/bounds-of-braces    ?c)
(defun-bounds-of cory/bounds-of-string    ?g)
(defun-bounds-of cory/bounds-of-symbol    ?e)
(defun-bounds-of cory/bounds-of-window    ?w)
(defun-bounds-of cory/bounds-of-buffer    ?b)
(defun-bounds-of cory/bounds-of-paragraph ?p)
(defun-bounds-of cory/bounds-of-braces    ?l)
(defun-bounds-of cory/bounds-of-defun     ?d)
(defun-bounds-of cory/bounds-of-sentence  ?.)

;; (defun open-line-below ()
;;   (interactive)
;;   (end-of-line)
;;   (newline)
;;   (indent-for-tab-command))

;; (defun open-line-above ()
;;   (interactive)
;;   (beginning-of-line)
;;   (newline)
;;   (forward-line -1)
;;   (indent-for-tab-command))

;; (global-set-key (kbd "<C-return>") 'open-line-below)
;; (global-set-key (kbd "<C-S-return>") 'open-line-above)

;; (defun move-line-down ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines 1))
;;     (forward-line)
;;     (move-to-column col)))

;; (defun move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines -1))
;;     (move-to-column col)))

;; (global-set-key (kbd "<C-S-down>") 'move-line-down)
;; (global-set-key (kbd "<C-S-up>") 'move-line-up)

;; Move more quickly
;; (global-set-key (kbd "C-S-n")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (next-line 5))))

;; (global-set-key (kbd "C-S-p")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (previous-line 5))))

;; (global-set-key (kbd "C-S-f")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (forward-char 5))))

;; (global-set-key (kbd "C-S-b")
;;                 (lambda ()
;;                   (interactive)
;;                   (ignore-errors (backward-char 5))))

;; FIXME Only works when launching a new instance, not in a server.
;; Unbind <C-i> from the TAB key and bind it to indent-region.
;; Since TAB and <C-i> cannot be differentiated in TTY emacs,
;; the workaround is to conditionally bind TAB to indent-region
;; when there is an active region selected.
;; (if (window-system)
;;     (progn
;;       (define-key input-decode-map [?\C-i] [C-i])
;;       (global-set-key (kbd "<C-i>") 'indent-region))
;;   (defun cory/tab-replacement (&optional START END)
;;     (interactive "r")
;;     (if (use-region-p)
;;         (indent-region START END)
;;       (indent-for-tab-command)))
;;   (global-set-key (kbd "TAB") 'cory/tab-replacement))

;; ;; Do the same with <C-m> and RET
;; (if (window-system)
;;     (progn
;;       (define-key input-decode-map [?\C-m] [C-m])
;;       (global-set-key (kbd "<C-m>") 'newline))
;;   (define-key text-mode-map (kbd "RET") 'newline)
;;   (define-key prog-mode-map (kbd "RET") 'newline))

;; (define-key input-decode-map [?\C-m] [C-m])
;; (global-set-key (kbd "<C-m>") 'newline)
;; (define-key input-decode-map [?\C-i] [C-i])
;; (global-set-key (kbd "<C-i>") 'indent-region)

;; (dolist (pair '(("C-;"   exchange-point-and-mark)
;; 		("C-a"   crux-move-beginning-of-line)
;; 		("C-S-a" crux-smart-open-line-above)
;; 		("C-S-b" cory/backward-char-expand)
;; 		("C-d"   delete-char)
;; 		("C-S-d" sp-backward-delete-char)
;; 		("C-e"   move-end-of-line)
;; 		("C-S-e" crux-smart-open-line)
;; 		("C-f"   forward-char)
;; 		("C-S-f" cory/forward-char-expand)
;; 		("C-g"   keyboard-quit)
;; 		("C-S-g" cory/grab)
;; 		("<C-i>" kill-ring-save)
;; 		("C-S-i" cory/sync-grab)
;; 		("C-j"   cory/join)
;; 		("C-k"   cory/kill)
;; 		("C-l"   cory/line)
;; 		("C-S-l" cory/goto-line)
;; 		("<C-m>" backward-word)
;; 		("C-S-m" cory/backward-symbol)
;; 		("C-n"   next-line)
;; 		("C-S-n" cory/next-line-expand)
;; 		("C-o"   cory/block)
;; 		("C-S-o" cory/to-block)
;; 		("C-p"   previous-line)
;; 		("C-S-p" cory/previous-line-expand)
;; 		("C-q"   cory/quit-window-or-buffer)
;; 		("C-S-q" cory/goto-line)
;; 		("C-r"   cory/replace-selection)
;; 		("C-S-r" cory/swap-grab)
;; 		("C-s"   cory/search)
;; 		("C-S-s" cory/visit)
;; 		("C-t"   avy-goto-char)
;; 		("C-S-t" avy-goto-word-0)
;; 		("C-v"   forward-word)
;; 		("C-S-v" forward-symbol)
;; 		("C-w"   cory/mark-word)
;; 		("C-S-w" cory/mark-symbol)
;; 		("C-y"   yank)
;; 		("C-S-y" consult-yank-pop)
;; 		("C-'"   repeat)
;; 		("C-/"   undo-only)
;; 		("C-?"   undo-redo)

;; 		("C-< r" cory/beginning-of-parens)
;; 		("C-< s" cory/beginning-of-brackets)
;; 		("C-< c" cory/beginning-of-braces)
;; 		("C-< g" cory/beginning-of-string)
;; 		("C-< e" cory/beginning-of-symbol)
;; 		("C-< w" cory/beginning-of-window)
;; 		("C-< b" cory/beginning-of-buffer)
;; 		("C-< p" cory/beginning-of-paragraph)
;; 		("C-< l" cory/beginning-of-braces)
;; 		("C-< d" cory/beginning-of-defun)
;; 		("C-< ." cory/beginning-of-sentence)

;; 		("C-> r" cory/end-of-parens)
;; 		("C-> s" cory/end-of-brackets)
;; 		("C-> c" cory/end-of-braces)
;; 		("C-> g" cory/end-of-string)
;; 		("C-> e" cory/end-of-symbol)
;; 		("C-> w" cory/end-of-window)
;; 		("C-> b" cory/end-of-buffer)
;; 		("C-> p" cory/end-of-paragraph)
;; 		("C-> l" cory/end-of-braces)
;; 		("C-> d" cory/end-of-defun)
;; 		("C-> ." cory/end-of-sentence)

;; 		("C-, r" cory/inner-of-parens)
;; 		("C-, s" cory/inner-of-brackets)
;; 		("C-, c" cory/inner-of-braces)
;; 		("C-, g" cory/inner-of-string)
;; 		("C-, e" cory/inner-of-symbol)
;; 		("C-, w" cory/inner-of-window)
;; 		("C-, b" cory/inner-of-buffer)
;; 		("C-, p" cory/inner-of-paragraph)
;; 		("C-, l" cory/inner-of-braces)
;; 		("C-, d" cory/inner-of-defun)
;; 		("C-, ." cory/inner-of-sentence)

;; 		("C-. r" cory/bounds-of-parens)
;; 		("C-. s" cory/bounds-of-brackets)
;; 		("C-. c" cory/bounds-of-braces)
;; 		("C-. g" cory/bounds-of-string)
;; 		("C-. e" cory/bounds-of-symbol)
;; 		("C-. w" cory/bounds-of-window)
;; 		("C-. b" cory/bounds-of-buffer)
;; 		("C-. p" cory/bounds-of-paragraph)
;; 		("C-. l" cory/bounds-of-braces)
;; 		("C-. d" cory/bounds-of-defun)
;; 		("C-. ." cory/bounds-of-sentence)))
;;   (global-set-key (kbd (car pair)) (cadr pair)))

;; (add-hook 'ielm-mode-hook
;; 	  (lambda ()
;; 	    (define-key ielm-map (kbd "<return>") 'ielm-return)))

;; (define-key minibuffer-local-map (kbd "<return>") 'exit-minibuffer)

;; (eval-after-load 'help-mode
;;   '(define-key help-mode-map (kbd "<return>") 'help-follow-symbol))

;; (define-key button-map (kbd "<return>") 'push-button)

;;; Misc useful functions

(defun describe-all-keymaps ()
  "Describe all keymaps in currently-defined variables."
  (interactive)
  (with-output-to-temp-buffer "*keymaps*"
    (let (symbs seen)
      (mapatoms (lambda (s)
                  (when (and (boundp s) (keymapp (symbol-value s)))
                    (push (indirect-variable s) symbs))))
      (dolist (keymap symbs)
        (unless (memq keymap seen)
          (princ (format "* %s\n\n" keymap))
          (princ (substitute-command-keys (format "\\{%s}" keymap)))
          (princ (format "\f\n%s\n\n" (make-string (min 80 (window-width)) ?-)))
          (push keymap seen))))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'describe-all-keymaps)))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x x k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x x r") 'rename-current-buffer-file)

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Keybinds for Russian Layout
;; The keybinds map to be in the same location on the board
;; (global-set-key (kbd "C-н") #'next-line) ;; C-n
;; (global-set-key (kbd "C-р") #'previous-line) ;; C-p
;; (global-set-key (kbd "C-с") #'forward-char) ;; C-f
;; (global-set-key (kbd "C-т") #'backward-char) ;; C-b
;; (global-set-key (kbd "M-с") #'forward-word) ;; M-f
;; (global-set-key (kbd "M-т") #'backward-word) ;; M-b
;; (global-set-key (kbd "C-л") #'consult-line) ;; C-s
;; (global-set-key (kbd "C-д") #'recenter-top-bottom) ;; C-l
;; (global-set-key (kbd "C-к") #'sp-delete-char) ;; C-d
;; (global-set-key (kbd "C-е") #'beginning-of-visual-line) ;; C-a
;; (global-set-key (kbd "C-о") #'end-of-visual-line) ;; C-e
;; (global-set-key (kbd "C-х") #'sp-kill-hybrid-sexp) ;; C-k
;; (global-set-key (kbd "C-з") #'sp-kill-region) ;; C-w
;; (global-set-key (kbd "C-б") #'keyboard-quit) ;; C-g
;; (global-set-key (kbd "C-п") #'yank) ;; C-y
;; (global-set-key (kbd "C-ж") #'scroll-up-command) ;; C-v
;; (global-set-key (kbd "M-ж") #'scroll-down-command) ;; M-v
;; (global-set-key (kbd "M-ё") #'execute-extended-command) ;; M-x

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

  (org-mode . (lambda () (interactive)
                (setq prettify-symbols-alist '(("[#A]" . " ")
                                               ("[#B]" . " ")
                                               ("[#C]" . " ")
                                               ("[ ]" . " ")
                                               ("[X]" . " ")
                                               ("[-]" . " ")
                                               ("#+begin_src" . " ")
                                               ("#+end_src" . "―")
                                               ("#+begin_collapsible" . " ")
                                               ("#+end_collapsible" . "―")
                                               ("#+begin_aside" . " ")
                                               ("#+end_aside" . "―")
                                               ("#+begin_quote" . " ")
                                               ("#+end_quote" . "―")
                                               ("#+begin_defn" .  " ")
                                               ("#+end_defn" . "―")
                                               ("#+begin_questionable" .  " ")
                                               ("#+end_questionable" . "―")
                                               ("#+begin_problem" .  " ")
                                               ("#+end_problem" . "―")
                                               ("#+EXCLUDE_TAGS:" . " ")
                                               (":PROPERTIES:" . "\n")
                                               (":END:" . "―")
                                               ("#+STARTUP:" . " ")
                                               ("#+TITLE: " . "")
                                               ("#+title: " . "")
                                               ("#+RESULTS:" . " ")
                                               ("#+NAME:" . " ")
                                               ("#+ROAM_TAGS:" . " ")
                                               ("#+FILETAGS:" . " ")
                                               ("#+HTML_HEAD:" . " ")
                                               ("#+SUBTITLE:" . " ")
                                               ("#+AUTHOR:" . " ")
                                               (":Effort:" . " ")
                                               ("SCHEDULED:" . " ")
                                               ("DEADLINE:" . " ")
                                               ("#+begin_defn" . " ")
                                               ("#+end_defn" . "―")))
                (prettify-symbols-mode)))

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
  ;; :hook
  ;; (text-mode   . visual-fill-column-mode)
  ;; (prog-mode   . visual-fill-column-mode)
  ;; (conf-mode   . visual-fill-column-mode)
  ;; (fundamental-mode . visual-fill-column-mode)
  ;; (term-mode   . visual-fill-column-mode)
  ;; (eshell-mode . visual-fill-column-mode)
  :custom
  (global-visual-fill-column-mode t)
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (visual-fill-column-mode 0))))

;; Drag and drop
(use-package org-download
  :commands (org-mode org-download-clipboard)
  :custom
  (org-download-screenshot-method "flameshot gui -s --raw > %s")
  :bind ("<f8>" . org-download-screenshot))

;; Org roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Code/Org/Roam")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head
	       "%<%Y%m%d%H%M%S>-${slug}.org"
	       ,(let ((options '("#+options: _:{}"
				 "#+options: ^:{}"
				 "#+startup: latexpreview"
				 "#+startup: entitiespretty"
				 "#+startup: inlineimages"
				 "#+title: ${title}"
				 "#+date: %U")))
		  (mapconcat 'identity options "\n")))
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

;; Writing
(use-package writegood-mode
  :hook (flyspell-mode . writegood-mode)
  :bind (:map flyspell-mode-map
	 ("C-c C-g g" . writegood-grade-level)
	 ("C-c C-g e" . writegood-reading-ease)))

(use-package dictionary)

;; Spelling
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(use-package flyspell-correct
  :after flyspell
  :init
  (add-to-list 'ispell-skip-region-alist '("+begin_src" . "+end_src"))
  (setq flyspell-use-meta-tab nil)
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package frog-menu
  :custom
  ;; Need to redefine keys to account for custom keyboard layout
  (frog-menu-avy-keys (append (string-to-list "aoehsfnbit")
			      (string-to-list "ulrpdyc")
			      (string-to-list "qjkzvwmqxg")
			      (string-to-list (upcase "aoehsfnbit"))
			      (string-to-list (upcase "ulrpdyc"))
			      (string-to-list (upcase "qjkzvwmqxg"))
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
;; --- DIRED ---
;;
(custom-set-variables '(dired-listing-switches "-gho --group-directories-first"))
(custom-set-variables '(dired-dwim-target t))
(custom-set-variables '(delete-by-moving-to-trash t))
;; TODO set `dired-compress-file-alist' to include all archive types

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Make M-> and M-< work in dired
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; Use only one dired buffer at a time
(use-package dired-single)

;; Open certain file extensions in external programs
(use-package dired-open
  :custom
  (dired-open-extensions '(("mp4" . "mpc-qt")
			   ("mpeg" . "mpc-qt")
			   ("ogg" . "mpc-qt")
			   ("mkv" . "mpc-qt")
			   ("webm" . "mpc-qt")
			   ("mp3" . "strawberry")
			   ("opus" . "strawberry")
			   ("wav" . "strawberry")
			   ("weba" . "strawberry")
			   ("aac" . "strawberry")
			   ("doc" . "libreoffice")
			   ("docx" . "libreoffice")
			   ("odt" . "libreoffice")
			   ("ppt" . "libreoffice")
			   ("pptx" . "libreoffice"))))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

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

;; Add newlines when C-n at the end of file
(setq next-line-add-newlines t)

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
            #'TeX-revert-document-buffer)

  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  )

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

;; Use primary as clipboard in emacs
;; (setq x-select-enable-primary t)

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(70 . 70))
;; (add-to-list 'default-frame-alist '(alpha . (70 . 70)))

(defun toggle-transparency ()
  "Toggles emacs transparency."
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

;; Set the fringe to an big enough width
(custom-set-variables '(fringe-mode 20))

;;; init.el ends here
