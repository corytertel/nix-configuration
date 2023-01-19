;;; init.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:

;; Goals:
;; 1. Frictionless
;; 2. Robust
;; 3. Fast
;; 4. Flexible

;; Config Rules:
;; 1. All eligible variables will be set through customize.
;; 2. Anything that is reliant or tied to a package must be put in the
;;    `use-package' of the package. Binds related to the package must
;;    be put in :bind, variables must be set in :custom, and functions
;;    must be put in :config.
;; 3. All functions that are not related to a package must be declared
;;    in the functions file. All keybinds not related to a package must
;;    be declared in the keybinds file.

;;; Code:

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
;; 			    ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

;;
;; --- MISC ---
;;

(setq visible-bell nil
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

(setq-default frame-resize-pixelwise t)

;; (global-so-long-mode 1)
(save-place-mode 1)

;; don't back up files
;; (setq make-backup-files nil)

;; Write backup files to tmp dir
;; (setq backup-directory-alist `((".*" . ,temporary-file-directory)) )

;; Write backup files to backup directory instead of editing directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Automatically purge backup files not accessed in a week:
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
             (> (- current (float-time (fifth (file-attributes file))))
                week))
      (message "%s" file)
      (delete-file file))))

;; Use hex mode for binary files
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.dat\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.exe\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.o\\'" . hexl-mode))

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

;; Buffer management
(use-package ibuffer-project
  :hook (ibuffer-mode . cory/ibuffer-setup)
  :custom
  (ibuffer-truncate-lines nil)
  (ibuffer-project-use-cache t)
  (ibuffer-expert t) ; stop yes no prompt on delete
  :config
  (defun cory/ibuffer-setup ()
    (setq ibuffer-filter-groups
	  (append (cdr (ibuffer-project-generate-filter-groups))
		  '(("Programming"
		     (mode . prog-mode))
		    ("Org"
		     (mode . org-mode))
		    ("Magit"
		     (name . "^magit"))
		    ("Planner"
		     (or
		      (name . "^\\*Calendar\\*$")
		      (name . "^\\*Org Agenda\\*")))
		    ("Sunrise"
		     (mode . sunrise-mode))
		    ("Emacs"
		     (or
		      (name . "^\\*scratch\\*$")
		      (name . "^\\*Messages\\*$"))))))))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

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

;; Show the time in Phoenix and Moscow in world-clock
(with-eval-after-load 'time
  (add-to-list 'zoneinfo-style-world-list '("America/Phoenix" "Phoenix"))
  (add-to-list 'zoneinfo-style-world-list '("Europe/Moscow" "Moscow")))

;;; init.el ends here
