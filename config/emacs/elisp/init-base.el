;;; Commentary:

;; Goals:
;; 1. Frictionless
;; 2. Robust
;; 3. Fast
;; 4. Flexible
;; 5. Compatibility (Non-esotericness)

;; Config Rules:
;; 1. All eligible variables will be set through customize.
;; 2. Anything that is reliant or tied to a package must be put in the
;;    `use-package' of the package. Binds related to the package must
;;    be put in :bind, variables must be set in :custom, and functions
;;    must be put in :config.
;; 3. All functions that are not related to a package must be declared
;;    in the functions file. All keybinds not related to a package must
;;    be declared in the keybinds file.

;; Small, big, bigger keybind philosophy.

;;; Code:

;;
;; -- PACKAGE SETUP ---
;;

(require 'package)

;; makes impure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;
;; --- MISC ---
;;

(setq visible-bell nil
      ring-bell-function #'ignore
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default left-margin-width 1
	      sentence-end-double-space nil
	      lisp-backquote-indentation nil
	      blink-cursor-blinks 10
	      fast-but-imprecise-scrolling t
	      auto-save-interval 60
	      kill-do-not-save-duplicates t
	      bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t
	      frame-resize-pixelwise t)

(save-place-mode 1)

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
             (> (- current (float-time (nth 4 (file-attributes file))))
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

;; Show the time in Phoenix and Moscow in world-clock
(with-eval-after-load 'time
  (add-to-list 'zoneinfo-style-world-list '("America/Phoenix" "Phoenix"))
  (add-to-list 'zoneinfo-style-world-list '("Europe/Moscow" "Moscow")))

;; Startup time
(defun cory/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'cory/display-startup-time)

;; Make all async-shell-command's have their own buffer
(setq async-shell-command-buffer 'new-buffer)

;; Make eval vary based on mode
(defvar eval-last-sexp-function #'eval-last-sexp)

(defun eval-last-sexp-command (eval-last-sexp-arg-internal)
  (interactive "P")
  (funcall eval-last-sexp-function eval-last-sexp-arg-internal))

(global-set-key (kbd "C-x C-e") #'eval-last-sexp-command)
