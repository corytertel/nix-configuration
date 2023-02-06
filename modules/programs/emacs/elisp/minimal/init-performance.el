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

;; Garbage-collect on focus-out, Emacs should feel snappier overall.
(add-function :after after-focus-change-function
  (defun cory/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

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
;; --- NATIVE COMP ---
;;

;; Silence compiler warnings as they can be pretty disruptive
;; (setq comp-async-report-warnings-errors nil)

;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq inhibit-automatic-native-compilation nil))

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Suppressing compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)
(setq warning-minimum-level :error)
