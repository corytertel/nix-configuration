;;
;; --- DIRED ---
;;

(setq dired-listing-switches "-ghoaF --group-directories-first"
      dired-dwim-target t
      delete-by-moving-to-trash t)

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

;; Open certain file extensions in external programs
(use-package dired-open
  :custom
  (dired-open-extensions '(("mp4" . "mpc-qt")
			   ("mpeg" . "mpc-qt")
			   ("ogg" . "mpc-qt")
			   ("mkv" . "mpc-qt")
			   ("mov" . "mpc-qt")
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
			   ("pptx" . "libreoffice")
			   ("xcf" . "gimp"))))

(defun cory/dired ()
  (interactive)
  (dired default-directory))

(defun cory/last-buffer-not-dired (buffers)
  (if buffers
      (let ((name (buffer-name (car buffers))))
	(if (and (equal name (string-trim name "[ \*]+" "\*"))
	      (not (equal 'dired-mode (with-current-buffer (car buffers)
				      major-mode))))
	    (car buffers)
	  (cory/last-real-buffer (cdr buffers))))
    nil))

(defun cory/goto-last-buffer-not-dired ()
  (interactive)
  (switch-to-buffer (cory/last-buffer-not-dired (cdr (buffer-list)))))

(add-hook 'dired-mode-hook (lambda ()
			     (hl-line-mode 1)
			     (setq-local cursor-type nil)))

(defun cory/dired-make-directory ()
  (interactive)
  (call-interactively #'make-directory)
  (call-interactively #'revert-buffer))

(defun cory/dired-make-file ()
  (interactive)
  (call-interactively #'make-empty-file)
  (call-interactively #'revert-buffer))

;; Binds
(global-set-key (kbd "C-/") #'cory/dired)
(define-key dired-mode-map (kbd "C-/") #'cory/goto-last-buffer-not-dired)

;; Minimak binds
(define-key dired-mode-map (kbd "C-M-n") nil)
(define-key dired-mode-map (kbd "C-M-e") #'dired-next-subdir)
(define-key dired-mode-map (kbd "C-M-p") nil)
(define-key dired-mode-map (kbd "C-M-i") #'dired-prev-subdir)
(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "C-d") #'dired-display-file)
(define-key dired-mode-map (kbd "* C-n") nil)
(define-key dired-mode-map (kbd "* C-e") #'dired-next-marked-file)
(define-key dired-mode-map (kbd "* C-p") nil)
(define-key dired-mode-map (kbd "* C-i") #'dired-prev-marked-file)
(define-key dired-mode-map (kbd "M-s a C-M-s") nil)
(define-key dired-mode-map (kbd "M-f a C-M-f") #'dired-do-isearch-regexp)
(define-key dired-mode-map (kbd "M-s a C-s") nil)
(define-key dired-mode-map (kbd "M-f a C-f") #'dired-do-isearch)
(define-key dired-mode-map (kbd "M-s f C-M-s") nil)
(define-key dired-mode-map (kbd "M-f f C-M-f") #'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "M-s f C-s") nil)
(define-key dired-mode-map (kbd "M-f f C-f") #'dired-isearch-filenames)
(define-key dired-mode-map (kbd "n") nil)
(define-key dired-mode-map (kbd "e") #'dired-next-line)
(define-key dired-mode-map (kbd "p") #'dired-do-redisplay)
(define-key dired-mode-map (kbd "i") #'dired-previous-line)
(define-key dired-mode-map (kbd "l") #'dired-maybe-insert-subdir)
(define-key dired-mode-map (kbd "C-f") #'dired-isearch-filenames)
(define-key dired-mode-map (kbd "C-M-f") #'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "<find>") #'dired-isearch-filenames)
(define-key dired-mode-map (kbd "M-<find>") #'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "C-c C-n") #'cory/dired-make-directory)
(define-key dired-mode-map (kbd "C-c C-f") #'cory/dired-make-file)

;;; Sunrise

;; TODO set `dired-compress-file-alist' to include all archive types

;; NOTE May slow down Emacs
;; Auto refresh buffers
;; (global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
;; (setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose nil)

(use-package sunrise
  :disabled t
  :init
  (defcustom cory/sunrise-open-extensions nil
    "Alist of extensions mapping to a programs to run them in.
The filename is appended after the program."
    :type '(alist
            :key-type (string :tag "Extension")
            :value-type (string :tag "Program"))
    :group 'sunrise)
  (defcustom cory/sunrise-open-use-nohup t
    "If non-nil, use nohup(1) to keep external processes opened
even if emacs process is terminated.
This only affects the built-in handlers."
    :type 'boolean
    :group 'sunrise)
  (defcustom cory/sunrise-open-query-before-exit t
    "If non-nil, ask the user if they want to kill any external
processes started by `cory/sunrise-open-file' when they exit emacs.
This only affects the built-in handlers."
    :type 'boolean
    :group 'sunrise)
  :custom
  (sunrise-use-commander-keys nil)
  (sunrise-listing-switches "-ghoaF --group-directories-first")
  (sunrise-virtual-listing-switches "-ghoaFd --group-directories-first")
  (sunrise-show-hidden-files t)
  (delete-by-moving-to-trash t)
  (cory/sunrise-open-extensions '(("mp4" . "mpc-qt")
				  ("mpeg" . "mpc-qt")
				  ("ogg" . "mpc-qt")
				  ("mkv" . "mpc-qt")
				  ("mov" . "mpc-qt")
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
				  ("pptx" . "libreoffice")
				  ("xcf" . "gimp")))
  :bind
  ;; NOTE Sunrise uses "C-c s", "C-c t", "C-c r", "C-c v",
  ;; "C-c p", and "C-c b", from the user's space
  (("C-/" . cory/sunrise-toggle)
   :map sunrise-mode-map
   ;; Remap traditional "commander keys" to non-function keys
   ("C-c 2"     . sunrise-goto-dir)
   ("C-c 3"     . sunrise-quick-view)
   ("C-c 4"     . sunrise-advertised-find-file)
   ("C-c 5"     . sunrise-do-copy)
   ("C-c 6"     . sunrise-do-rename)
   ("C-c 7"     . dired-create-directory)
   ("C-c 8"     . sunrise-do-delete)
   ("C-c 0"     . sunrise-quit)
   ("C-c C-3"   . sunrise-sort-by-name)
   ("C-c C-4"   . sunrise-sort-by-extension)
   ("C-c C-5"   . sunrise-sort-by-time)
   ("C-c C-6"   . sunrise-sort-by-size)
   ("C-c C-7"   . sunrise-sort-by-number)
   ("C-c &"     . sunrise-do-symlink)
   ("<insert>"  . sunrise-mark-toggle)
   ("C-<prior>" . sunrise-dired-prev-subdir)
   ;; Remove binds to function keys
   ("<f2>"  . other-window)
   ("<f3>"  . kmacro-start-macro-or-insert-counter)
   ("<f4>"  . kmacro-end-or-call-macro)
   ("<f10>" . menu-bar-open)
   ;; Other binds
   ("M-<" . cory/sunrise-back-to-top)
   ("M->" . cory/sunrise-jump-to-bottom))
  :hook
  ;; (sunrise-mode . hl-line-mode)
  (sunrise-mode . (lambda () (setq-local cursor-type nil)))
  :config
  (set-face-attribute 'hl-line nil :background nil :inherit 'highlight)

  (defun cory/sunrise-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 1))

  (defun cory/sunrise-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))

  (defun cory/kill-all-sunrise-buffers ()
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(if (derived-mode-p 'sunrise-mode)
	    (kill-buffer buf)))))

  ;; TODO
  ;; (defun cory/kill-all-duplicate-sunrise-buffers ()
  ;;   (interactive))

  (defun cory/sunrise-toggle ()
    "Show or hide the Sunrise Commander."
    (interactive)
    (if (sunrise-quit)
	(hl-line-mode -1)
      (if (and ;; (boundp 'sunrise-left-buffer)
	   ;; (boundp 'sunrise-right-buffer)
	   (buffer-live-p sunrise-left-buffer)
	   (buffer-live-p sunrise-right-buffer))
	  (cory/sunrise-show)
	(sunrise-show))))

  (defun cory/sunrise-show ()
    "Ensure the Sunrise Commander is shown with the past two active buffers."
    (interactive)
    (message "Starting Sunrise Commander...")
    (let ((msg nil))
      (when (cory/sunrise-ensure-windows
             (selected-frame)
             sunrise-left-buffer
             sunrise-right-buffer)
	(setq msg sunrise-start-message))
      (setq sunrise-this-directory default-directory)
      (sunrise-highlight)  ; W32Emacs needs this.
      (hl-line-mode 1)
      (message "%s" msg)))

  (defun cory/sunrise-ensure-windows (frame left-buffer right-buffer)
    "Set up the Sunrise window configuration (two windows in `sunrise-mode').

LEFT-DIRECTORY and RIGHT-DIRECTORY, if non-nil, are the directories to show."
    (cl-destructuring-bind (a-window b-window view-window)
	(sunrise--analyze-frame frame)
      (let ((existing-layout-p (and a-window b-window view-window)))
	(unless existing-layout-p
          (sunrise-switch-to-nonpane-buffer)
          (sunrise--set-frame-plist
           frame
           'restore-config (current-window-configuration)
           'restore-buffer (current-buffer)
           'current-frame  frame)
          (run-hooks 'sunrise-init-hook)
          (sunrise-select-viewer-window)
          (delete-other-windows)
          (unless (and sunrise-panes-height
                       (< sunrise-panes-height (frame-height)))
            (setq sunrise-panes-height (sunrise-get-panes-size)))
          (when (and (<= sunrise-panes-height (* 2 window-min-height))
                     (eq sunrise-window-split-style 'vertical))
            (setq sunrise-panes-height (* 2 window-min-height)))
          (let ((root (selected-window)))
            (setq view-window (split-window root sunrise-panes-height))
            (setq a-window (selected-window))
            (cl-ecase sunrise-window-split-style
              (horizontal
               (setq b-window (split-window-horizontally)))
              (vertical
               (setq b-window (split-window-vertically)))
              (top
               (ignore))))

          (set-window-buffer a-window left-buffer)
          (set-window-buffer b-window right-buffer)

          (if (buffer-live-p other-window-scroll-buffer)
              (switch-to-buffer other-window-scroll-buffer)
            (sunrise-switch-to-nonpane-buffer))))

      ;; (sunrise--update-frame-plist
      ;;  frame
      ;;  'a-directory (lambda (old-directory) (or a-directory old-directory))
      ;;  'b-directory (lambda (old-directory) (or b-directory old-directory)))

      ;; (when a-window (sunrise--setup-directory-window a-directory a-window))
      ;; (when b-window (sunrise--setup-directory-window b-directory b-window))
      (sunrise-restore-panes-width)
      (run-hooks 'sunrise-start-hook)))

  (defun cory/sunrise-open-start-process (file command)
    "Open FILE with COMMAND.
FILE is string, path to the file you want to open.  It is
resolved with `file-truename'.
Note that FILE should not be \"shell escaped\", that is handled
by this function if the shell is invoked.
COMMAND is a string representing the command to run.  If you want
to call it with any switches, these should be included in this
string as well."
    (let ((process
           (apply 'start-process "sunrise-open" nil
                  (if cory/sunrise-open-use-nohup
                      (list "sh" "-c"
                            (concat
                             "nohup "
                             command
                             " "
                             (shell-quote-argument (file-truename file))
                             " 2>&1 >/dev/null"))
                    (append (split-string command " ")
                            (list (file-truename file)))))))
      (when (and process
		 (not cory/sunrise-open-query-before-exit))
	(set-process-query-on-exit-flag process nil))
      process))

  (defun cory/sunrise-open-by-extension (filename)
    "Open a file according to its extension.
The mappings from extensions to applications is specified by
`cory/sunrise-open-extensions'."
    (interactive)
    (let (process)
      (when (and filename
		 (not (file-directory-p filename)))
	(--each-while cory/sunrise-open-extensions (not process)
          (when (string-match-p (concat "\\." (regexp-quote (car it)) "\\'") filename)
            (setq process (cory/sunrise-open-start-process filename (cdr it)))))
	process)))

  ;; Redefinition
  (defun sunrise-find-file (filename &optional wildcards)
    "Determine the proper way of handling an object in the file system.

FILENAME can be either a regular file, a regular directory, a
Sunrise VIRTUAL directory, or a virtual directory served by AVFS.
WILDCARDS is passed to `sunrise-find-regular-file'."
    (interactive (find-file-read-args "Find file or directory: " nil))
    (cl-ecase (sunrise-classify-file filename)
      (file
       (or (cory/sunrise-open-by-extension filename)
	   (sunrise-find-regular-file filename wildcards)))
      (directory
       (sunrise-find-regular-directory filename))
      (virtual-directory
       (sunrise-find-virtual-directory filename))
      (avfs-directory
       (sunrise-find-regular-directory (sunrise-avfs-dir filename))))))
