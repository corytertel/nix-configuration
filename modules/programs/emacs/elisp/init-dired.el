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
;; (use-package dired-single)

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

;; (use-package dirvish
;;   :config
;;   (dirvish-override-dired-mode))

(use-package sunrise
  :custom
  (sunrise-set-use-commander-keys nil)
  :bind
  ;; NOTE Sunrise uses "C-c s", "C-c t", "C-c r", "C-c v",
  ;; "C-c p", and "C-c b", from the user's space
  (("C-z" . cory/sunrise-toggle)
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
   ("<f10>" . menu-bar-open))
  :hook
  ;; (sunrise-mode . hl-line-mode)
  (sunrise-mode . (lambda () (setq-local cursor-type nil)))
  :config
  (set-face-attribute 'hl-line nil :background nil :inherit 'highlight)

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
    (or (sunrise-quit)
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

  )
