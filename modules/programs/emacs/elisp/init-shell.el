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

(defun cory/eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun cory/eshell-forward-char-or-complete-from-history (&optional arg)
  "If at the end of the eshell buffer, complete from history.
Else, go foward ARG characters."
  (interactive)
  (if (and (= (point) (point-max))
	(not (= (save-excursion (eshell-bol) (point)) (point-max))))
      (cape-history t)
    (forward-char arg)))

(defun cory/eshell-move-end-of-line-or-complete-from-history (&optional arg)
  "If at the end of the eshell buffer, complete from history.
Else, go to the end of line ARG number of times."
  (interactive)
  (if (and (= (point) (point-max))
	(not (= (save-excursion (eshell-bol) (point)) (point-max))))
      (cape-history t)
    (move-end-of-line arg)))

(defun cory/eshell-insert-file-name ()
  (interactive)
  (insert (read-file-name "Dir: ")))

;; Keybinds
(define-key eshell-mode-map (kbd "M-f") #'consult-history)
(define-key eshell-mode-map (kbd "M-r") #'consult-history)
(define-key eshell-mode-map [remap recenter-top-bottom] #'cory/eshell-clear-buffer)
(define-key eshell-mode-map [remap forward-char] #'cory/eshell-forward-char-or-complete-from-history)
(define-key eshell-mode-map (kbd "<right>") #'cory/eshell-forward-char-or-complete-from-history)
(define-key eshell-mode-map [remap move-end-of-line] #'cory/eshell-move-end-of-line-or-complete-from-history)
(define-key eshell-mode-map [remap end-of-visual-line] #'cory/eshell-move-end-of-line-or-complete-from-history)
(define-key eshell-mode-map (kbd "<end>") #'cory/eshell-move-end-of-line-or-complete-from-history)
(define-key eshell-mode-map [remap find-file] #'cory/eshell-insert-file-name)

(defun cory/configure-eshell ()
  "Eshell configuration that will run the first time eshell launches."
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size 1000
	eshell-buffer-maximum-lines 1000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t)

  ;; (setq-local completion-in-region-function #'consult-completion-in-region)

  ;; Alias setup
  ;; (eshell/alias "nixos-update" "nix flake update")
  ;; (eshell/alias "nixos-clean" "sudo nix-collect-garbage --delete-older-than $1")
  ;; (eshell/alias "nixos-superclean" "sudo nix-collect-garbage --delete-old")
  ;; (eshell/alias "n" "cd $HOME/.config/nix")
  ;; (eshell/alias "nd" "nix develop $*")
  ;; (eshell/alias "ls" "eshell/ls -A")
  ;; (eshell/alias "l" "ls $*")
  ;; (eshell/alias "ll" "ls -l -h $*")
  ;; (eshell/alias "c" "clear-scrollback")
  ;; (eshell/alias "clear" "clear-scrollback")
  ;; (eshell/alias "grep" "grep -i --color=auto $*")
  ;; (eshell/alias "rm" "eshell/rm --verbose $*")
  ;; (eshell/alias "mv" "eshell/mv --interactive --verbose $1 $2")
  ;; (eshell/alias "cp" "eshell/cp --interactive --verbose $1 $2 $3")
  ;; (eshell/alias "nf" "neofetch $*")
  ;; (eshell/alias "e" "find-file $1")
  ;; (eshell/alias "eo" "find-file-other-window $1")
  ;; (eshell/alias "edit" "find-file $1")
  ;; (eshell/alias "edit-other" "find-file-other-window $1")
  )

(add-hook 'eshell-first-time-mode-hook 'cory/configure-eshell)

;; Optimize corfu for eshell
;; (add-hook 'eshell-mode-hook
;;           (lambda () (setq-local corfu-quit-at-boundary t
;; 			    corfu-quit-no-match t
;; 			    corfu-auto t
;; 			    completion-at-point-functions
;; 			    (list (cape-super-capf
;; 				   #'cape-history
;; 				   #'pcomplete-completions-at-point)))
;; 	    (corfu-mode 1)))

;; Don't use corfu in eshell
(add-hook 'eshell-mode-hook
          (lambda () (setq-local
		 completion-cycle-threshold t
		 completion-at-point-functions
		 (list #'pcomplete-completions-at-point)
		 completion-styles '(basic partial-completion emacs22))
	    (corfu-mode -1)))

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
  :disabled t
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
  :disabled t
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

(defun eshell/extract (file &rest args)
  "Unpack FILE with ARGS.
Stolen from aweshell."
  (let ((command (-some (lambda (x)
                          (if (string-match-p (car x) file)
                              (cadr x)))
                        '((".*\.tar.bz2" "tar xjf")
                          (".*\.tar.gz" "tar xzf")
                          (".*\.bz2" "bunzip2")
                          (".*\.rar" "unrar x")
                          (".*\.gz" "gunzip")
                          (".*\.tar" "tar xf")
                          (".*\.tbz2" "tar xjf")
                          (".*\.tgz" "tar xzf")
                          (".*\.zip" "unzip")
                          (".*\.Z" "uncompress")
                          (".*" "echo 'Could not unpack the file:'")))))
    (let ((unpack-command (concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))
    ))

(defun eshell/cat (filename)
  "Like cat(1) but with syntax highlighting.
Stole from aweshell. FILENAME is the file to display."
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
         (remove-text-properties 0 (length contents) '(read-only nil) contents)
         contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

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

;; Running programs in a term-mode buffer
;; (with-eval-after-load 'esh-opt
;;   (setq eshell-destroy-buffer-when-process-dies t)
;;   (setq eshell-visual-commands '("htop" "zsh" "vim")))

;;; Give eshell/ls icons
;; Make files and dirs clickable as well as prettyfied w/icons and suffixes

(defun eshell-ls-file-at-point ()
  "Get the full path of the Eshell listing at point."
  (get-text-property (point) 'file-name))

(defun eshell-ls-find-file ()
  "Open the Eshell listing at point."
  (interactive)
  (find-file (eshell-ls-file-at-point)))

(defun eshell-ls-delete-file ()
  "Delete the Eshell listing at point."
  (interactive)
  (let ((file (eshell-ls-file-at-point)))
    (when (yes-or-no-p (format "Delete file %s?" file))
      (delete-file file 'trash))))

(defvar eshell-ls-file-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eshell-ls-find-file)
    (define-key map (kbd "<return>") #'eshell-ls-find-file)
    (define-key map [mouse-1] #'eshell-ls-find-file)
    (define-key map (kbd "D") #'eshell-ls-delete-file)
    map)
  "Keys in effect when point is over a file from `eshell/ls'.")

(defface all-the-icons-eshell-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'all-the-icons-faces)

(defcustom all-the-icons-eshell-v-adjust 0.01
  "The default vertical adjustment of the icon in the eshell buffer."
  :group 'all-the-icons
  :type 'number)

(defcustom all-the-icons-eshell-monochrome t
  "Whether to show the icons as the same color as the text on the same line."
  :group 'all-the-icons
  :type 'boolean)

(defun cory/eshell-better-ls (file)
  "Add features to listings in `eshell/ls' output.
The features are:
1. Add decoration like 'ls -F':
 * Mark directories with a `/'
 * Mark executables with a `*'
2. Make each listing into a clickable link to open the
corresponding file or directory.
3. Add icons (requires `all-the-icons`)
This function is meant to be used as advice around
`eshell-ls-annotate', where FILE is the cons describing the file."
  (let* ((name (car file))
         (icon (if (eq (cadr file) t)
                   (all-the-icons-icon-for-dir name)
                 (all-the-icons-icon-for-file name)))
         (suffix
          (cond
           ;; Directory
           ((eq (cadr file) t)
            "/")
           ;; Executable
           ((and (/= (user-uid) 0) ; root can execute anything
               (eshell-ls-applicable (cdr file) 3 #'file-executable-p (car file)))
            "*"))))
    (cons
     (concat " "
             icon
             " "
             (propertize name
                         'keymap eshell-ls-file-keymap
                         'mouse-face 'highlight
                         'file-name (expand-file-name (substring-no-properties (car file)) default-directory))
             (when (and suffix (not (string-suffix-p suffix name)))
               (propertize suffix 'face 'shadow)))
     (cdr file))))

(advice-add #'eshell-ls-annotate :filter-return #'cory/eshell-better-ls)

;; Eshell prompt
(use-package eshell-prompt-extras
  :config
  (defun epe-theme-cory ()
    "Cory's prompt."
    ;; If the prompt spans over multiple lines, the regexp should match
    ;; last line only.
    (setq eshell-prompt-regexp "^╰─λ ")
    (let* ((icon (all-the-icons-icon-for-mode 'nix-mode))
	   (nix (propertize
		 icon
		 'face
		 (plist-put
		  (get-text-property 0 'face icon)
		  :height 1.0)
		 'font-lock-face
		 (plist-put
		  (get-text-property 0 'font-lock-face icon)
		  :height 1.0)
		 'display
		 '(raise 0))))
      (concat
       (epe-colorize-with-face (epe-status) 'epe-status-face)
       (when (epe-remote-p)
	 (epe-colorize-with-face
	  (concat "(" (epe-remote-user) "@" (epe-remote-host) ")")
	  'epe-remote-face))
       (when (and epe-show-python-info (bound-and-true-p venv-current-name))
	 (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))
       (let ((f (cond ((eq epe-path-style 'fish) 'epe-fish-path)
                      ((eq epe-path-style 'single) 'epe-abbrev-dir-name)
                      ((eq epe-path-style 'full) 'abbreviate-file-name))))
	 (pcase (epe-extract-git-component (funcall f (eshell/pwd)))
	   (`(,prefix nil)
            (format
             (propertize "╭╴%s %s" 'face '(:weight regular))
	     nix
             (propertize prefix 'face '(:weight bold :foreground "#3647d9"))))
	   (`(,prefix ,git-component)
            (format
             (epe-colorize-with-face "╭╴%s %s%s %s %s" '(:weight regular))
	     nix
             (epe-colorize-with-face prefix '(:weight bold :foreground "#3647d9"))
             (if (string-empty-p git-component)
		 ""
               (concat "/"
                       (epe-colorize-with-face git-component '(:weight bold :foreground "#2d9574"))))
             (epe-colorize-with-face
              (concat (or (epe-git-branch)
			 (epe-git-tag))
                      (epe-git-dirty)
                      (epe-git-untracked))
              '(:weight italic :foreground "#1f8c35"))
	     (epe-colorize-with-face
	      (let ((unpushed (epe-git-unpushed-number)))
		(if (= unpushed 0) ""
		  (concat "↑" (number-to-string unpushed))))
	      '(:foreground "#e01bd0"))))))
       (epe-colorize-with-face "\n╰─λ" '(:weight regular))
       " ")))
  (with-eval-after-load "esh-opt"
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-cory)))

;; Eshell undistract me
;; (setq eshell-undistract-me-play-sound t)
;; (setq eshell-undistract-me-sound-path "/run/current-system/sw/share/sounds/freedesktop/stereo/complete.oga")
;; (add-hook 'eshell-pre-command-hook #'eshell-undistract-me-pre-command)
;; (add-hook 'eshell-before-prompt-hook #'eshell-undistract-me-before-prompt)

;; Vterm
(use-package vterm
  :ensure t
  :commands (vterm))

;; (use-package multi-vterm
;;   :ensure t
;;   :bind
;;   ("C-c C-t" . multi-vterm-dedicated-toggle))

;; Desktop launching
(defun emacs-shell ()
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-shell")))
    (unwind-protect
	(eshell))))
