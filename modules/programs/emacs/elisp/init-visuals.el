;;
;; --- MISC ---
;;

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips

;; Make the cursor a bar
(setq-default cursor-type 'hollow)

(setq initial-scratch-message nil)

;; Beacon
;; (use-package beacon
;;   :config
;;   (beacon-mode 1))

;; Visual feedback on yank/kill
;; (use-package goggles
;;   :hook ((prog-mode text-mode) . goggles-mode)
;;   :config
;;   (setq-default goggles-pulse nil))

;; Window dividers
(setq window-divider-default-right-width 3)
(let ((color (face-background 'mode-line)))
  (dolist (face '(window-divider-first-pixel
		  window-divider-last-pixel
		  window-divider))
    (set-face-foreground face color)))
(window-divider-mode 1)

;; Center text in the frame
(use-package olivetti
  :hook ((text-mode         . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
	 (woman-mode        . olivetti-mode)
	 (ibuffer-mode      . olivetti-mode)
         (org-mode          . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)
         (mu4e-compose-mode . olivetti-mode)
	 (image-mode        . olivetti-mode)
	 (eshell-mode . (lambda ()
			  (setq-local olivetti-body-width 150)
			  (olivetti-mode))))
  :custom
  (olivetti-body-width 100)
  :bind
  (:map olivetti-mode-map
   ([left-margin mouse-4] . previous-line)
   ([left-margin mouse-5] . next-line)
   ([left-margin mouse-6] . backward-char)
   ([left-margin mouse-7] . forward-char)
   ([right-margin mouse-4] . previous-line)
   ([right-margin mouse-5] . next-line)
   ([right-margin mouse-6] . backward-char)
   ([right-margin mouse-7] . forward-char)))

;;
;; --- FONT ---
;;

;; Setting the font
(set-face-attribute 'default nil :family "Victor Mono" :height 100)
;; Set fixed pitch face
(set-face-attribute 'fixed-pitch nil :family "Victor Mono" :height 100)
;; Set variable pitch face
(set-face-attribute 'variable-pitch nil :family "Liberation Serif" :height 110)

;; Don't unload fonts when not in use
(setq inhibit-compacting-font-caches t)

;; Italic comments
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)

;; Icons
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0)
  (all-the-icons-default-adjust 0.0)
  :config
  (when (and (not (if (find-font (font-spec :name "all-the-icons")) t nil))
           (window-system))
    (all-the-icons-install-fonts t)))

;; Ligatures and Indicators
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; APL-like characters for scheme
(add-hook
 'scheme-mode-hook
 (lambda ()
   (setq prettify-symbols-alist '(("lambda" . #x3BB)
				  ("->"    . #x2192)
				  ("<=="   . #x21D0)
				  ("==>"   . #x21D2)
				  ("<="       . "≤")
				  (">="       . "≥")
				  ("define"   . "≝")
				  ("set!"     . "≐")
				  ("set-car!" . "≔")
				  ("set-cdr!" . "≕")
				  ("#t"       . "✓")
				  ("#f"       . "✗")
				  ("'()"      . "∅")
				  ("null"     . "∅")
				  ("if"       . "⁇")
				  ("or"       . "∨")
				  ("and"      . "∧")
				  ("not"      . "¬")))
   (prettify-symbols-mode 1)))

;; APL-like characters for elisp
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq prettify-symbols-alist '(("lambda" . #x3BB)
				  ("->"    . #x2192)
				  ("<=="   . #x21D0)
				  ("==>"   . #x21D2)
				  ("<="       . "≤")
				  (">="       . "≥")
				  ("t"        . "✓")
				  ("'()"      . "∅")
				  ("nil"      . "∅")
				  ("if"       . "⁇")
				  ("or"       . "∨")
				  ("and"      . "∧")
				  ("not"      . "¬")))
   (prettify-symbols-mode 1)))

;;
;; --- THEME ---
;;

(setq custom-safe-themes t) ; Treat all themes as safe
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
;; (add-hook 'emacs-startup-hook (lambda () (load-theme 'plain-light t)))

;; Basic theme settings
(set-face-attribute 'mode-line nil
		    :foreground "#141404"
		    :background "#c0daff"
		    :box nil
		    :underline "#3647d9"
		    :overline "#3647d9")
(set-face-attribute 'mode-line-inactive nil
		    :foreground "#141404"
		    :background "#ffffff"
		    :box nil
		    :underline "#3647d9"
		    :overline "#3647d9")
(set-face-attribute 'mode-line-buffer-id nil
		    :inherit 'bold
		    :foreground "#3647d9")
(set-face-attribute 'fringe nil
		    :background "#ffffff")
(set-face-attribute 'secondary-selection nil
		    :background "#fffccc")

;;
;; --- MODE-LINE ---
;;


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

;; Git Parse Repo Status
;; See https://kitchingroup.cheme.cmu.edu/blog/2014/09/19/A-git-status-Emacs-modeline/
(defun cory/mode-line-git-parse-status ()
  "Display the status of the repo."
  (interactive)
  (let ((U 0)   ; untracked files
        (M 0)   ; modified files
        (O 0)   ; other files
        (U-files "")
        (M-files "")
        (O-files ""))
    (dolist (line (split-string
                   (shell-command-to-string "git status --porcelain")
                   "\n"))
      (cond

       ;; ignore empty line at end
       ((string= "" line) nil)

       ((string-match "^\\?\\?" line)
        (setq U (+ 1 U))
        (setq U-files (concat U-files "\n" line)))

       ((string-match "^ M" line)
        (setq M (+ 1 M))
        (setq M-files (concat M-files "\n" line)))))

    ;; construct propertized string
    (concat
     "Git["
     (propertize
      (format "M:%d" M)
      'face (if (> M 0)
                'error
              'success)
      'help-echo M-files)
     " "
     (propertize
      (format "?:%d" U)
      'face (if (> U 0)
                'warning
              'success)
      'help-echo U-files)
     "]")))

;; (setq-default
;;  mode-line-format
;;  '("  "
;;    (:eval (let ((icon (all-the-icons-icon-for-mode major-mode)))
;; 	    (propertize
;; 	     icon
;; 	     'face
;; 	     (plist-put
;; 	      (get-text-property 0 'face icon)
;; 	      :height 1.0)
;; 	     'font-lock-face
;; 	     (plist-put
;; 	      (get-text-property 0 'font-lock-face icon)
;; 	      :height 1.0)
;; 	     'display
;; 	     '(raise 0))))
;;    "  "
;;    (:eval (let ((str (if buffer-read-only
;;                          (if (buffer-modified-p) "%%*" "%%%%")
;;                        (if (buffer-modified-p) "**" "--"))))
;;             (if buffer-read-only
;;                 (propertize str 'face 'modeline-read-only-face)
;;               (if (buffer-modified-p)
;;                   (propertize str 'face 'modeline-modified-face)
;;                 str))))
;;    (list 'line-number-mode "  ")
;;    (:eval (when line-number-mode
;;             (let ((str "L%l"))
;;               (if (/= (buffer-size) (- (point-max) (point-min)))
;;                   (propertize str 'face 'modeline-narrow-face)
;;                 str))))
;;    "  %p"
;;    (list 'column-number-mode "  C%c")
;;    "  " mode-line-buffer-identification
;;    "  " mode-line-modes
;;    (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))
;;    "  "
;;    (:eval (cory/mode-line-git-parse-status))))

(setq-default
 mode-line-format
 '("  "
   (:eval (let ((icon (all-the-icons-icon-for-mode major-mode)))
	    (propertize
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
   "  "
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
   "  " mode-line-modes))

(use-package moody
  :custom
  ;; (moody-mode-line-height (* (aref (font-info (face-font 'mode-line)) 2) 1.5))
  ;; (moody-mode-line-height 40
  (moody-mode-line-height 25)
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :config
  (minions-mode)
  (push '(macrursors-mode) minions-available-modes))
