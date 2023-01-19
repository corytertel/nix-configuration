;;
;; --- MISC ---
;;

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips

;; Make the cursor a bar
;; (setq-default cursor-type 'bar)
(setq-default cursor-type 'hollow)

(setq initial-scratch-message nil)

;; Beacon
(use-package beacon
  :config
  (beacon-mode 1))

;; Visual feedback on yank/kill
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil))

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

;; Add padding to the sides
;; (require 'frame)
;; (setq-default default-frame-alist
;; 	      (append (list
;; 		       '(internal-border-width . 20)
;; 		       ;; '(left-fringe . 0)
;; 		       ;; '(right-fringe . 0)
;; 		       '(tool-bar-lines . 0)
;; 		       '(menu-bar-lines . 0)
;; 		       '(line-spacing . 0.075)
;; 		       '(vertical-scroll-bars . nil))))
;; (setq-default window-resize-pixelwise t)
;; (setq-default frame-resize-pixelwise t)
;; (add-hook 'before-make-frame-hook 'window-divider-mode)

;; (use-package visual-fill-column
;;   ;; :hook
;;   ;; (text-mode   . visual-fill-column-mode)
;;   ;; (prog-mode   . visual-fill-column-mode)
;;   ;; (conf-mode   . visual-fill-column-mode)
;;   ;; (fundamental-mode . visual-fill-column-mode)
;;   ;; (term-mode   . visual-fill-column-mode)
;;   ;; (eshell-mode . visual-fill-column-mode)
;;   :custom
;;   (global-visual-fill-column-mode t)
;;   (visual-fill-column-width 100)
;;   (visual-fill-column-center-text t)
;;   :config
;;   (add-hook 'pdf-view-mode-hook (lambda () (visual-fill-column-mode 0))))

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
  (olivetti-body-width 100))

;; Super smooth scrolling
;; (setq scroll-step            1
;;       scroll-conservatively  10000)
;; (setq next-screen-context-lines 5)

;; Smooth scrolling
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; (setq mouse-wheel-progressive-speed nil)

;; Smooth pixel scrolling
;; (pixel-scroll-mode 1)

;; (pixel-scroll-precision-mode)

;; (setq-local scroll-margin 1
;; 	    scroll-conservatively 101
;; 	    scroll-up-aggressively 0.01
;; 	    scroll-down-aggressively 0.01
;; 	    scroll-preserve-screen-position t
;; 	    auto-window-vscroll nil)

;; Minimap
;; (use-package minimap
;;   :custom
;;   (minimap-window-location ')
;;   :config
;;   (minimap-mode))

;;
;; --- FONT ---
;;

;; Setting the font
(set-face-attribute 'default nil :family "Victor Mono")
;; Set fixed pitch face
;; (set-face-attribute 'fixed-pitch nil :font "Victor Mono")
(set-face-attribute 'fixed-pitch nil :family "Victor Mono")
;; Set variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Oxygen-Sans")
(set-face-attribute 'variable-pitch nil :family "Oxygen-Sans")

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
;; (use-package pretty-mode
;;   :config
;;   (add-hook 'prog-mode-hook 'pretty-mode))
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

(set-face-attribute 'mode-line nil
		    :foreground "#141404"
		    :background "#c0daff")
(set-face-attribute 'mode-line-inactive nil
		    :foreground "#141404"
		    :background "#ffffff")
(set-face-attribute 'fringe nil
		    :background "#ffffff")
;; (set-face-attribute 'whitespace-line nil
;; 		    :background 'unspecified)

(set-face-attribute 'show-paren-match nil
		    :underline t)
(set-face-attribute 'show-paren-mismatch nil
		    :underline t)

;;
;; --- MODE-LINE ---
;;

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

;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
;; (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
;;   "Show the information of git diff in status-line"
;;   (setq ad-return-value
;;         (concat ad-return-value
;;                 (let ((plus-minus (vc-git--run-command-string
;;                                    file "diff" "--numstat" "--")))
;;                   (if (and plus-minus
;;                            (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
;;                       (concat
;;                        " "
;;                        (format "+%s" (match-string 1 plus-minus))
;;                        (format "-%s" (match-string 2 plus-minus)))
;;                     "")))))

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
   "  " mode-line-modes
   (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))
   "  "
   (:eval (cory/mode-line-git-parse-status))))

(use-package moody
  :custom
  ;; (moody-mode-line-height (* (aref (font-info (face-font 'mode-line)) 2) 1.5))
  ;; (moody-mode-line-height 40)
  (moody-mode-line-height 30)
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
