;;
;; --- MISC ---
;;

;; (scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips

;; Make the cursor a bar
;; (setq-default cursor-type 'hollow)
(setq-default cursor-type 'box)
;; (set-cursor-color "darkseagreen2")
(set-cursor-color "black")

;;
;; --- FONT ---
;;

;; Setting the font
(if bitmap-font-p
    (add-to-list
     'default-frame-alist
     (cons 'font monospace-font-name))
  ;; (progn
  ;;   (set-face-attribute 'default nil :family monospace-font-name)
  ;;   ;; Set fixed pitch face
  ;;   (set-face-attribute 'fixed-pitch nil :family monospace-font-name)
  ;;   ;; Set variable pitch face
  ;;   (set-face-attribute 'variable-pitch nil :family variable-font-name))
  (progn
    (set-face-attribute 'default nil :family monospace-font-name :height monospace-font-height)
    ;; Set fixed pitch face
    (set-face-attribute 'fixed-pitch nil :family monospace-font-name :height monospace-font-height)
    ;; Set variable pitch face
    (set-face-attribute 'variable-pitch nil :family variable-font-name :height variable-font-height)))


;; Don't unload fonts when not in use
(setq inhibit-compacting-font-caches t)

;; Font locks
(set-face-attribute 'font-lock-comment-face nil :foreground "dark red")
;; (unless bitmap-font-p
;;   (set-face-attribute 'font-lock-comment-face nil
;; 		      :foreground "dark red"
;; 		      :family variable-font-name
;; 		      :height variable-font-height))
(set-face-attribute 'font-lock-comment-delimiter-face nil
		    :foreground "dark red"
		    :inherit nil)
(set-face-attribute 'font-lock-builtin-face nil
		    :inherit 'bold)
(set-face-attribute 'font-lock-keyword-face nil
		    :inherit 'bold)
(set-face-attribute 'font-lock-negation-char-face nil
		    :inherit 'error)
(set-face-attribute 'font-lock-string-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-type-face nil
		    :inherit 'bold)
(set-face-attribute 'font-lock-warning-face nil
		    :inherit 'warning)
(set-face-attribute 'font-lock-punctuation-face nil
		    :slant 'italic)

;; Popup
(with-eval-after-load 'popup
  (set-face-attribute 'popup-tip-face nil
		      ;; :background "white"
		      :slant 'italic
		      ;; :box '(:line-width 1 :color "dark red" :style nil)
		      :background "#c0daff"))

;; Emojis
(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (progn
	      (set-fontset-font
	       t
	       (if (version< emacs-version "28.1")
		   '(#x1f300 . #x1fad0)
		 'emoji)
	       (cond
		((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
		((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
		((member "Noto Emoji" (font-family-list)) "Noto Emoji")
		((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
		((member "Symbola" (font-family-list)) "Symbola"))))))

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
;; (add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; ;; APL-like characters for scheme
;; (add-hook
;;  'scheme-mode-hook
;;  (lambda ()
;;    (setq prettify-symbols-alist '(("lambda" . #x3BB)
;; 				  ("->"    . #x2192)
;; 				  ("<=="   . #x21D0)
;; 				  ("==>"   . #x21D2)
;; 				  ("<="       . "≤")
;; 				  (">="       . "≥")
;; 				  ;; ("define"   . "≝")
;; 				  ;; ("set!"     . "≐")
;; 				  ;; ("set-car!" . "≔")
;; 				  ;; ("set-cdr!" . "≕")
;; 				  ("#t"       . "✓")
;; 				  ("#f"       . "✗")
;; 				  ("'()"      . "∅")
;; 				  ("null"     . "∅")
;; 				  ;; ("if"       . "⁇")
;; 				  ("or"       . "∨")
;; 				  ("and"      . "∧")
;; 				  ("not"      . "¬")
;; 				  ("chain"        . (?~ (Br . Bl) ?>))
;; 				  ("chain-and"    . (#x2227 (Br . Bl) ?~ (Br . Bl) ?>))
;; 				  ("chain-when"   . (#x2047 (Br . Bl) ?~ (Br . Bl) ?>))
;; 				  ("chain-lambda" . (#x3bb  (Br . Bl) ?~ (Br . Bl) ?>))
;; 				  ("case-lambda" . (?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl) #x3bb))))
;;    (prettify-symbols-mode 1)))

;; ;; APL-like characters for elisp
;; (add-hook
;;  'emacs-lisp-mode-hook
;;  (lambda ()
;;    (setq prettify-symbols-alist '(("lambda" . #x3BB)
;; 				  ("->"    . #x2192)
;; 				  ("<=="   . #x21D0)
;; 				  ("==>"   . #x21D2)
;; 				  ("<="       . "≤")
;; 				  (">="       . "≥")
;; 				  ("t"        . "✓")
;; 				  ("'()"      . "∅")
;; 				  ("nil"      . "∅")
;; 				  ;; ("if"       . "⁇")
;; 				  ("or"       . "∨")
;; 				  ("and"      . "∧")
;; 				  ("not"      . "¬")))
;;    (prettify-symbols-mode 1)))

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
		    :box '(:line-width 1 :color "#3647d9" :style nil)
		    :underline nil
		    :overline nil)
(set-face-attribute 'mode-line-inactive nil
    		    :foreground "#141404"
		    :background "#ffffff"
		    :box '(:line-width 1 :color "#3647d9" :style nil)
		    :underline nil
		    :overline nil)
(set-face-attribute 'mode-line-buffer-id nil
		    :inherit 'bold
		    :foreground "#3647d9"
		    :background nil)
;; (set-face-attribute 'mode-line-emphasis
;; 		      :weight 'bold)
(set-face-attribute 'mode-line-highlight nil
		    :background "darkseagreen2"
		    :box '(:line-width 1 :color "grey40" :style flat-button))
(set-face-attribute 'header-line nil
		    :background "#ffffff"
		    :underline nil
		    :overline nil)
(set-face-attribute 'fringe nil
		    :background "#ffffff")
(set-face-attribute 'secondary-selection nil
		    :background "#fffccc")

(unless bitmap-font-p
  (set-face-attribute 'mode-line nil
		      :family variable-font-name
		      :height variable-font-height)
  (set-face-attribute 'mode-line-inactive nil
		      :family variable-font-name
		      :height variable-font-height)
  (set-face-attribute 'mode-line-buffer-id nil
		      :family variable-font-name
		      :height variable-font-height)
  (set-face-attribute 'header-line nil
		      :family variable-font-name
		      :height variable-font-height))

;;
;; --- MODE-LINE ---
;;


;; Buffer state in modeline
(defface modeline-narrow-face
  `((t (:foreground "#141404" :background "#ed8f23")))
  "Todo/fixme highlighting."
  :group 'faces)

(defface modeline-read-only-face
  `((t (:foreground "#141404" :background "#9feaae")))
  "Read-only buffer highlighting."
  :group 'faces)

(defface modeline-modified-face
  `((t (:foreground "#d8d8d8" :background "#e60909")))
  "Modified buffer highlighting."
  :group 'faces)

(unless bitmap-font-p
  (set-face-attribute 'modeline-narrow-face nil
                      :family variable-font-name
                      :height variable-font-height)
  (set-face-attribute 'modeline-read-only-face nil
                      :family variable-font-name
                      :height variable-font-height)
  (set-face-attribute 'modeline-modified-face nil
                      :family variable-font-name
                      :height variable-font-height))

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
   ;; (:eval (let ((icon (all-the-icons-icon-for-mode major-mode)))
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
   ;; "  "
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

;; (use-package moody
;;   :custom
;;   ;; (moody-mode-line-height (* (aref (font-info (face-font 'mode-line)) 2) 1.5))
;;   ;; (moody-mode-line-height 23)
;;   (moody-mode-line-height 30)
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :config
  (minions-mode)
  ;; (push '(macrursors-mode) minions-available-modes)
  )

;;; Line Numbers
;; (setq display-line-numbers-type 'relative)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
(set-face-attribute 'line-number nil :inherit '(shadow fixed-pitch))

;;
;; --- Fringes ---
;;

;; Set the fringe to an big enough widthh
(setq-default fringe-mode 20)
(set-fringe-mode 20)

;; Truncation indicators
;; (define-fringe-bitmap 'left-curly-arrow
;;   (vector #b0000001111111111
;; 	  #b0000111111111111
;; 	  #b0001111000000000
;; 	  #b0001110000000000
;; 	  #b0011100000000000
;; 	  #b0001110000000000
;; 	  #b0000111000000001
;; 	  #b0000011100000011
;; 	  #b0000001110000111
;; 	  #b0000000111001111
;; 	  #b0000000011111111
;; 	  #b0000000001111111
;; 	  #b0000000001111111
;; 	  #b0000000011111111
;; 	  #b0000000111111111
;; 	  #b0000001111111111)
;;   16
;;   16
;;   'center)

;; (define-fringe-bitmap 'right-curly-arrow
;;   (vector #b1111111111000000
;; 	  #b1111111111110000
;; 	  #b0000000001111000
;; 	  #b0000000000111000
;; 	  #b0000000000011100
;; 	  #b0000000000111000
;; 	  #b1000000001110000
;; 	  #b1100000011100000
;; 	  #b1110000111000000
;; 	  #b1111001110000000
;; 	  #b1111111100000000
;; 	  #b1111111000000000
;; 	  #b1111111000000000
;; 	  #b1111111100000000
;; 	  #b1111111110000000
;; 	  #b1111111111000000)
;;   16
;;   16
;;   'center)
