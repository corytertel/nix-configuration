;;
;; --- MISC ---
;;

(scroll-bar-mode -1) ; Disables the visible scrollbar
(tool-bar-mode -1)   ; Disables the toolbar
(menu-bar-mode -1)   ; Disables the menubar
(tooltip-mode -1)    ; Disables tooltips

;; Make the cursor a bar
(setq-default cursor-type 'hollow)
;; (setq-default cursor-type 'box)
;; (set-cursor-color "darkseagreen2")

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
;; (setq window-divider-default-right-width 3)
;; (let ((color (face-background 'mode-line)))
;;   (dolist (face '(window-divider-first-pixel
;; 		  window-divider-last-pixel
;; 		  window-divider))
;;     (set-face-foreground face color)))
;; (window-divider-mode 1)

;; ;; Center text in the frame
;; (use-package olivetti
;;   :hook ((text-mode         . olivetti-mode)
;;          (prog-mode         . olivetti-mode)
;;          (Info-mode         . olivetti-mode)
;; 	 (woman-mode        . olivetti-mode)
;; 	 (ibuffer-mode      . olivetti-mode)
;;          (org-mode          . olivetti-mode)
;;          (mu4e-view-mode    . olivetti-mode)
;;          (elfeed-show-mode  . olivetti-mode)
;;          (mu4e-compose-mode . olivetti-mode)
;; 	 (image-mode        . olivetti-mode))
;;   :custom
;;   (olivetti-body-width 100)
;;   ;; :config
;;   ;;   (defgroup auto-olivetti nil
;;   ;;     "Automatically enable `olivetti-mode' in wide windows."
;;   ;;     :link '(url-link :tag "Homepage" "https://sr.ht/~ashton314/auto-olivetti")
;;   ;;     :group 'text
;;   ;;     :prefix "auto-olivetti-")

;;   ;;   (defcustom auto-olivetti-enabled-modes '(fundamental-mode)
;;   ;;     "Modes for which `olivetti-mode' should automatically be enabled for."
;;   ;;     :type '(repeat symbol))

;;   ;;   (defcustom auto-olivetti-threshold-fraction 1.3
;;   ;;     "Fraction of `olivetti-body-width' at which to enable `olivetti-mode'."
;;   ;;     :type 'float)

;;   ;;   (defcustom auto-olivetti-threshold-absolute 100
;;   ;;     "Number of columns at which to enable `olivetti-mode'."
;;   ;;     :type 'natnum)

;;   ;;   (defcustom auto-olivetti-threshold-method 'absolute
;;   ;;     "How to determine if the activation threshold has been met.
;;   ;; - fraction: use `auto-olivetti-threshold-fraction' * `olivetti-body-width'
;;   ;; - absolute: use `auto-olivetti-threshold-absolute'"
;;   ;;     :type '(choice (const fraction) (const absolute)))

;;   ;;   (defvar-local auto-olivetti--vlm-active nil
;;   ;;     "Old value of `visual-line-mode' in current buffer.")

;;   ;;   (defun auto-olivetti--do-change ()
;;   ;;     "Turn on or off `olivetti-mode' depending on the current window configuration."
;;   ;;     (setq-local auto-olivetti--vlm-active (or olivetti--visual-line-mode
;;   ;;                                              (and (not olivetti-mode) visual-line-mode)))
;;   ;;     (if (and (bound-and-true-p auto-olivetti-mode)                  ; mode enabled?
;;   ;;            (apply #'derived-mode-p auto-olivetti-enabled-modes)   ; in correct major-mode
;;   ;;            (> (window-total-width)                                ; window big enough?
;;   ;;               (if (eq auto-olivetti-threshold-method 'fraction)
;;   ;;                   (* (or olivetti-body-width 80) auto-olivetti-threshold-fraction)
;;   ;;                 auto-olivetti-threshold-absolute)))
;;   ;; 	(olivetti-mode +1)
;;   ;;       (when olivetti-mode
;;   ;; 	(olivetti-mode -1)
;;   ;; 	(when (bound-and-true-p auto-olivetti--vlm-active)
;;   ;;           (visual-line-mode)))))

;;   ;;   (define-minor-mode auto-olivetti-mode
;;   ;;     "Automatically enable `olivetti-mode' in wide windows."
;;   ;;     :global t :group 'auto-olivetti
;;   ;;     (if auto-olivetti-mode
;;   ;; 	(add-hook 'window-configuration-change-hook 'auto-olivetti--do-change)
;;   ;;       (prog2
;;   ;;           (remove-hook 'window-configuration-change-hook 'auto-olivetti--do-change)
;;   ;;           (olivetti-mode -1)
;;   ;; 	(when (bound-and-true-p auto-olivetti--vlm-active)
;;   ;;           (visual-line-mode)))))

;;   ;;   (auto-olivetti-mode 1)
;;   )

(use-package perfect-margin
  :custom
  (perfect-margin-visible-width 100)
  ;; (perfect-margin-ignore-regexps '())
  (perfect-margin-ignore-regexps '(" \\*Minibuf-[0-9]+\\*"))
  (perfect-margin-hide-fringes nil)
  :init
  ;; Set fringes before turning on perfect-margin
  ;; Set the fringe to an big enough widthh
  (setq-default fringe-mode 20)
  (set-fringe-mode 20)
  :config
  ;; enable perfect-mode
  (perfect-margin-mode t)
  ;; auto-center minibuffer windows
  ;; (setq perfect-margin-ignore-filters nil)
  ;; auto-center special windows
  ;; (setq perfect-margin-ignore-regexps nil)
  ;; add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (global-set-key (kbd (concat margin "<wheel-up>")) 'mwheel-scroll)
    (global-set-key (kbd (concat margin "<wheel-down>")) 'mwheel-scroll))

  (add-hook 'eww-mode-hook (lambda () (setq-local perfect-margin-visible-width 128))))

;; (use-package visual-fill-column
;;   :hook
;;   ((text-mode         . visual-fill-column-mode)
;;    (prog-mode         . visual-fill-column-mode)
;;    (Info-mode         . visual-fill-column-mode)
;;    (woman-mode        . visual-fill-column-mode)
;;    (ibuffer-mode      . visual-fill-column-mode)
;;    (org-mode          . visual-fill-column-mode)
;;    (mu4e-view-mode    . visual-fill-column-mode)
;;    (elfeed-show-mode  . visual-fill-column-mode)
;;    (mu4e-compose-mode . visual-fill-column-mode)
;;    (image-mode        . visual-fill-column-mode))
;;   :custom
;;   (visual-fill-column-width 100)
;;   (visual-fill-column-center-text t))

;;
;; --- FONT ---
;;

;; Setting the font
(set-face-attribute 'default nil :family monospace-font-name :height monospace-font-height)
;; Set fixed pitch face
(set-face-attribute 'fixed-pitch nil :family monospace-font-name :height monospace-font-height)
;; Set variable pitch face
(set-face-attribute 'variable-pitch nil :family variable-font-name :height variable-font-height)

;; Don't unload fonts when not in use
(setq inhibit-compacting-font-caches t)

;; Font locks
(set-face-attribute 'font-lock-comment-face nil
		    :foreground "dark red"
		    :family variable-font-name
		    :height variable-font-height)
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
				  ;; ("define"   . "≝")
				  ;; ("set!"     . "≐")
				  ;; ("set-car!" . "≔")
				  ;; ("set-cdr!" . "≕")
				  ("#t"       . "✓")
				  ("#f"       . "✗")
				  ("'()"      . "∅")
				  ("null"     . "∅")
				  ;; ("if"       . "⁇")
				  ("or"       . "∨")
				  ("and"      . "∧")
				  ("not"      . "¬")
				  ("chain"        . (?~ (Br . Bl) ?>))
				  ("chain-and"    . (#x2227 (Br . Bl) ?~ (Br . Bl) ?>))
				  ("chain-when"   . (#x2047 (Br . Bl) ?~ (Br . Bl) ?>))
				  ("chain-lambda" . (#x3bb  (Br . Bl) ?~ (Br . Bl) ?>))
				  ("case-lambda" . (?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl) #x3bb))))
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
				  ;; ("if"       . "⁇")
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
		    :box '(:line-width 1 :color "#3647d9" :style nil)
		    :underline nil
		    :overline nil
		    :family variable-font-name
		    :height variable-font-height)
(set-face-attribute 'mode-line-inactive nil
    		    :foreground "#141404"
		    :background "#ffffff"
		    :box '(:line-width 1 :color "#3647d9" :style nil)
		    :underline nil
		    :overline nil
		    :family variable-font-name
		    :height variable-font-height)
(set-face-attribute 'mode-line-buffer-id nil
		    :inherit 'bold
		    :foreground "#3647d9"
		    :background nil
		    :family variable-font-name
		    :height variable-font-height)
;; (set-face-attribute 'mode-line-emphasis
;; 		      :weight 'bold)
(set-face-attribute 'mode-line-highlight nil
		    :background "darkseagreen2"
		    :box '(:line-width 1 :color "grey40" :style flat-button))
(set-face-attribute 'header-line nil
		    :background "#ffffff"
		    :family variable-font-name
		    :underline nil
		    :overline nil
		    :height variable-font-height)
(set-face-attribute 'fringe nil
		    :background "#ffffff")
(set-face-attribute 'secondary-selection nil
		    :background "#fffccc")

;;
;; --- MODE-LINE ---
;;


;; Buffer state in modeline
(defface modeline-narrow-face
  `((t (:foreground "#141404" :background "#ed8f23" :family ,variable-font-name :height ,variable-font-height)))
  "Todo/fixme highlighting."
  :group 'faces)

(defface modeline-read-only-face
  `((t (:foreground "#141404" :background "#9feaae" :family ,variable-font-name :height ,variable-font-height)))
  "Read-only buffer highlighting."
  :group 'faces)

(defface modeline-modified-face
  `((t (:foreground "#d8d8d8" :background "#e60909" :family ,variable-font-name :height ,variable-font-height)))
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
(setq display-line-numbers-type 'relative)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;
;; --- Fringes ---
;;

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
