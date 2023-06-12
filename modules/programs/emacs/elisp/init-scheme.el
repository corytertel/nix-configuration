;;
;; --- RACKET ---
;;

(use-package racket-mode
  :mode "\\.rkt\\'"
  :bind
  (:map racket-mode-map
   ("C-c C-r" . racket-run)
   :map racket-repl-mode-map
   ("C-c C-r" . racket-run))
  :config
  (defun setup-racket-eldoc ()
    (eldoc-mode +1)
    (setq eldoc-documentation-function #'racket-xp-eldoc-function))

  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook      #'setup-racket-eldoc)
  (add-hook 'racket-mode-hook      #'racket-xp-mode))

(use-package dr-racket-like-unicode)
;; (use-package bracketed-paste)

;;
;; --- CHICKEN SCHEME ---
;;

(custom-set-variables '(scheme-program-name "csi -R r7rs"))

(add-to-list 'auto-mode-alist
             '("\\.egg\\'" . scheme-mode))
(add-to-list 'auto-mode-alist
             '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist
             '("\\.sld\\'" . scheme-mode))
(add-hook 'scheme-mode-hook
          (lambda ()
            (setq open-paren-in-column-0-is-defun-start t)))

(defun scheme-module-indent (state indent-point normal-indent) 0)

(defun scheme-indent-hook ()
  (put 'module 'scheme-indent-function 'scheme-module-indent)
  (put 'define-library 'scheme-indent-function 'scheme-module-indent)
  (put 'define-module 'scheme-indent-function 'scheme-module-indent))

(add-hook 'scheme-mode-hook 'scheme-indent-hook)

;; Faces
(defface scheme-comment-5
  '((t (:underline t
	:foreground "midnight blue"
	:family "Liberation Serif"
	:height 1.5)))
  "Face for scheme comments with 5 semicolons."
  :group 'faces)

(defface scheme-comment-4
  '((t (:foreground "blue4"
	:family "Liberation Serif"
	:height 1.2)))
  "Face for scheme comments with 4 semicolons."
  :group 'faces)

(defface scheme-comment-3
  '((t (:underline t
	:foreground "sienna4"
	:family "Liberation Serif"
	:height 1.0)))
  "Face for scheme comments with 3 semicolons."
  :group 'faces)

;; (add-hook
;;  'scheme-mode-hook
;;  (lambda ()
;;    (interactive)
;;    (font-lock-add-keywords nil `((,(rx bol ";;;;;" (one-or-more nonl) eol) . scheme-comment-5)))
;;    (font-lock-add-keywords nil `((,(rx bol ";;;;" (one-or-more nonl) eol) . scheme-comment-4)))
;;    (font-lock-add-keywords nil `((,(rx bol ";;;" (one-or-more nonl) eol) . scheme-comment-3)))))

(use-package geiser
  :hook
  (geiser-mode . (lambda ()
		   (geiser-capf-setup nil)))
  (scheme-mode . scheme-super-capf)
  ;; (scheme-mode . cory/run-geiser-p)
  :custom
  (geiser-active-implementations '(chicken))
  (geiser-debug-show-debug nil)
  :config
  (with-eval-after-load 'geiser-mode
    (dolist (bind '("C-c C-d d"
		    "C-c C-d C-d"
		    "C-c C-d i"
		    "C-c C-d TAB"
		    "C-h ."))
      (define-key
	geiser-mode-map
	(kbd bind)
	#'cory/chicken-doc-look-up-manual))

    (define-key geiser-mode-map (kbd "C-.") nil)
    (define-key geiser-mode-map (kbd "C-M-i") nil))

  (with-eval-after-load 'geiser-doc
    (define-key geiser-doc-mode-map (kbd "N") nil)
    (define-key geiser-doc-mode-map (kbd "P") nil)
    (define-key geiser-doc-mode-map (kbd "E") #'geiser-doc-next-section)
    (define-key geiser-doc-mode-map (kbd "I") #'geiser-doc-previous-section)
    (define-key geiser-doc-mode-map (kbd "n") nil)
    (define-key geiser-doc-mode-map (kbd "p") nil)
    (define-key geiser-doc-mode-map (kbd "e") #'forward-button)
    (define-key geiser-doc-mode-map (kbd "i") #'backward-button)
    (define-key geiser-doc-mode-map (kbd "b") nil)
    (define-key geiser-doc-mode-map (kbd "j") #'geiser-doc-previous)
    (define-key geiser-doc-mode-map (kbd "f") nil)
    (define-key geiser-doc-mode-map (kbd "l") #'geiser-doc-next))

  ;; Suppress async-shell-command popup
  (add-to-list 'display-buffer-alist
	       '("\\*Geiser Debug\\*" display-buffer-no-window))

  (defun scheme-super-capf ()
    (setq-local completion-at-point-functions
		(list (cape-super-capf
		       (cape-company-to-capf #'company-yasnippet)
		       ;; #'cape-yasnippet
		       #'geiser-capf--for-filename
		       #'geiser-capf--for-module
		       #'geiser-capf--for-symbol)
		      #'cape-file)))

  (defun cory/run-geiser-p ()
    "Asks the user whether they want to start the geiser repl or not."
    (when (y-or-n-p "Start geiser repl?")
      (call-interactively 'geiser)))

  (defun cory/chicken-doc-look-up-manual (&optional arg)
    "Look up manual for symbol at point.
With prefix argument, ask for the lookup symbol (with completion)."
    (interactive "P")
    (unless (geiser-doc--manual-available-p)
      (error "No manual available"))
    (let ((symbol (or (and (not arg) (geiser--symbol-at-point))
                     (geiser-completion--read-symbol "Symbol: ")))
	  (current (buffer-name (current-buffer))))
      (eww (concat "http://api.call-cc.org/5/cdoc/?q="
		   (symbol-name symbol)
		   "&query-name=Look+up")
	   t)
      (when (get-buffer "*Chicken Documentation*")
	(kill-buffer "*Chicken Documentation*"))
      (rename-buffer "*Chicken Documentation*")
      (switch-to-buffer current)
      ;; (display-buffer "*Chicken Documentation*")
      (switch-to-buffer-other-window "*Chicken Documentation*"))))

(use-package geiser-chicken
  :after geiser)

;; (push ov macrursors--overlays)
;; (setq scheme-overlays nil)

;; (overlay-put (make-overlay (point-at-bol) (point-at-eol)) 'display "...")

;; (remove-overlays (point-min) (point-max) 'display "...")
