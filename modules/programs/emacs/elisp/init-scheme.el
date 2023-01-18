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

(use-package geiser
  :hook
  (geiser-mode . (lambda ()
		   (geiser-capf-setup nil)))
  (scheme-mode . scheme-super-capf)
  ;; (scheme-mode . cory/run-geiser-p)
  :custom
  (geiser-active-implementations '(chicken))
  :config
  (with-eval-after-load 'geiser-mode
    (dolist (bind '("C-c C-d d"
		    "C-c C-d C-d"
		    "C-c C-d i"
		    "C-c C-d TAB"))
      (define-key
	geiser-mode-map
	(kbd bind)
	#'cory/chicken-doc-look-up-manual))

    (define-key
      geiser-mode-map
      (kbd "C-.") nil))

  (defun scheme-super-capf ()
    (setq-local completion-at-point-functions
		(list (cape-super-capf
		       #'tempel-complete
		       #'geiser-capf--for-filename
		       #'geiser-capf--for-module
		       #'geiser-capf--for-symbol)
		      #'cape-dabbrev
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
