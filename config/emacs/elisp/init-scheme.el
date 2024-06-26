
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

(add-hook
 'scheme-mode-hook
 (lambda ()
   (put 'module 'scheme-indent-function 'scheme-module-indent)
   (put 'define-library 'scheme-indent-function 'scheme-module-indent)
   (put 'define-module 'scheme-indent-function 'scheme-module-indent)
   (put 'and-let* 'scheme-indent-function 1)
   (put 'parameterize 'scheme-indent-function 1)
   (put 'handle-exceptions 'scheme-indent-function 1)
   (put 'when 'scheme-indent-function 1)
   (put 'unless 'scheme-indent-function 1)
   (put 'match 'scheme-indent-function 1)))

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

(add-hook
 'scheme-mode-hook
 (lambda ()
   (interactive)
   (font-lock-add-keywords nil '(("set-car!" . 'font-lock-keyword-face)))
   (font-lock-add-keywords nil '(("set-cdr!" . 'font-lock-keyword-face)))
   (font-lock-add-keywords nil '(("define-message" . 'font-lock-keyword-face)))
   (font-lock-add-keywords nil '(("define-object" . 'font-lock-keyword-face)))))

(let ((str (make-string 200 ?a)))
  (substring str 0 (min 50 (length str))))

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
  ;; Eval result
  (defface geiser-result-overlay-face
    '((t (:inherit eval-result-overlay-face)))
    "Face used to display evaluation results at the end of line."
    :group 'geiser-faces)

  (defvar geiser--last-post-command-position 0
    "Holds the cursor position from the last run of post-command-hooks.")

  (make-variable-buffer-local 'geiser--last-post-command-position)

  (defvar geiser--eval-overlay-max-length 50)

  (defun geiser--remove-overlay ()
    (unless (equal (point) geiser--last-post-command-position)
      (remove-overlays (point-min) (point-max) 'category 'geiser-result))
    (setq geiser--last-post-command-position (point)))

  (defun cory/geiser-eval-last-sexp (print-to-buffer-p)
    (interactive "P")
    (let* ((res (geiser-eval-last-sexp print-to-buffer-p))
	   (str (substring res 0 (min geiser--eval-overlay-max-length (length res)))))
      (unless (string-match-p "ERROR: .*" str)
	(let* ((pt (save-excursion (move-end-of-line nil) (point)))
	       (ov (make-overlay pt pt)))
	  (overlay-put ov 'after-string
		       (concat " " (propertize str 'face 'geiser-result-overlay-face)))
	  (overlay-put ov 'category 'geiser-result)))))

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
    (define-key geiser-mode-map (kbd "C-M-i") nil)
    (define-key geiser-mode-map (kbd "C-x C-e") #'cory/geiser-eval-last-sexp)

    (add-hook 'geiser-mode-hook
	      (lambda () (add-to-list 'post-command-hook #'geiser--remove-overlay))))

  (with-eval-after-load 'geiser-doc
    (define-key geiser-doc-mode-map (kbd "N") nil)
    (define-key geiser-doc-mode-map (kbd "P") nil)
    (define-key geiser-doc-mode-map (kbd "H") #'geiser-doc-next-section)
    (define-key geiser-doc-mode-map (kbd "T") #'geiser-doc-previous-section)
    (define-key geiser-doc-mode-map (kbd "n") nil)
    (define-key geiser-doc-mode-map (kbd "p") nil)
    (define-key geiser-doc-mode-map (kbd "h") #'forward-button)
    (define-key geiser-doc-mode-map (kbd "t") #'backward-button)
    (define-key geiser-doc-mode-map (kbd "b") nil)
    (define-key geiser-doc-mode-map (kbd "d") #'geiser-doc-previous)
    (define-key geiser-doc-mode-map (kbd "f") nil)
    (define-key geiser-doc-mode-map (kbd "n") #'geiser-doc-next))

  ;; Suppress async-shell-command popup
  (add-to-list 'display-buffer-alist
	       '("\\*Geiser Debug\\*" display-buffer-no-window))

  (defun scheme-super-capf ()
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       ;; (cape-company-to-capf #'company-yasnippet)
		       #'yasnippet-capf
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
      (switch-to-buffer-other-window "*Chicken Documentation*")))

  (require 'cmuscheme)
  (defun cory/chicken-doc (&optional obtain-function)
    (interactive)
    (let ((func (funcall (or obtain-function 'current-word))))
      (when func
	(process-send-string (scheme-proc)
                             (format "(require-library chicken-doc) ,doc %S\n" func))
	(save-selected-window
          (select-window (display-buffer (get-buffer scheme-buffer) t))
          (goto-char (point-max))))))

  ;; Geiser REPL binds
  (with-eval-after-load 'geiser-repl
    (define-key geiser-repl-mode-map (kbd "M-i") #'cory/mark-list)
    (define-key geiser-repl-mode-map (kbd "C-j") nil)
    (define-key geiser-repl-mode-map (kbd "C-d") nil)))

(use-package geiser-chicken
  :after geiser)

;; (push ov macrursors--overlays)
;; (setq scheme-overlays nil)

;; (overlay-put (make-overlay (point-at-bol) (point-at-eol)) 'display "...")

;; (remove-overlays (point-min) (point-max) 'display "...")
