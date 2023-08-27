;;; Common Lisp

(use-package sly
  :commands (sly sly-connect)
  :bind
  (:map sly-mode-map
   ("C-x C-e" . sly-eval-last-expression))
  :init
  (setq-default sly-symbol-completion-mode nil)
  (setq inferior-lisp-program "sbcl")
  ;; (setq sly-default-lisp 'roswell)
  ;; (setq ros-config (concat user-emacs-directory
  ;;                          "ros-conf.lisp"))
  ;; (setq sly-lisp-implementations
  ;;       `((sbcl ("sbcl") :coding-system utf-8-unix)
  ;;         (ccl ("ccl") :coding-system utf-8-unix)
  ;;         (ecl ("ecl") :coding-system utf-8-unix)
  ;;         (roswell ("ros" "-Q" "-l" ,ros-config "run"))
  ;;         (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
  ;;               :coding-system utf-8-unix)))
  (setq sly-lisp-implementations
        `((sbcl ("sbcl") :coding-system utf-8-unix)))

  ;; (defun qlot-sly ()
  ;;   "Start a sly repl using qlot at the projects root"
  ;;   (interactive)
  ;;   (let ((dir (cdr (project-current))))
  ;;     (if (cd dir)
  ;;         (sly 'qlot)
  ;;       (error (format "Failed to cd to %s" dir)))))

  ;; (defun sly-critique-defun ()
  ;;   "Lint this file with lisp-critic"
  ;;   (interactive)
  ;;   ;; (sly-eval-async '(ql:quickload :lisp-critic))
  ;;   (let ((form (apply #'buffer-substring-no-properties
  ;;                      (sly-region-for-defun-at-point))))
  ;;     (sly-eval-async
  ;;      `(cl:format  "~a" (list ,(read form)))
  ;;      nil (sly-current-package))))

  ;; (defun sly-critique-file ()
  ;;   "Lint this file with lisp-critic"
  ;;   (interactive)
  ;;   (sly-eval-async '(ql:quickload :lisp-critic))
  ;;   (sly-eval-async `(lisp-critic:critique ,(buffer-file-name))))

  (add-hook 'sly-mode-hook (lambda ()
			     (setq-local completion-at-point-functions
					 (list (cape-super-capf
						(cape-company-to-capf #'company-yasnippet)
						;; #'cape-yasnippet
						#'sly-complete-filename-maybe
						#'sly-complete-symbol)))))
  :config

  ;;; Eval result

  (defface sly-result-overlay-face
    '((t (:inherit eval-result-overlay-face)))
    "Face used to display evaluation results at the end of line."
    :group 'sly)

  (defvar sly--last-post-command-position 0
    "Holds the cursor position from the last run of post-command-hooks.")

  (make-variable-buffer-local 'sly--last-post-command-position)

  (defvar sly-result-overlay-max-length 50)

  (defun sly--remove-overlay ()
    (unless (equal (point) sly--last-post-command-position)
      (remove-overlays (point-min) (point-max) 'category 'sly-result))
    (setq sly--last-post-command-position (point)))

  ;; Redefinition
  (defun sly-interactive-eval (string)
    "Read and evaluate STRING and print value in minibuffer.

A prefix argument(`C-u') inserts the result into the current
buffer. A negative prefix argument (`M--') will sends it to the
kill ring."
    (interactive (list (sly-read-from-minibuffer "SLY Eval: ")))
    (cl-case current-prefix-arg
      ((nil)
       (sly-eval-display string))
      ((-)
       (sly-eval-save string))
      (t
       (sly-eval-print string))))

  (defun sly-eval-display (string)
    "Eval STRING in Lisp; insert any output and the result at point."
    (sly-eval-async `(slynk:eval-and-grab-output ,string)
      (lambda (result)
	(cl-destructuring-bind (output value) result
	  (let* ((start (point))
		 (ppss (syntax-ppss))
		 (string-or-comment-p (or (nth 3 ppss) (nth 4 ppss)))
		 (res (concat output (if string-or-comment-p
					 ""
				       " => ")
			      value))
		 (str (substring res 0 (min sly-result-overlay-max-length (length res))))
		 (pt (save-excursion (move-end-of-line nil) (point)))
		 (ov (make-overlay pt pt)))
	    (overlay-put ov 'category 'sly-result)
	    (overlay-put ov 'after-string
			 (concat " " (propertize (if (string= "nil" str) "∅" str)
						 'face
						 'sly-result-overlay-face)))
	    (message value))))))

  (add-hook 'sly-mode-hook
	    (lambda () (add-to-list 'post-command-hook #'sly--remove-overlay))))

(use-package sly-asdf
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sly-repl-ansi-color
  :config
  (push 'sly-repl-ansi-color sly-contribs))

(use-package lisp-extra-font-lock)

;; From: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
- `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
- an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
- a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
	    (or (not (looking-at "\\sw\\|\\s_"))
	       (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
		   calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
	      (goto-char indent-point)
	      (skip-syntax-forward " ")
	      (not (looking-at ":")))
	    (save-excursion
	      (goto-char orig-point)
	      (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
					 'lisp-indent-function)
			   (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
		     (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))
