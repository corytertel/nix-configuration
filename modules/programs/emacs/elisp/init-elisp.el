;;; Lisps

;; Nicer elisp regex syntax highlighting
(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode) . easy-escape-minor-mode))

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

;;; Elisp

;; subr-x
(put 'if-let   'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)

;; emacs-lisp-mode
(defvar eval-print-as-comment-prefix ";;=> ")

(defun eval-print-as-comment (&optional arg)
  (interactive "P")
  (let ((start (point)))
    (eval-print-last-sexp arg)
    (save-excursion
      (goto-char start)
      (save-match-data
        (re-search-forward "[[:space:]\n]*" nil t)
        (insert eval-print-as-comment-prefix)))))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-;") 'eval-print-as-comment)

;; Make the faces of code only inside elisp code styled in such a way that
;; elisp appears like its "literally programmed"

;; (copy-face 'font-lock-comment-face
;; 	   'elisp-font-lock-comment-face)
;; (copy-face 'font-lock-comment-delimiter-face
;; 	   'elisp-font-lock-comment-delimiter-face)

;; (set-face-attribute 'elisp-font-lock-comment-face nil
;;                     :inherit 'variable-pitch)
;; (set-face-attribute 'elisp-font-lock-comment-delimiter-face nil
;;                     :inherit 'variable-pitch)

;; (add-hook
;;  'emacs-lisp-mode-hook
;;  (lambda ()
;;    (set (make-local-variable 'font-lock-comment-face)
;; 	'elisp-font-lock-comment-face)
;;    (set (make-local-variable 'font-lock-comment-delimiter-face)
;; 	'elisp-font-lock-comment-delimiter-face)))

;;; Eval result overlay

(defface eval-result-overlay-face
  '((t (:background "grey90" :box (:line-width -1 :color "yellow"))))
  "Face used to display evaluation results at the end of line."
  :group 'faces)

(defvar elisp--last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'elisp--last-post-command-position)

(defun cory/elisp--remove-overlay ()
  (unless (equal (point) elisp--last-post-command-position)
    (remove-overlays (point-min) (point-max) 'category 'eval-result))
  (setq elisp--last-post-command-position (point)))

(defun cory/eval-last-sexp (eval-last-sexp-arg-internal)
  (interactive "P")
  (let* ((res (prin1-to-string (eval-last-sexp eval-last-sexp-arg-internal)))
	 (pt (save-excursion (move-end-of-line nil) (point)))
	 (ov (make-overlay pt pt)))
    (overlay-put ov 'category 'eval-result)
    (overlay-put ov 'after-string
		 (concat " " (propertize (concat "=> " (if (string= "nil" res) "∅" res))
					 'face
					 'eval-result-overlay-face)))))

(global-set-key [remap eval-last-sexp] #'cory/eval-last-sexp)

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (add-to-list 'post-command-hook #'cory/elisp--remove-overlay)))
