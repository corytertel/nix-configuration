;; Nicer elisp regex syntax highlighting
(use-package easy-escape
  :hook ((emacs-lisp-mode lisp-mode) . easy-escape-minor-mode))

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

;; Completion
(defun cory/elisp-super-capf ()
  (setq-local completion-at-point-functions
	      (list (cape-capf-super
		     ;; (cape-company-to-capf #'company-yasnippet)
		     #'yasnippet-capf
		     #'elisp-completion-at-point))))

(add-hook 'emacs-lisp-mode-hook #'cory/elisp-super-capf)

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

(defvar eval-overlay-max-length 50)

(defun cory/elisp--remove-overlay ()
  (unless (equal (point) elisp--last-post-command-position)
    (remove-overlays (point-min) (point-max) 'category 'eval-result))
  (setq elisp--last-post-command-position (point)))

(defun cory/eval-last-sexp (eval-last-sexp-arg-internal)
  (interactive "P")
  (let* ((res (prin1-to-string (eval-last-sexp eval-last-sexp-arg-internal)))
	 (str (substring res 0 (min eval-overlay-max-length (length res))))
	 (pt (save-excursion (move-end-of-line nil) (point)))
	 (ov (make-overlay pt pt)))
    (overlay-put ov 'category 'eval-result)
    (overlay-put ov 'after-string
		 (concat " " (propertize (concat "=> " (if (string= "nil" str) "∅" str))
					 'face
					 'eval-result-overlay-face)))))

(setq eval-last-sexp-function #'cory/eval-last-sexp)

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (add-to-list 'post-command-hook #'cory/elisp--remove-overlay)))
