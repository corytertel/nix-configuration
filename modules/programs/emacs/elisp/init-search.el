;;; Search functions

(require 's)

(defun cory/regex-combos (l)
  "Return a list of regexes that will match combitations of the strings in `L'."
  (if (not (cdr l))
      l
    (cl-reduce
     #'append
     (mapcar
      (lambda (e)
	(mapcar
	 (lambda (f) (concat e ".*" f))
	 (cory/regex-combos (remove e l))))
      l))))

(defun cory/isearch-string (str)
  "Return the string to isearch for using STR."
  (let* ((len (length str))
	 (last-char (aref str (- len 1)))
	 (history (if (> last-char 127)
		      (substring str 0 (- len 1))
		    str))
	 (search-args (s-split " " history)))
    (if (cdr search-args)
	(concat
	 "^.*\\("
	 (cl-reduce
	  (lambda (x y) (concat x "\\|" y))
	  (cory/regex-combos search-args))
	 "\\).*$")
      (car search-args))))

(defun cory/isearch-forward-resume ()
  (interactive)
  (let ((search-str (cory/isearch-string (car search-ring))))
    (isearch-resume search-str t nil t search-str t)))

(defun cory/isearch-backward-resume ()
  (interactive)
  (let ((search-str (cory/isearch-string (car search-ring))))
    (isearch-resume search-str t nil nil search-str t)))

(defun cory/visual-isearch-forward ()
  (interactive)
  (let ((search-str (cory/isearch-string (read-string "I-search: "))))
    (isearch-resume search-str t nil t search-str t)))

(defun cory/visual-isearch-backward ()
  (interactive)
  (let ((search-str (cory/isearch-string (read-string "I-search backward: "))))
    (isearch-resume search-str t nil nil search-str t)))

(defun cory/search-forward-dwim ()
  (interactive)
  ;; Are we defining a macro?
  (cond (defining-kbd-macro
	  (call-interactively 'isearch-forward))
	;; Fall back to isearch.
	(t
	 ;; If region is active, prepopulate the isearch term.
	 (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
	     (let ((region (buffer-substring-no-properties (mark) (point))))
               (deactivate-mark)
	       ;; (setq consult--line-history (cons region consult--line-history))
               (isearch-resume region nil nil t region nil))
	   (cory/visual-isearch-forward)))))

(defun cory/search-backward-dwim ()
  (interactive)
  ;; Are we defining a macro?
  (cond (defining-kbd-macro
	  (call-interactively 'isearch-backward))
	;; Fall back to isearch.
	(t
	 ;; If region is active, prepopulate the isearch term.
	 (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
	     (let ((region (buffer-substring-no-properties (mark) (point))))
	       (deactivate-mark)
	       ;; (setq consult--line-history (cons region consult--line-history))
	       (isearch-resume region nil nil nil region nil))
	   (cory/visual-isearch-backward)))))

(setq search-whitespace-regexp ".*")
(setq isearch-lax-whitespace t)
