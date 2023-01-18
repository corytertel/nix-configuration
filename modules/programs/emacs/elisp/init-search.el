;;; Search functions

;; Phi search
(use-package phi-search)

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

(defun cory/isearch-string ()
  "Return the string to isearch for."
  (let ((search-args (s-split " " (car consult--line-history))))
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
  (let ((search-str (cory/isearch-string)))
    (isearch-resume search-str t nil t search-str t)))

(defun cory/isearch-backward-resume ()
  (interactive)
  (let ((search-str (cory/isearch-string)))
    (isearch-resume search-str t nil nil search-str t)))

(defun cory/consult-line-backward (&optional initial start)
  "Search for a matching line, but backwards.

Depending on the setting `consult-point-placement' the command jumps to the
beginning or the end of the first match on the line or the line beginning. The
default candidate is the non-empty line next to point. This command obeys
narrowing. Optional INITIAL input can be provided. The search starting point is
changed if the START prefix argument is set. The symbol at point and the last
`isearch-string' is added to the future history."
  (interactive (list nil (not (not current-prefix-arg))))
  (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
         (top (not (eq start consult-line-start-from-top)))
         (candidates (or (consult--with-increased-gc
                         (reverse (consult--line-candidates top curr-line)))
                        (user-error "No lines"))))
    (consult--read
     candidates
     :prompt (if top "Go to line (backward) from top: " "Go to line (backward): ")
     :annotate (consult--line-prefix curr-line)
     :category 'consult-location
     :sort nil
     :require-match t
     ;; Always add last isearch string to future history
     :add-history (list (thing-at-point 'symbol) isearch-string)
     :history '(:input consult--line-history)
     :lookup #'consult--line-match
     :default (car candidates)
     ;; Add isearch-string as initial input if starting from isearch
     :initial (or initial
                 (and isearch-mode
                    (prog1 isearch-string (isearch-done))))
     :state (consult--location-state candidates))))

(defun cory/visual-isearch-forward ()
  (interactive)
  (let ((completions-format 'one-column))
    (consult-line))
  (beginning-of-line)
  (cory/isearch-forward-resume))

(defun cory/visual-isearch-backward ()
  (interactive)
  (let ((completions-format 'one-column))
    (cory/consult-line-backward))
  (end-of-line)
  (cory/isearch-backward-resume))

(defun cory/search-forward-dwim ()
  (interactive)
  ;; Are we using multiple cursors?
  (cond ((and (boundp 'multiple-cursors-mode)
	    multiple-cursors-mode
	    (fboundp  'phi-search))
         (call-interactively 'phi-search))
        ;; Are we defining a macro?
        (defining-kbd-macro
          (call-interactively 'isearch-forward))
        ;; Fall back to isearch.
        (t
         ;; If region is active, prepopulate the isearch term.
	 (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
	     (let ((region (buffer-substring-no-properties (mark) (point))))
               (deactivate-mark)
	       (setq consult--line-history (cons region consult--line-history))
               (isearch-resume region nil nil t region nil))
	   (cory/visual-isearch-forward)))))

(defun cory/search-backward-dwim ()
  (interactive)
  ;; Are we using multiple cursors?
  (cond ((and (boundp 'multiple-cursors-mode)
            multiple-cursors-mode
            (fboundp  'phi-search-backward))
         (call-interactively 'phi-search-backward))
        ;; Are we defining a macro?
        (defining-kbd-macro
          (call-interactively 'isearch-backward))
        ;; Fall back to isearch.
        (t
         ;; If region is active, prepopulate the isearch term.
         (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
             (let ((region (buffer-substring-no-properties (mark) (point))))
               (deactivate-mark)
	       (setq consult--line-history (cons region consult--line-history))
               (isearch-resume region nil nil nil region nil))
           (cory/visual-isearch-backward)))))
