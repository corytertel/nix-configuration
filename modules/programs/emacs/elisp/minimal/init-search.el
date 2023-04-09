(setq search-whitespace-regexp ".*"
      isearch-lax-whitespace t)

(defun cory/isearch-forward-dwim ()
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
               (isearch-resume region nil nil t region nil))
	   (call-interactively #'isearch-forward)))))

(defun cory/isearch-backward-dwim ()
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
	       (isearch-resume region nil nil nil region nil))
	   (call-interactively #'isearch-backward)))))

(defun cory/isearch-forward-regex-dwim ()
  (interactive)
  ;; Are we defining a macro?
  (cond (defining-kbd-macro
	  (call-interactively 'isearch-forward-regexp))
	;; Fall back to isearch.
	(t
	 ;; If region is active, prepopulate the isearch term.
	 (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
	     (let ((region (buffer-substring-no-properties (mark) (point))))
               (deactivate-mark)
               (isearch-resume region nil nil t region nil))
	   (call-interactively #'isearch-forward-regexp)))))

(defun cory/isearch-backward-regex-dwim ()
  (interactive)
  ;; Are we defining a macro?
  (cond (defining-kbd-macro
	  (call-interactively 'isearch-backward-regexp))
	;; Fall back to isearch.
	(t
	 ;; If region is active, prepopulate the isearch term.
	 (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
	     (let ((region (buffer-substring-no-properties (mark) (point))))
	       (deactivate-mark)
	       (isearch-resume region nil nil nil region nil))
	   (call-interactively #'isearch-backward-regexp)))))

(defun cory/isearch-call-macro-at-point ()
  (interactive)
  (if (equal isearch-string "")
      (setq isearch-error "No search string")
    (let ((string isearch-string)
	  (regexp isearch-regexp)
	  (word isearch-regexp-function)
	  (forward isearch-forward)
	  (message isearch-message)
	  (case-fold isearch-case-fold-search))
      (isearch-exit)
      (call-interactively #'kmacro-call-macro)
      (isearch-resume string regexp word forward message case-fold))))

(global-set-key [remap isearch-forward] #'cory/isearch-forward-dwim)
(global-set-key [remap isearch-backward] #'cory/isearch-backward-dwim)
(global-set-key [remap isearch-forward-regexp] #'cory/isearch-forward-regex-dwim)
(global-set-key [remap isearch-backward-regexp] #'cory/isearch-backward-regex-dwim)
(define-key isearch-mode-map [remap kmacro-end-or-call-macro] #'cory/isearch-call-macro-at-point)
