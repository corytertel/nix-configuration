;;; Scroll functions

(defun cory/scroll-down (arg)
  "Move cursor down half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (next-line dist)))

(defun cory/scroll-up (arg)
  "Move cursor up half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (previous-line dist)))

(put 'cory/scroll-down 'scroll-command t)
(put 'cory/scroll-up 'scroll-command t)

;;; Selection functions

(defmacro cory/def-bounds-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (car bounds))
	 (set-mark-command nil)
	 (goto-char (cdr bounds))))))

(defmacro cory/def-beginning-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (car bounds))))))

(defmacro cory/def-end-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (cdr bounds))))))

(cory/def-bounds-of-thing cory/mark-word 'word)
(cory/def-bounds-of-thing cory/mark-list 'list)
(cory/def-bounds-of-thing cory/mark-symbol 'symbol)
(cory/def-bounds-of-thing cory/mark-sexp 'sexp)
(cory/def-bounds-of-thing cory/mark-number 'number)
(cory/def-bounds-of-thing cory/mark-sentence 'sentence)
(cory/def-bounds-of-thing cory/mark-url 'url)
(cory/def-bounds-of-thing cory/mark-email 'email)
(cory/def-bounds-of-thing cory/mark-line 'line)

(cory/def-beginning-of-thing cory/beginning-of-word 'word)
(cory/def-beginning-of-thing cory/beginning-of-list 'list)
(cory/def-beginning-of-thing cory/beginning-of-symbol 'symbol)
(cory/def-beginning-of-thing cory/beginning-of-sexp 'sexp)
(cory/def-beginning-of-thing cory/beginning-of-number 'number)
(cory/def-beginning-of-thing cory/beginning-of-sentence 'sentence)
(cory/def-beginning-of-thing cory/beginning-of-url 'url)
(cory/def-beginning-of-thing cory/beginning-of-email 'email)
(cory/def-beginning-of-thing cory/beginning-of-line 'line)

(cory/def-end-of-thing cory/end-of-word 'word)
(cory/def-end-of-thing cory/end-of-list 'list)
(cory/def-end-of-thing cory/end-of-symbol 'symbol)
(cory/def-end-of-thing cory/end-of-sexp 'sexp)
(cory/def-end-of-thing cory/end-of-number 'number)
(cory/def-end-of-thing cory/end-of-sentence 'sentence)
(cory/def-end-of-thing cory/end-of-url 'url)
(cory/def-end-of-thing cory/end-of-email 'email)
(cory/def-end-of-thing cory/end-of-line 'line)

(defun cory/beginning-of-workspace ()
  "If a secondary selection is active, goto the beginning of it.
Else, goto the beginning of the buffer."
  (interactive)
  (if (and
      (secondary-selection-exist-p)
      (< (overlay-start mouse-secondary-overlay)
	 (overlay-end mouse-secondary-overlay))
      (<= (overlay-start mouse-secondary-overlay)
	 (point)
	 (overlay-end mouse-secondary-overlay)))
      (goto-char (overlay-start mouse-secondary-overlay))
    (goto-char (point-min))))

(defun cory/end-of-workspace ()
  "If a secondary selection is active, goto the end of it.
Else, goto the end of the buffer."
  (interactive)
  (if (and
      (secondary-selection-exist-p)
      (< (overlay-start mouse-secondary-overlay)
	 (overlay-end mouse-secondary-overlay))
      (<= (overlay-start mouse-secondary-overlay)
	 (point)
	 (overlay-end mouse-secondary-overlay)))
      (goto-char (- (overlay-end mouse-secondary-overlay) 1))
    (goto-char (point-max))))

;;; Mouse functions

(defun cory/mouse-goto-bol (click)
  "Move to beginning of line for mouse-1 click in left fringe."
  (interactive "e")
  (cory/mouse-goto-line click 'left))

(defun cory/mouse-goto-eol (click)
  "Move to beginning of line for mouse-1 click in left fringe."
  (interactive "e")
  (mouse-goto-line click 'right))

(defun cory/mouse-goto-line (click left/right)
  "Helper for `mouse-goto-(bol|eol)'."
  (let* ((posn      (event-start click))
         (click-pt  (posn-point posn))
         (window    (posn-window posn))
         (buf       (window-buffer window))
         (clicks    (if (eq mouse-selection-click-count-buffer buf)
                        (event-click-count click)
                      0)))
    (when (= clicks 1)                  ; No-op if not single-click.
      (with-current-buffer buf
        (goto-char click-pt)
        (if (eq 'left left/right)
            (line-beginning-position)
          (line-end-position))))))

;;; Misc functions

(defun cory/create-tmp-file ()
  (interactive)
  (find-file (concat temporary-file-directory (read-string "New tmp file:"))))

(defun cory/insert-space ()
  "Insert a space."
  (interactive)
  (self-insert-command 1 ? )
  (backward-char))

(defun cory/newline-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                            (and (looking-back ">" 1) (looking-at "<"))
                            (and (looking-back "(" 1) (looking-at ")"))
                            (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(defun cory/kill-from-start-of-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line)
  (indent-for-tab-command))

(defun cory/find-file ()
  (interactive)
  (let* ((file (car (find-file-read-args
		     "Find file: "
		     (confirm-nonexistent-file-or-buffer))))
	 (run (lambda (command)
		(async-shell-command
		 (concat command " " (shell-quote-argument (expand-file-name file)))))))
    (cond
     ((string-match-p ".*\.mp4$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mpeg$" file) (funcall run "vlc"))
     ((string-match-p ".*\.ogg$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mkv$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mov$" file) (funcall run "vlc"))
     ((string-match-p ".*\.webm$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mp3$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.opus$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.wav$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.weba$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.aac$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.doc$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.docx$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.odt$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.ppt$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.pptx$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.xcf$" file) (funcall run "gimp"))
     ((string-match-p ".*\.pdf$" file) (funcall run "zathura"))
     ((string-match-p ".*\.jpg$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.jpeg$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.png$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.xpm$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.xbm$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.svg$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.webp$" file) (funcall run "sxiv"))
     (t (find-file file)))))

(defun cory/join-next-line ()
  (interactive)
  (join-line 4))

(defun cory/describe-all-keymaps ()
  "Describe all keymaps in currently-defined variables."
  (interactive)
  (with-output-to-temp-buffer "*keymaps*"
    (let (symbs seen)
      (mapatoms (lambda (s)
                  (when (and (boundp s) (keymapp (symbol-value s)))
                    (push (indirect-variable s) symbs))))
      (dolist (keymap symbs)
        (unless (memq keymap seen)
          (princ (format "* %s\n\n" keymap))
          (princ (substitute-command-keys (format "\\{%s}" keymap)))
          (princ (format "\f\n%s\n\n" (make-string (min 80 (window-width)) ?-)))
          (push keymap seen))))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'describe-all-keymaps)))))

(defun cory/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun cory/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun cory/toggle-double-space ()
  (interactive)
  (setq line-spacing
	(if line-spacing
	    (default-font-height)
	  nil)))

(defun cory/yank (&optional arg)
  (interactive "*P")
  (when (and mark-active delete-selection-mode)
    (if (> (point) (mark))
	(delete-region (mark) (point))
      (delete-region (point) (mark))))
  (yank arg))

(defun cory/describe-symbol-at-point ()
  (interactive)
  (let* ((v-or-f (symbol-at-point))
	 (found (if v-or-f (cl-some (lambda (x) (funcall (nth 1 x) v-or-f))
				    describe-symbol-backends)))
	 (v-or-f (if found v-or-f (function-called-at-point)))
	 (found (or found v-or-f)))
    (if found
	(describe-symbol (symbol-name v-or-f))
      (message "Error: symbol not found"))))

;; Dictionary search dwim
(defun dictionary-search-dwim (&optional arg)
  "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
  (interactive "P")
  (if arg
      (dictionary-search nil)
    (if (use-region-p)
        (dictionary-search (buffer-substring-no-properties
                            (region-beginning)
                            (region-end)))
      (if (thing-at-point 'word)
          (dictionary-lookup-definition)
        (dictionary-search-dwim '(4))))))
