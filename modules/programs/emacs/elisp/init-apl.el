;;; APL

;; (use-package gnu-apl-mode)

;; TODO
;; - APL function navigation (able to grab from gnu-apl-mode)
;; - APL documentation lookup
;; - Send to repl (send to open dyalog vterm instance)
(use-package dyalog-mode
  :hook
  (dyalog-mode . (lambda () (set-input-method "dyalog-apl-prefix")))

  :bind
  (:map dyalog-mode-map
   ("C-c C-h" . cory/dyalog-symbols-help-toggle))

  :init
  (add-to-list 'auto-mode-alist '("\\.apl.?\\'" . dyalog-mode))

  :config
  (require 'quail)
  (quail-define-package
   "dyalog-apl-prefix" "Dyalog APL prefix" "Dyalog-Input" t
   "" nil t nil nil nil nil nil nil nil nil t)
  (quail-define-rules
   ("``" ?\N{DIAMOND OPERATOR})
   ("`1" ?\N{DIAERESIS})
   ("`2" ?\N{MACRON})
   ("`3" ?\N{LESS-THAN SIGN})
   ("`4" ?\N{LESS-THAN OR EQUAL TO})
   ("`5" ?\N{EQUALS SIGN})
   ("`6" ?\N{GREATER-THAN OR EQUAL TO})
   ("`7" ?\N{GREATER-THAN SIGN})
   ("`8" ?\N{NOT EQUAL TO})
   ("`9" ?\N{LOGICAL OR})
   ("`0" ?\N{LOGICAL AND})
   ("`-" ?\N{MULTIPLICATION SIGN})
   ("`=" ?\N{DIVISION SIGN})
   ("`~" ?\N{APL FUNCTIONAL SYMBOL QUAD DIAMOND})
   ("`!" ?\N{APL FUNCTIONAL SYMBOL I-BEAM})
   ("`@" ?\N{APL FUNCTIONAL SYMBOL DEL TILDE})
   ("`#" ?\N{APL FUNCTIONAL SYMBOL DEL STILE})
   ("`$" ?\N{APL FUNCTIONAL SYMBOL DELTA STILE})
   ("`%" ?\N{APL FUNCTIONAL SYMBOL CIRCLE STILE})
   ("`^" ?\N{APL FUNCTIONAL SYMBOL CIRCLE BACKSLASH})
   ("`&" ?\N{CIRCLED MINUS})
   ("`*" ?\N{APL FUNCTIONAL SYMBOL CIRCLE STAR})
   ("`(" ?\N{APL FUNCTIONAL SYMBOL DOWN CARET TILDE})
   ("`)" ?\N{APL FUNCTIONAL SYMBOL UP CARET TILDE})
   ("`_" ?\N{EXCLAMATION MARK})
   ("`+" ?\N{APL FUNCTIONAL SYMBOL QUAD DIVIDE})
   ("`q" ?\N{QUESTION MARK})
   ("`w" ?\N{APL FUNCTIONAL SYMBOL OMEGA})
   ("`e" ?\N{SMALL ELEMENT OF})
   ("`r" ?\N{APL FUNCTIONAL SYMBOL RHO})
   ("`t" ?\N{TILDE})
   ("`y" ?\N{UPWARDS ARROW})
   ("`u" ?\N{DOWNWARDS ARROW})
   ("`i" ?\N{APL FUNCTIONAL SYMBOL IOTA})
   ("`o" ?\N{WHITE CIRCLE})
   ("`p" ?\N{ASTERISK})
   ("`[" ?\N{LEFTWARDS ARROW})
   ("`]" ?\N{RIGHTWARDS ARROW})
   ("`\\" ?\N{RIGHT TACK})
   ("`E" ?\N{APL FUNCTIONAL SYMBOL EPSILON UNDERBAR})
   ("`T" ?\N{APL FUNCTIONAL SYMBOL TILDE DIAERESIS})
   ("`I" ?\N{APL FUNCTIONAL SYMBOL IOTA UNDERBAR})
   ("`O" ?\N{APL FUNCTIONAL SYMBOL CIRCLE DIAERESIS})
   ("`P" ?\N{APL FUNCTIONAL SYMBOL STAR DIAERESIS})
   ("`{" ?\N{APL FUNCTIONAL SYMBOL QUOTE QUAD})
   ("`}" ?\N{APL FUNCTIONAL SYMBOL ZILDE})
   ("`|" ?\N{LEFT TACK})
   ("`a" ?\N{APL FUNCTIONAL SYMBOL ALPHA})
   ("`s" ?\N{LEFT CEILING})
   ("`d" ?\N{LEFT FLOOR})
   ("`f" ?\N{LOW LINE})
   ("`g" ?\N{NABLA})
   ("`h" ?\N{INCREMENT})
   ("`j" ?\N{RING OPERATOR})
   ("`k" ?\N{APOSTROPHE})
   ("`l" ?\N{APL FUNCTIONAL SYMBOL QUAD})
   ("`;" ?\N{APL FUNCTIONAL SYMBOL DOWN TACK JOT})
   ("`'" ?\N{APL FUNCTIONAL SYMBOL UP TACK JOT})
   ("`J" ?\N{APL FUNCTIONAL SYMBOL JOT DIAERESIS})
   ("`K" ?\N{APL FUNCTIONAL SYMBOL QUAD EQUAL})
   ("`L" ?\N{APL FUNCTIONAL SYMBOL SQUISH QUAD})
   ("`:" ?\N{IDENTICAL TO})
   ("`\"" ?\N{NOT IDENTICAL TO})
   ("`z" ?\N{SUBSET OF})
   ("`x" ?\N{SUPERSET OF})
   ("`c" ?\N{INTERSECTION})
   ("`v" ?\N{UNION})
   ("`b" ?\N{UP TACK})
   ("`n" ?\N{DOWN TACK})
   ("`m" ?\N{VERTICAL LINE})
   ("`," ?\N{APL FUNCTIONAL SYMBOL UP SHOE JOT})
   ("`." ?\N{APL FUNCTIONAL SYMBOL BACKSLASH BAR})
   ("`/" ?\N{APL FUNCTIONAL SYMBOL SLASH BAR})
   ("`Z" ?\N{SUBSET OF OR EQUAL TO})
   ("`<" ?\N{APL FUNCTIONAL SYMBOL COMMA BAR})
   ("`>" ?\N{APL FUNCTIONAL SYMBOL DELTA UNDERBAR})
   ("`?" ?\N{APL FUNCTIONAL SYMBOL QUAD COLON}))

  (defvar cory/dyalog-symbols-help-buffer-name "*Dyalog Symbols*")

  (defun cory/dyalog--format (key char)
    (concat (char-to-string char) " "
	    (propertize key 'face 'font-lock-string-face)))

  (defvar cory/dyalog-help-syms
    (concat
     (cory/dyalog--format "`[" ?\N{LEFTWARDS ARROW}) "	"

     (cory/dyalog--format "`-" ?\N{MULTIPLICATION SIGN}) "	"
     (cory/dyalog--format "`=" ?\N{DIVISION SIGN}) "	"
     (cory/dyalog--format "`p" ?\N{ASTERISK}) "	"
     (cory/dyalog--format "`*" ?\N{APL FUNCTIONAL SYMBOL CIRCLE STAR}) "	"
     (cory/dyalog--format "`+" ?\N{APL FUNCTIONAL SYMBOL QUAD DIVIDE}) "	"
     (cory/dyalog--format "`o" ?\N{WHITE CIRCLE}) "	"
     (cory/dyalog--format "`_" ?\N{EXCLAMATION MARK}) "	"
     (cory/dyalog--format "`q" ?\N{QUESTION MARK}) "	"

     (cory/dyalog--format "`9" ?\N{LOGICAL OR}) "	"
     (cory/dyalog--format "`0" ?\N{LOGICAL AND}) "	"
     (cory/dyalog--format "`(" ?\N{APL FUNCTIONAL SYMBOL DOWN CARET TILDE}) "	"
     (cory/dyalog--format "`)" ?\N{APL FUNCTIONAL SYMBOL UP CARET TILDE}) "\n"

     (cory/dyalog--format "`s" ?\N{LEFT CEILING}) "	"
     (cory/dyalog--format "`d" ?\N{LEFT FLOOR}) "	"
     (cory/dyalog--format "`m" ?\N{VERTICAL LINE}) "	"
     (cory/dyalog--format "`b" ?\N{UP TACK}) "	"
     (cory/dyalog--format "`n" ?\N{DOWN TACK}) "	"
     (cory/dyalog--format "`|" ?\N{LEFT TACK}) "	"
     (cory/dyalog--format "`\\" ?\N{RIGHT TACK}) "	"

     (cory/dyalog--format "`1" ?\N{DIAERESIS}) "	"
     (cory/dyalog--format "`T" ?\N{APL FUNCTIONAL SYMBOL TILDE DIAERESIS}) "	"
     (cory/dyalog--format "`P" ?\N{APL FUNCTIONAL SYMBOL STAR DIAERESIS}) "	"
     (cory/dyalog--format "`j" ?\N{RING OPERATOR}) "	"
     (cory/dyalog--format "`J" ?\N{APL FUNCTIONAL SYMBOL JOT DIAERESIS}) "	"
     (cory/dyalog--format "`O" ?\N{APL FUNCTIONAL SYMBOL CIRCLE DIAERESIS}) "\n"

     (cory/dyalog--format "`5" ?\N{EQUALS SIGN}) "	"
     (cory/dyalog--format "`8" ?\N{NOT EQUAL TO}) "	"
     (cory/dyalog--format "`4" ?\N{LESS-THAN OR EQUAL TO}) "	"
     (cory/dyalog--format "`3" ?\N{LESS-THAN SIGN}) "	"
     (cory/dyalog--format "`7" ?\N{GREATER-THAN SIGN}) "	"
     (cory/dyalog--format "`6" ?\N{GREATER-THAN OR EQUAL TO}) "	"
     (cory/dyalog--format "`:" ?\N{IDENTICAL TO}) "	"
     (cory/dyalog--format "`\"" ?\N{NOT IDENTICAL TO}) "	"

     (cory/dyalog--format "`<" ?\N{APL FUNCTIONAL SYMBOL COMMA BAR}) "	"
     (cory/dyalog--format "`r" ?\N{APL FUNCTIONAL SYMBOL RHO}) "	"
     (cory/dyalog--format "`%" ?\N{APL FUNCTIONAL SYMBOL CIRCLE STILE}) "	"
     (cory/dyalog--format "`&" ?\N{CIRCLED MINUS}) "	"
     (cory/dyalog--format "`^" ?\N{APL FUNCTIONAL SYMBOL CIRCLE BACKSLASH}) "\n"

     (cory/dyalog--format "`y" ?\N{UPWARDS ARROW}) "	"
     (cory/dyalog--format "`u" ?\N{DOWNWARDS ARROW}) "	"
     (cory/dyalog--format "`z" ?\N{SUBSET OF}) "	"
     (cory/dyalog--format "`x" ?\N{SUPERSET OF}) "	"
     (cory/dyalog--format "`Z" ?\N{SUBSET OF OR EQUAL TO}) "	"
     (cory/dyalog--format "`L" ?\N{APL FUNCTIONAL SYMBOL SQUISH QUAD}) "	"
     (cory/dyalog--format "`$" ?\N{APL FUNCTIONAL SYMBOL DELTA STILE}) "	"
     (cory/dyalog--format "`#" ?\N{APL FUNCTIONAL SYMBOL DEL STILE}) "	"

     (cory/dyalog--format "`i" ?\N{APL FUNCTIONAL SYMBOL IOTA}) "	"
     (cory/dyalog--format "`I" ?\N{APL FUNCTIONAL SYMBOL IOTA UNDERBAR}) "	"
     (cory/dyalog--format "`e" ?\N{SMALL ELEMENT OF}) "	"
     (cory/dyalog--format "`E" ?\N{APL FUNCTIONAL SYMBOL EPSILON UNDERBAR}) "	"
     (cory/dyalog--format "`t" ?\N{TILDE}) "\n"

     (cory/dyalog--format "`v" ?\N{UNION}) "	"
     (cory/dyalog--format "`c" ?\N{INTERSECTION}) "	"

     (cory/dyalog--format "`/" ?\N{APL FUNCTIONAL SYMBOL SLASH BAR}) "	"
     (cory/dyalog--format "`." ?\N{APL FUNCTIONAL SYMBOL BACKSLASH BAR}) "	"

     (cory/dyalog--format "`{" ?\N{APL FUNCTIONAL SYMBOL QUOTE QUAD}) "	"
     (cory/dyalog--format "`l" ?\N{APL FUNCTIONAL SYMBOL QUAD}) "	"
     (cory/dyalog--format "`?" ?\N{APL FUNCTIONAL SYMBOL QUAD COLON}) "	"
     (cory/dyalog--format "`K" ?\N{APL FUNCTIONAL SYMBOL QUAD EQUAL}) "	"
     (cory/dyalog--format "`~" ?\N{APL FUNCTIONAL SYMBOL QUAD DIAMOND}) "	"
     (cory/dyalog--format "`!" ?\N{APL FUNCTIONAL SYMBOL I-BEAM}) "	"
     (cory/dyalog--format "`;" ?\N{APL FUNCTIONAL SYMBOL DOWN TACK JOT}) "	"
     (cory/dyalog--format "`'" ?\N{APL FUNCTIONAL SYMBOL UP TACK JOT}) "	"

     (cory/dyalog--format "`k" ?\N{APOSTROPHE}) "\n"

     (cory/dyalog--format "``" ?\N{DIAMOND OPERATOR}) "	"
     (cory/dyalog--format "`," ?\N{APL FUNCTIONAL SYMBOL UP SHOE JOT}) "	"
     (cory/dyalog--format "`]" ?\N{RIGHTWARDS ARROW}) "	"
     (cory/dyalog--format "`w" ?\N{APL FUNCTIONAL SYMBOL OMEGA}) "	"
     (cory/dyalog--format "`a" ?\N{APL FUNCTIONAL SYMBOL ALPHA}) "	"
     (cory/dyalog--format "`h" ?\N{INCREMENT}) "	"
     (cory/dyalog--format "`g" ?\N{NABLA}) "	"
     (cory/dyalog--format "`>" ?\N{APL FUNCTIONAL SYMBOL DELTA UNDERBAR}) "	"
     (cory/dyalog--format "`@" ?\N{APL FUNCTIONAL SYMBOL DEL TILDE}) "	"
     (cory/dyalog--format "`2" ?\N{MACRON}) "	"
     (cory/dyalog--format "`f" ?\N{LOW LINE}) "	"
     (cory/dyalog--format "`}" ?\N{APL FUNCTIONAL SYMBOL ZILDE})))

  (defun cory/dyalog-symbols-help-on ()
    (interactive)
    (unless (get-buffer cory/dyalog-symbols-help-buffer-name)
      (let ((cur-buffer-name (buffer-name)))
	(with-current-buffer (get-buffer-create cory/dyalog-symbols-help-buffer-name)
	  (insert cory/dyalog-help-syms)
	  (goto-char (point-min))
	  (local-set-key (kbd "q") 'kill-buffer-and-window)
	  (not-modified)
	  (read-only-mode 1)
	  (setq-local cursor-type nil))
	(pop-to-buffer cory/dyalog-symbols-help-buffer-name
		       '((display-buffer-in-side-window)
			 (side . top)
			 (window-parameters . ((no-other-window . nil)
					       (mode-line-format . none)))
			 (window-height . fit-window-to-buffer)))
	(select-window (get-buffer-window cur-buffer-name) nil))
      t))

  (defun cory/dyalog-symbols-help-off ()
    (interactive)
    (let ((cur-buffer-name (buffer-name)))
      (when-let ((buffer-to-kill (get-buffer cory/dyalog-symbols-help-buffer-name)))
	(when-let ((window-to-delete (get-buffer-window buffer-to-kill)))
	  (delete-window window-to-delete))
	(kill-buffer buffer-to-kill)
	t)))

  (defun cory/dyalog-symbols-help-toggle ()
    (interactive)
    (or (cory/dyalog-symbols-help-off) (cory/dyalog-symbols-help-on)))

  (defface apl-eval-result-overlay-face
    '((t (:background "grey90" :foreground "black")))
    "Face used to display evaluation results at the end of line."
    :group 'apl-faces)

  (defvar apl--last-post-command-position 0
    "Holds the cursor position from the last run of post-command-hooks.")
  (make-variable-buffer-local 'apl--last-post-command-position)

  (defun cory/apl--remove-overlay ()
    (unless (equal (point) apl--last-post-command-position)
      (remove-overlays (point-min) (point-max) 'category 'apl-eval-result))
    (setq apl--last-post-command-position (point)))

  (defvar cory/apl-startup-commands '(")load buildse"
				      "⎕IO←0")
    "Expressions to be run when a new workspace is created.")

  (defvar cory/apl-session-commands '("BUILD_SESSION 'US'"
				      "]box on -style=max")
    "Expressions to be run every time a workspace is loaded.")

  (defun cory/apl-eval-line ()
    "Evaluates the APL expression of the current line. Evaluates this expression alone,
meaning this function does not consider the current workspace."
    (interactive)
    (let* ((str
	    (string-trim-left
	     (shell-command-to-string
	      (concat
	       "dyalogscript <(echo -e \""
	       (seq-reduce (lambda (x y) (concat x y "\n")) cory/apl-startup-commands "")
	       (seq-reduce (lambda (x y) (concat x y "\n")) cory/apl-session-commands "")
	       "⎕ ← "
	       (string-trim-left
		(buffer-substring-no-properties (line-beginning-position) (line-end-position))
		" *⎕ ← ")
	       "\")"))
	     ".*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n"))
	   (pt (save-excursion (forward-line 1)
			       (move-beginning-of-line nil)
			       (point)))
	   (ov (make-overlay pt pt)))
      (overlay-put ov 'category 'apl-eval-result)
      (overlay-put ov 'after-string
		   (propertize str
			       'face
			       'apl-eval-result-overlay-face))))

  (defvar cory/apl-workspace-file nil)
  (make-variable-buffer-local 'cory/apl-workspace-file)

  (defun cory/apl--create-workspace (name)
    (shell-command-to-string
     (concat "dyalogscript <(echo -e \""
	     (seq-reduce (lambda (x y) (concat x y "\n")) cory/apl-startup-commands "")
	     ")save " name "\")")))

  (defun cory/apl-find-workspace ()
    (interactive)
    (let* ((workspace-file nil)
	   (workspace-dir (locate-dominating-file
			   default-directory
			   (lambda (dir)
			     (when-let ((files
					 (seq-filter (lambda (f) (string-match ".*\\.dws" f))
						     (file-name-all-completions "" dir)))
					(l (yes-or-no-p "Found workspace file, load?")))
			       (cond ((= 1 (length files)) (setq workspace-file (car files)))
				     (t (setq workspace-file
					      (completing-read
					       "Multiple workspace files found, please select one:"
					       files)))))))))
      (cond ((and (not workspace-dir)
		  (yes-or-no-p "Unable to locate a workspace file, would you like to create one?"))
	     (setq cory/apl-workspace-file
		   (concat
		    (expand-file-name default-directory)
		    (string-remove-suffix ".dws"
					  (read-string "Workspace name (default: workspace): "
						       nil nil "workspace"))
		    ".dws"))
	     (cory/apl--create-workspace cory/apl-workspace-file)
	     (message "Loading workspace file: %s" cory/apl-workspace-file))
	    (workspace-file
	     (setq cory/apl-workspace-file (expand-file-name
					    (concat workspace-dir workspace-file)))
	     (message "Loading workspace file: %s" cory/apl-workspace-file)))))

  (add-hook 'dyalog-mode-hook #'cory/apl-find-workspace)

  (defun cory/apl-load-workspace ()
    (interactive)
    (setq cory/apl-workspace-file
	  (expand-file-name
	   (car (find-file-read-args
		 "Find workspace: "
		 (confirm-nonexistent-file-or-buffer))))))

  (defun cory/apl-send-to-workspace (arg)
    (interactive "P")
    (cond ((not cory/apl-workspace-file)
	   (message "No workspace file loaded."))
	  (t
	   (let* ((beg (cond ((not mark-active) (line-beginning-position))
			     ((> (mark) (point)) (point))
			     (t (mark))))
		  (end (cond ((not mark-active) (line-end-position))
			     ((> (mark) (point)) (mark))
			     (t (point))))
		  (str
		   (string-trim
		    (shell-command-to-string
		     (concat
		      "dyalogscript <(echo -e \")load "
		      cory/apl-workspace-file
		      "\n"
		      (seq-reduce (lambda (x y) (concat x y "\n")) cory/apl-session-commands "")
		      ((lambda (str) (cond ((string-match "^ *).*" str) str)
				      ((string-match "^ *].*" str) str)
				      ((string-match "^ *⎕ *←.*" str) str)
				      (t (concat "⎕ ← " str))))
		       (buffer-substring-no-properties beg end))
		      "\n)save "
		      cory/apl-workspace-file
		      "\")"))
		    ".*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n.*\n"
		    "\n.*\n")))
	     (cond (arg
		    (move-end-of-line nil)
		    (newline)
		    (insert str))
		   (t
		    (let* ((pt (save-excursion (forward-line 1)
					       (move-beginning-of-line nil)
					       (point)))
			   (ov (make-overlay pt pt)))
		      (overlay-put ov 'category 'apl-eval-result)
		      (overlay-put ov 'after-string
				   (propertize str
					       'face
					       'apl-eval-result-overlay-face)))))))))

  (define-key dyalog-mode-map [remap eval-last-sexp] #'cory/apl-send-to-workspace)

  (add-hook 'dyalog-mode-hook
	    (lambda () (add-to-list 'post-command-hook #'cory/apl--remove-overlay))))
