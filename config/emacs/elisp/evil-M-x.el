;;; evil-M-x.el --- M-x integration into evil-ex  -*- lexical-binding: t -*-
;; File MUST be byte compiled for this function to work!!!
;; Else, it will exceed max recursion depth when evaluated iteratively.

(require 'evil)
(require 'evil-common)

;; The goal of this is to integrate : and M-x to be one.
;; Both emacs interactive commands will work, as well as vim commands.
;; Give evil's : emacs' M-x functionality.

;; The only modification here should be to not require a match for completing-read so non-emacs
;; commands "fall-through" to be executed by evil.
;; Also remove all traces of M-X.
(defun evil-read-extended-command-1 (&optional initial-input)
  (let ((buffer (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-self-insert-hook
                    (lambda ()
                      (setq execute-extended-command--last-typed
                            (minibuffer-contents)))
                    nil 'local)
          (setq-local minibuffer-default-add-function
	              (lambda ()
	                ;; Get a command name at point in the original buffer
	                ;; to propose it after M-n.
	                (let ((def
                               (with-current-buffer
			           (window-buffer (minibuffer-selected-window))
			         (and (commandp (function-called-at-point))
				    (format
                                     "%S" (function-called-at-point)))))
		              (all (sort (minibuffer-default-add-completions)
                                         #'string<)))
		          (if def
		              (cons def (delete def all))
		            all)))))
      ;; Read a string, completing from and restricting to the set of
      ;; extended-command history list.
      (completing-read
       (concat (cond
	        ((eq current-prefix-arg '-) "- ")
	        ((and (consp current-prefix-arg)
		    (eq (car current-prefix-arg) 4))
		 "C-u ")
	        ((and (consp current-prefix-arg)
		    (integerp (car current-prefix-arg)))
	         (format "%d " (car current-prefix-arg)))
	        ((integerp current-prefix-arg)
	         (format "%d " current-prefix-arg)))
               ":")
       (lambda (string pred action)
         (if (and suggest-key-bindings (eq action 'metadata))
	     '(metadata
	       (affixation-function . read-extended-command--affixation)
	       (category . command))
           (let ((pred
                  (if (memq action '(nil t))
                      ;; Exclude from completions obsolete commands
                      ;; lacking a `current-name', or where `when' is
                      ;; not the current major version.
                      (lambda (sym)
                        (let ((obsolete (get sym 'byte-obsolete-info)))
                          (and (funcall pred sym)
                             (or (equal string (symbol-name sym))
                                (not obsolete)
                                (and
                                 ;; Has a current-name.
                                 (functionp (car obsolete))
                                 ;; when >= emacs-major-version
                                 (condition-case nil
                                     (>= (car (version-to-list
                                              (caddr obsolete)))
                                        emacs-major-version)
                                   ;; If the obsoletion version isn't
                                   ;; valid, include the command.
                                   (error t)))))))
                    pred)))
             (complete-with-action action obarray string pred))))
       (lambda (sym)
         (and (commandp sym)
            (cond ((null read-extended-command-predicate))
                  ((functionp read-extended-command-predicate)
                   ;; Don't let bugs break completion; interpret
                   ;; them as the absence of a predicate.
                   (condition-case-unless-debug err
                       (funcall read-extended-command-predicate sym buffer)
                     (error (message "read-extended-command-predicate: %s: %s"
                                     sym (error-message-string err))))))))
       nil initial-input 'extended-command-history))))

(defun evil-read-extended-command (&optional initial-input)
  "Read command name to invoke via `evil-execute-extended-command'.
Use read-extended-command-predicate' to determine which commands
to include among completion candidates.

Returns a pair of the form (TYPE . COMMAND), where TYPE is the
type of command that should be invoked by `evil-execute-extended-command'
and command is the string of that command. TYPE is either `emacs' or `evil'.
`emacs' commands are functions that should be executed interactively, whereas
`evil' commands are strings that should be handled like ex in vim."
  (if-let ((buffer (current-buffer))
           (string (evil-read-extended-command-1 initial-input))
           (sym (intern-soft string))
           (x (commandp sym))
           (y (cond ((null read-extended-command-predicate))
                    ((functionp read-extended-command-predicate)
                     ;; Don't let bugs break completion; interpret
                     ;; them as the absence of a predicate.
                     (condition-case-unless-debug err
                         (funcall read-extended-command-predicate sym buffer)
                       (error (message "read-extended-command-predicate: %s: %s"
                                       sym (error-message-string err))))))))
      (cons 'emacs string)
    (cons 'evil string)))

;; Integrate evil-ex with emacs' native execute-extended-command
(evil-define-command evil-execute-extended-command (prefixarg &optional command-type command-name initial-input typed)
  "Read a command name, then read the arguments and call the command.
  To pass a prefix argument to the command you are
  invoking, give a prefix argument to `execute-extended-command'.

  This command provides completion when reading the command name.
  Which completion candidates are shown can be controlled by
  customizing `read-extended-command-predicate'.

The Ex command line is initialized with the value of INITIAL-INPUT. If
the command is called interactively the initial input depends on the
current state. In Normal state if a prefix count is given then the
initial input is \".,.+count\", otherwise it is empty. In Visual state
the initial input is the visual region '<,'> or `<,`>. The variable
`evil-ex-initial-input', if non-nil, is appended to the line."
  :keep-visual t
  :repeat abort
  (interactive
   (let* ((initial-input
           (let ((s (concat
                     (cond
                      ((and (evil-visual-state-p)
                          evil-ex-visual-char-range
                          (memq (evil-visual-type) '(inclusive exclusive)))
                       "`<,`>")
                      ((evil-visual-state-p) "'<,'>")
                      (current-prefix-arg
                       (let ((arg (prefix-numeric-value current-prefix-arg)))
                         (cond ((< arg 0) (setq arg (1+ arg)))
                               ((> arg 0) (setq arg (1- arg))))
                         (if (= arg 0) "." (format ".,.%+d" arg)))))
                     evil-ex-initial-input)))
             (unless (string= s "") s)))
          (execute-extended-command--last-typed nil)
          (command-input (evil-read-extended-command initial-input)))
     (list current-prefix-arg
           (car command-input)
           (cdr command-input)
           initial-input
           execute-extended-command--last-typed)))
  (cond
   ((eq command-type 'emacs)
    ;; Emacs<24 calling-convention was with a single `prefixarg' argument.
    (unless command-name
      (let ((current-prefix-arg prefixarg) ; for prompt
            (execute-extended-command--last-typed nil))
        (setq command-name (evil-read-extended-command))
        (setq typed execute-extended-command--last-typed)))
    (let* ((function (and (stringp command-name) (intern-soft command-name)))
           (binding (and suggest-key-bindings
        	       (not executing-kbd-macro)
        	       (where-is-internal function overriding-local-map t)))
           (delay-before-suggest 0)
           find-shorter shorter)
      (unless (commandp function)
        (error "`%s' is not a valid command name" command-name))
      ;; If we're executing a command that's remapped, we can't actually
      ;; execute that command with the keymapping we've found with
      ;; `where-is-internal'.
      (when (and binding (command-remapping function))
        (setq binding nil))
      ;; Some features, such as novice.el, rely on this-command-keys
      ;; including M-x COMMAND-NAME RET.
      (set--this-command-keys (concat ":" (symbol-name function) "\r"))
      (setq this-command function)
      ;; Normally `real-this-command' should never be changed, but here we really
      ;; want to pretend that : <cmd> RET is nothing more than a "key
      ;; binding" for <cmd>, so the command the user really wanted to run is
      ;; `function' and not `execute-extended-command'.
      (setq real-this-command function)
      (let ((prefix-arg prefixarg))
        (command-execute function 'record))
      ;; Ensure that we never have two of the suggest-binding timers in
      ;; flight.
      (when execute-extended-command--binding-timer
        (cancel-timer execute-extended-command--binding-timer))
      (when (and suggest-key-bindings
               (or binding
                  (and extended-command-suggest-shorter typed)))
        ;; If this command displayed something in the echo area, then
        ;; postpone the display of our suggestion message a bit.
        (setq delay-before-suggest
              (cond
               ((zerop (length (current-message))) 0)
               ((numberp suggest-key-bindings) suggest-key-bindings)
               (t 2)))
        (when (and extended-command-suggest-shorter
                 (not binding)
                 (not executing-kbd-macro)
                 (symbolp function)
                 (> (length (symbol-name function)) 2))
          ;; There's no binding for CMD.  Let's try and find the shortest
          ;; string to use in M-x.  But don't actually do anything yet.
          (setq find-shorter t))
        (when (or binding find-shorter)
          (setq execute-extended-command--binding-timer
                (run-at-time
                 delay-before-suggest nil
                 (lambda ()
                   ;; If the user has typed any other commands in the
                   ;; meantime, then don't display anything.
                   (when (eq function real-last-command)
                     ;; Find shorter string.
                     (when find-shorter
                       (while-no-input
                         ;; FIXME: Can be slow.  Cache it maybe?
                         (setq shorter (execute-extended-command--shorter
                                        (symbol-name function) typed))))
                     (when (or binding shorter)
                       (with-temp-message
                           (execute-extended-command--describe-binding-msg
                            function binding shorter)
                         (sit-for (if (numberp suggest-key-bindings)
                                      suggest-key-bindings
                                    2))))))))))))
   ((eq command-type 'evil)
    (unless (= (length command-name) 0) (evil-ex-execute command-name)))
   (t (error "Unknown command type %s" (symbol-name command-type)))))

;;; For each ex command, add it as an emacs command alias so it works with M-x
;; (add-hook
;;  'after-init-hook
;;  (lambda ()
;;    (dolist (pair evil-ex-commands)
;;      (unless (functionp (intern-soft (car pair)))
;;        (defalias (intern (car pair))
;;          (if (symbolp (cdr pair))
;;              (cdr pair)
;;            (cdr (assoc (cdr pair) evil-ex-commands))))))))

;; (dolist (pair evil-ex-commands)
;;   (unless (functionp (intern-soft (car pair)))
;;     (defalias (intern (car pair))
;;       (if (symbolp (cdr pair))
;;           (cdr pair)
;;         (cdr (assoc (cdr pair) evil-ex-commands))))))
