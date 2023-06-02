;;; company-yasnippet and it's dependencies extracted from company

(require 'cl-lib)
(require 'subr-x)
(require 'pcase)

(defvar company-tooltip-align-annotations nil)
(defvar-local company-backend nil)
(defvar-local company-callback nil)
(defvar-local company-prefix nil)
(defvar-local company-candidates nil)
(defvar-local company-candidates-length nil)
(defvar-local company-candidates-cache nil)
(defvar-local company-candidates-predicate nil)
(defvar-local company-common nil)
(defvar company-selection-default 0)
(defvar-local company-selection company-selection-default)
(defvar-local company-selection-changed nil)
(defvar-local company--manual-action nil)
(defvar-local company--manual-prefix nil)
(defvar-local company--point-max nil)
(defvar-local company-point nil)
(defvar company-timer nil)
(defvar company-async-wait 0.03)
(defvar company-async-timeout 2)
(defvar company--disabled-backends nil)
(defvar company-abort-manual-when-too-short nil)
(defvar company-minimum-prefix-length 3)
(defvar company-echo-timer nil)
(defvar company-abort-on-unique-match t)
(defvar-local company-my-keymap nil)
(defvar company-emulation-alist '((t . nil)))
(defvar company-async-redisplay-delay 0.005)
(defvar company-require-match 'company-explicit-action-p)
(defvar company-transformers nil)
(defvar company-insertion-on-trigger nil)
(defvar company-insertion-triggers '(?\  ?\) ?.))
(defvar company-idle-delay .2)
(defvar company-auto-update-doc nil)
(defvar company-tooltip-limit 10)
(defvar company--electric-saved-window-configuration nil)
(defvar company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend))

(defsubst company-strip-prefix (str)
  (substring str (length company-prefix)))

(defvar company-safe-backends
  '((company-abbrev . "Abbrev")
    (company-bbdb . "BBDB")
    (company-capf . "completion-at-point-functions")
    (company-clang . "Clang")
    (company-cmake . "CMake")
    (company-css . "CSS (obsolete backend)")
    (company-dabbrev . "dabbrev for plain text")
    (company-dabbrev-code . "dabbrev for code")
    (company-elisp . "Emacs Lisp (obsolete backend)")
    (company-etags . "etags")
    (company-files . "Files")
    (company-gtags . "GNU Global")
    (company-ispell . "Ispell")
    (company-keywords . "Programming language keywords")
    (company-nxml . "nxml (obsolete backend)")
    (company-oddmuse . "Oddmuse")
    (company-semantic . "Semantic")
    (company-tempo . "Tempo templates")))
(put 'company-safe-backends 'risky-local-variable t)

(defun company-safe-backends-p (backends)
  (and (consp backends)
     (not (cl-dolist (backend backends)
	  (unless (if (consp backend)
		      (company-safe-backends-p backend)
		    (assq backend company-safe-backends))
	    (cl-return t))))))

(defvar company-backends `(company-bbdb
			   ,@(unless (version<= "26" emacs-version)
                               (list 'company-nxml))
			   ,@(unless (version<= "26" emacs-version)
                               (list 'company-css))
			   company-semantic
			   company-cmake
			   company-capf
			   company-clang
			   company-files
			   (company-dabbrev-code company-gtags company-etags
						 company-keywords)
			   company-oddmuse company-dabbrev))

(put 'company-backends 'safe-local-variable 'company-safe-backends-p)

(defun company--posn-col-row (posn)
  (let ((col (car (posn-col-row posn)))
        ;; `posn-col-row' doesn't work well with lines of different height.
        ;; `posn-actual-col-row' doesn't handle multiple-width characters.
        (row (cdr (or (posn-actual-col-row posn)
                     ;; When position is non-visible for some reason.
                     (posn-col-row posn)))))
    (when (bound-and-true-p display-line-numbers)
      (cl-decf col (+ 2 (line-number-display-width))))
    (cons (+ col (window-hscroll)) row)))

(defun company--col-row (&optional pos)
  (company--posn-col-row (posn-at-point pos)))

(defun company--row (&optional pos)
  (cdr (company--col-row pos)))

(defun company-doc-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*company-documentation*")
    (erase-buffer)
    (fundamental-mode)
    (when string
      (save-excursion
        (insert string)
        (visual-line-mode)))
    (current-buffer)))

(defun company--force-sync (fun args backend)
  (let ((value (apply fun args)))
    (if (not (eq (car-safe value) :async))
        value
      (let ((res 'trash)
            (start (time-to-seconds)))
        (funcall (cdr value)
                 (lambda (result) (setq res result)))
        (while (eq res 'trash)
          (if (> (- (time-to-seconds) start) company-async-timeout)
              (error "Company: backend %s async timeout with args %s"
                     backend args)
            ;; XXX: Reusing the trick from company--fetch-candidates here
            ;; doesn't work well: sit-for isn't a good fit when we want to
            ;; ignore pending input (results in too many calls).
            ;; FIXME: We should deal with this by standardizing on a kind of
            ;; Future object that knows how to sync itself. In most cases (but
            ;; not all), by calling accept-process-output, probably.
            (sleep-for company-async-wait)))
        res))))

(defun company--prefix-str (prefix)
  (or (car-safe prefix) prefix))

(defun company--strip-duplicates (candidates)
  (let ((c2 candidates)
        (extras 'unk))
    (while c2
      (setcdr c2
              (let ((str (pop c2)))
                (while (let ((str2 (car c2)))
                         (if (not (equal str str2))
                             (progn
                               (setq extras 'unk)
                               nil)
                           (when (eq extras 'unk)
                             (setq extras (list (cons (company-call-backend
                                                       'annotation str)
                                                      (company-call-backend
                                                       'kind str)))))
                           (let ((extra2 (cons (company-call-backend
                                                'annotation str2)
                                               (company-call-backend
                                                'kind str2))))
                             (if (member extra2 extras)
                                 t
                               (push extra2 extras)
                               nil))))
                  (pop c2))
                c2)))))

(defun company--preprocess-candidates (candidates)
  (cl-assert (cl-every #'stringp candidates))
  (unless (company-call-backend 'sorted)
    (setq candidates (sort candidates 'string<)))
  (when (company-call-backend 'duplicates)
    (company--strip-duplicates candidates))
  candidates)

(defun company--multi-candidates-mapper (backend separate tag)
  (lambda (candidates)
    (when separate
      (let ((company-backend backend))
        (setq candidates
              (company--preprocess-candidates candidates))))
    (when tag
      (setq candidates
            (mapcar
             (lambda (str)
               (propertize str 'company-backend backend))
             candidates)))
    candidates))

(defun company--merge-async (pairs merger)
  (let ((async (cl-loop for pair in pairs
                        thereis
                        (eq :async (car-safe (car pair))))))
    (if (not async)
        (funcall merger (cl-loop for (val . mapper) in pairs
                                 collect (funcall mapper val)))
      (cons
       :async
       (lambda (callback)
         (let* (lst
                (pending (mapcar #'car pairs))
                (finisher (lambda ()
                            (unless pending
                              (funcall callback
                                       (funcall merger
                                                (nreverse lst)))))))
           (dolist (pair pairs)
             (push nil lst)
             (let* ((cell lst)
                    (val (car pair))
                    (mapper (cdr pair))
                    (this-finisher (lambda (res)
                                     (setq pending (delq val pending))
                                     (setcar cell (funcall mapper res))
                                     (funcall finisher))))
               (if (not (eq :async (car-safe val)))
                   (funcall this-finisher val)
                 (let ((fetcher (cdr val)))
                   (funcall fetcher this-finisher)))))))))))

(defun company--multi-backend-adapter-candidates (backends prefix separate)
  (let ((pairs (cl-loop for backend in backends
                        when (equal (company--prefix-str
                                     (let ((company-backend backend))
                                       (company-call-backend 'prefix)))
                                    prefix)
                        collect (cons (funcall backend 'candidates prefix)
                                      (company--multi-candidates-mapper
                                       backend
                                       separate
                                       ;; Small perf optimization: don't tag the
                                       ;; candidates received from the first
                                       ;; backend in the group.
                                       (not (eq backend (car backends))))))))
    (company--merge-async pairs (lambda (values) (apply #'append values)))))

(defun company-init-backend (backend)
  (and (symbolp backend)
     (not (fboundp backend))
     (ignore-errors (require backend nil t)))
  (cond
   ((symbolp backend)
    (condition-case err
        (progn
          (funcall backend 'init)
          (put backend 'company-init t))
      (error
       (put backend 'company-init 'failed)
       (unless (memq backend company--disabled-backends)
         (message "Company backend '%s' could not be initialized:\n%s"
                  backend (error-message-string err)))
       (cl-pushnew backend company--disabled-backends)
       nil)))
   ;; No initialization for lambdas.
   ((functionp backend) t)
   (t ;; Must be a list.
    (cl-dolist (b backend)
      (unless (keywordp b)
        (company-init-backend b))))))

(defun company--maybe-init-backend (backend)
  (or (not (symbolp backend))
     (eq t (get backend 'company-init))
     (unless (get backend 'company-init)
       (company-init-backend backend))))

(defun company--multi-backend-adapter (backends command &rest args)
  (let ((backends (cl-loop for b in backends
                           when (or (keywordp b)
                                   (company--maybe-init-backend b))
                           collect b))
        (separate (memq :separate backends)))

    (when (eq command 'prefix)
      (setq backends (butlast backends (length (member :with backends)))))

    (setq backends (cl-delete-if #'keywordp backends))

    (pcase command
      (`candidates
       (company--multi-backend-adapter-candidates backends (car args) separate))
      (`sorted separate)
      (`duplicates (not separate))
      ((or `prefix `ignore-case `no-cache `require-match)
       (let (value)
         (cl-dolist (backend backends)
           (when (setq value (company--force-sync
                              backend (cons command args) backend))
             (when (and (eq command 'ignore-case)
                      (eq value 'keep-prefix))
               (setq value t))
             (cl-return value)))))
      (_
       (let ((arg (car args)))
         (when (> (length arg) 0)
           (let ((backend (or (get-text-property 0 'company-backend arg)
                             (car backends))))
             (apply backend command args))))))))

(defun company-call-backend-raw (&rest args)
  (condition-case-unless-debug err
      (if (functionp company-backend)
          (apply company-backend args)
        (apply #'company--multi-backend-adapter company-backend args))
    (user-error (user-error
                 "Company: backend %s user-error: %s"
                 company-backend (error-message-string err)))
    (error (error "Company: backend %s error \"%s\" with args %s"
                  company-backend (error-message-string err) args))))

(defun company-call-backend (&rest args)
  (company--force-sync #'company-call-backend-raw args company-backend))

(defun company--good-prefix-p (prefix)
  (and (stringp (company--prefix-str prefix)) ;excludes 'stop
     (or (eq (cdr-safe prefix) t)
        (let ((len (or (cdr-safe prefix) (length prefix))))
          (if company--manual-prefix
              (or (not company-abort-manual-when-too-short)
                 ;; Must not be less than minimum or initial length.
                 (>= len (min company-minimum-prefix-length
                             (length company--manual-prefix))))
            (>= len company-minimum-prefix-length))))))

(defun company--sneaky-refresh ()
  (when company-candidates (company-call-frontends 'unhide))
  (let (inhibit-redisplay)
    (redisplay))
  (when company-candidates (company-call-frontends 'pre-command)))

(defun company-require-match-p ()
  (let ((backend-value (company-call-backend 'require-match)))
    (or (eq backend-value t)
       (and (not (eq backend-value 'never))
          (if (functionp company-require-match)
              (funcall company-require-match)
            (eq company-require-match t))))))

(defun company--flyspell-workaround-p ()
  ;; https://debbugs.gnu.org/23980
  (and (bound-and-true-p flyspell-mode)
     (version< emacs-version "27")))

(defun company--fetch-candidates (prefix)
  (let* ((non-essential (not (company-explicit-action-p)))
         (inhibit-redisplay t)
         (c (if (or company-selection-changed
                   ;; FIXME: This is not ideal, but we have not managed to deal
                   ;; with these situations in a better way yet.
                   (company-require-match-p))
                (company-call-backend 'candidates prefix)
              (company-call-backend-raw 'candidates prefix))))
    (if (not (eq (car c) :async))
        c
      (let ((res 'none))
        (funcall
         (cdr c)
         (lambda (candidates)
           (when (eq res 'none)
             (push 'company-foo unread-command-events))
           (setq res candidates)))
        (if (company--flyspell-workaround-p)
            (while (and (eq res 'none)
                      (not (input-pending-p)))
              (sleep-for company-async-wait))
          (while (and (eq res 'none)
                    (sit-for 0.5 t))))
        (while (member (car unread-command-events)
                       '(company-foo (t . company-foo)))
          (pop unread-command-events))
        (prog1
            (and (consp res) res)
          (setq res 'exited))))))

(defun company--transform-candidates (candidates)
  (let ((c candidates))
    (dolist (tr company-transformers)
      (setq c (funcall tr c)))
    c))

(defun company--postprocess-candidates (candidates)
  (when (or company-candidates-predicate company-transformers)
    (setq candidates (copy-sequence candidates)))
  (when company-candidates-predicate
    (setq candidates (cl-delete-if-not company-candidates-predicate candidates)))
  (company--transform-candidates candidates))

(defun company-calculate-candidates (prefix ignore-case)
  (let ((candidates (cdr (assoc prefix company-candidates-cache))))
    (or candidates
       (when company-candidates-cache
         (let ((len (length prefix))
               (completion-ignore-case ignore-case)
               prev)
           (cl-dotimes (i (1+ len))
             (when (setq prev (cdr (assoc (substring prefix 0 (- len i))
                                          company-candidates-cache)))
               (setq candidates (all-completions prefix prev))
               (cl-return t)))))
       ;; No cache match, call the backend.
       (let ((refresh-timer (run-with-timer company-async-redisplay-delay
                                            nil #'company--sneaky-refresh)))
         (setq candidates (company--preprocess-candidates
                           (company--fetch-candidates prefix)))
         ;; If the backend is synchronous, no chance for the timer to run.
         (cancel-timer refresh-timer)
         ;; Save in cache.
         (push (cons prefix candidates) company-candidates-cache)))
    ;; Only now apply the predicate and transformers.
    (company--postprocess-candidates candidates)))

(defun company--unique-match-p (candidates prefix ignore-case)
  (and candidates
     (not (cdr candidates))
     (eq t (compare-strings (car candidates) nil nil
                            prefix nil nil ignore-case))
     (not (eq (company-call-backend 'kind (car candidates))
            'snippet))))

(defun company-update-candidates (candidates)
  (setq company-candidates-length (length candidates))
  (if company-selection-changed
      ;; Try to restore the selection
      (let ((selected (and company-selection
                         (nth company-selection company-candidates))))
        (setq company-candidates candidates)
        (when selected
          (setq company-selection 0)
          (catch 'found
            (while candidates
              (let ((candidate (pop candidates)))
                (when (and (string= candidate selected)
                         (equal (company-call-backend 'annotation candidate)
                                (company-call-backend 'annotation selected)))
                  (throw 'found t)))
              (cl-incf company-selection))
            (setq company-selection company-selection-default
                  company-selection-changed nil))))
    (setq company-selection company-selection-default
          company-candidates candidates))
  ;; Calculate common.
  (let ((completion-ignore-case (company-call-backend 'ignore-case)))
    ;; We want to support non-prefix completion, so filtering is the
    ;; responsibility of each respective backend, not ours.
    ;; On the other hand, we don't want to replace non-prefix input in
    ;; `company-complete-common', unless there's only one candidate.
    (setq company-common
          (if (cdr company-candidates)
              (let ((common (try-completion "" company-candidates)))
                (when (string-prefix-p company-prefix common
                                       completion-ignore-case)
                  common))
            (car company-candidates)))))

(defun company-insertion-on-trigger-p (input)
  "Return non-nil if INPUT should trigger insertion.
For more details see `company-insertion-on-trigger' and
`company-insertion-triggers'."
  (and (if (functionp company-insertion-on-trigger)
         (funcall company-insertion-on-trigger)
       company-insertion-on-trigger)
     (if (functionp company-insertion-triggers)
         (funcall company-insertion-triggers input)
       (if (consp company-insertion-triggers)
           (memq (char-syntax (string-to-char input))
                 company-insertion-triggers)
         (string-match (regexp-quote (substring input 0 1))
                       company-insertion-triggers)))))

(defun company--insert-candidate (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    ;; XXX: Return value we check here is subject to change.
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (unless (equal company-prefix candidate)
        (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate)))))

(defun company-finish (result)
  (company--insert-candidate result)
  (company-cancel result))

(defun company-complete-selection ()
  "Insert the selected candidate."
  (interactive)
  (when (and (company-manual-begin) company-selection)
    (let ((result (nth company-selection company-candidates)))
      (company-finish result))))

(defun company--incremental-p ()
  (and (> (point) company-point)
     (> (point-max) company--point-max)
     (not (eq this-command 'backward-delete-char-untabify))
     (equal (buffer-substring (- company-point (length company-prefix))
                              company-point)
            company-prefix)))

(defun company--continue-failed (new-prefix)
  (cond
   ((and (or (not (company-require-match-p))
          ;; Don't require match if the new prefix
          ;; doesn't continue the old one, and the latter was a match.
          (not (stringp new-prefix))
          (<= (length new-prefix) (length company-prefix)))
       (member company-prefix company-candidates))
    ;; Last input was a success,
    ;; but we're treating it as an abort + input anyway,
    ;; like the `unique' case below.
    (company-cancel 'non-unique))
   ((company-require-match-p)
    ;; Wrong incremental input, but required match.
    (delete-char (- company-point (point)))
    (ding)
    (message "Matching input is required")
    company-candidates)
   (t (company-cancel))))

(defun company--continue ()
  (when (company-call-backend 'no-cache company-prefix)
    ;; Don't complete existing candidates, fetch new ones.
    (setq company-candidates-cache nil))
  (let* ((new-prefix (company-call-backend 'prefix))
         (ignore-case (company-call-backend 'ignore-case))
         (c (when (and (company--good-prefix-p new-prefix)
                     (setq new-prefix (company--prefix-str new-prefix))
                     (= (- (point) (length new-prefix))
                        (- company-point (length company-prefix))))
              (company-calculate-candidates new-prefix ignore-case))))
    (cond
     ((and company-abort-on-unique-match
         (company--unique-match-p c new-prefix ignore-case))
      ;; Handle it like completion was aborted, to differentiate from user
      ;; calling one of Company's commands to insert the candidate,
      ;; not to trigger template expansion, etc.
      (company-cancel 'unique))
     ((consp c)
      ;; incremental match
      (setq company-prefix new-prefix)
      (company-update-candidates c)
      c)
     ((and (characterp last-command-event)
         (company-insertion-on-trigger-p (string last-command-event)))
      ;; Insertion on trigger.
      (save-excursion
        (goto-char company-point)
        (company-complete-selection)
        nil))
     ((not (company--incremental-p))
      (company-cancel))
     (t (company--continue-failed new-prefix)))))

(defun company--should-complete ()
  (and (eq company-idle-delay 'now)
     (not (or buffer-read-only
           overriding-local-map))
     ;; Check if in the middle of entering a key combination.
     (or (equal (this-command-keys-vector) [])
        (not (keymapp (key-binding (this-command-keys-vector)))))
     (not (and transient-mark-mode mark-active))))

(defun company--show-doc-buffer ()
  "Show the documentation buffer for the selection."
  (let ((other-window-scroll-buffer)
        (selection (or company-selection 0)))
    (let* ((selected (nth selection company-candidates))
           (doc-buffer (or (company-call-backend 'doc-buffer selected)
                          (if company-auto-update-doc
                              (company-doc-buffer
                               (format "%s: No documentation available"
                                       selected))
                            (user-error "No documentation available"))))
           start)
      (when (consp doc-buffer)
        (setq start (cdr doc-buffer)
              doc-buffer (car doc-buffer)))
      (setq other-window-scroll-buffer (get-buffer doc-buffer))
      (let ((win (display-buffer doc-buffer t)))
        (set-window-start win (if start start (point-min)))))))

(defmacro company--electric-do (&rest body)
  (declare (indent 0) (debug t))
  `(when (company-manual-begin)
     (cl-assert (null company--electric-saved-window-configuration))
     (setq company--electric-saved-window-configuration (current-window-configuration))
     (let ((height (window-height))
           (row (company--row)))
       ,@body
       (and (< (window-height) height)
          (< (- (window-height) row 2) company-tooltip-limit)
          (recenter (- (window-height) row 2))))))

(defun company-show-doc-buffer (&optional toggle-auto-update)
  "Show the documentation buffer for the selection.
With a prefix argument TOGGLE-AUTO-UPDATE, toggle the value of
`company-auto-update-doc'.  When `company-auto-update-doc' is non-nil,
automatically show the documentation buffer for each selection."
  (interactive "P")
  (when toggle-auto-update
    (setq company-auto-update-doc (not company-auto-update-doc)))
  (if company-auto-update-doc
      (company--show-doc-buffer)
    (company--electric-do
      (company--show-doc-buffer))))
(put 'company-show-doc-buffer 'company-keep t)

(defun company-call-frontends (command)
  (when (and company-auto-update-doc
           (memq command '(update show)))
    (company-show-doc-buffer))
  (cl-loop for frontend in company-frontends collect
           (condition-case-unless-debug err
               (funcall frontend command)
             (error (error "Company: frontend %s error \"%s\" on command %s"
                           frontend (error-message-string err) command)))))

(defun company-explicit-action-p ()
  "Return whether explicit completion action was taken by the user."
  (or company--manual-action
     company-selection-changed))

(defun company--begin-new ()
  (let (prefix c)
    (cl-dolist (backend (if company-backend
                            ;; prefer manual override
                            (list company-backend)
                          company-backends))
      (setq prefix
            (if (or (symbolp backend)
                   (functionp backend))
                (when (company--maybe-init-backend backend)
                  (let ((company-backend backend))
                    (company-call-backend 'prefix)))
              (company--multi-backend-adapter backend 'prefix)))
      (when prefix
        (when (company--good-prefix-p prefix)
          (let ((ignore-case (company-call-backend 'ignore-case)))
            (setq company-prefix (company--prefix-str prefix)
                  company-backend backend
                  c (company-calculate-candidates company-prefix ignore-case))
            (cond
             ((and company-abort-on-unique-match
                 (company--unique-match-p c company-prefix ignore-case)
                 (if company--manual-action
                     ;; If `company-manual-begin' was called, the user
                     ;; really wants something to happen.  Otherwise...
                     (ignore (message "Sole completion"))
                   t))
              ;; ...abort and run the hooks, e.g. to clear the cache.
              (company-cancel 'unique))
             ((null c)
              (when company--manual-action
                (message "No completion found")))
             (t ;; We got completions!
              (when company--manual-action
                (setq company--manual-prefix prefix))
              (company-update-candidates c)
              (run-hook-with-args 'company-completion-started-hook
                                  (company-explicit-action-p))
              (company-call-frontends 'show)))))
        (cl-return c)))))

(defun company-ensure-emulation-alist ()
  (unless (eq 'company-emulation-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (cons 'company-emulation-alist
                (delq 'company-emulation-alist emulation-mode-map-alists)))))

(defun company--perform ()
  (cond
   (company-candidates
    (company--continue))
   ((company--should-complete)
    (company--begin-new)))
  (if (not company-candidates)
      (setq company-backend nil)
    (setq company-point (point)
          company--point-max (point-max))
    (company-ensure-emulation-alist)
    (company-call-frontends 'update)))

(defun company-echo-cancel (&optional unset)
  (when company-echo-timer
    (cancel-timer company-echo-timer))
  (when unset
    (setq company-echo-timer nil)))

(defun company-uninstall-map ()
  (setf (cdar company-emulation-alist) nil))

(defun company-enable-overriding-keymap (keymap)
  (company-uninstall-map)
  (setq company-my-keymap keymap))

(defun company-cancel (&optional result)
  (let ((prefix company-prefix)
        (backend company-backend))
    (setq company-backend nil
          company-prefix nil
          company-candidates nil
          company-candidates-length nil
          company-candidates-cache nil
          company-candidates-predicate nil
          company-common nil
          company-selection company-selection-default
          company-selection-changed nil
          company--manual-action nil
          company--manual-prefix nil
          company--point-max nil
          company-point nil)
    (when company-timer
      (cancel-timer company-timer))
    (company-echo-cancel t)
    (company-call-frontends 'hide)
    (company-enable-overriding-keymap nil)
    (when prefix
      (if (stringp result)
          (let ((company-backend backend))
            (run-hook-with-args 'company-completion-finished-hook result)
            (company-call-backend 'post-completion result))
        (run-hook-with-args 'company-completion-cancelled-hook result))
      (run-hook-with-args 'company-after-completion-hook result)))
  ;; Make return value explicit.
  nil)

(defun company-auto-begin ()
  (not company-candidates)
  (let ((company-idle-delay 'now))
    (condition-case-unless-debug err
        (let ((inhibit-quit nil))
          (company--perform)
          ;; Return non-nil if active.
          company-candidates)
      (error (message "Company: An error occurred in auto-begin")
             (message "%s" (error-message-string err))
             (company-cancel))
      (quit (company-cancel)))))

(defun company-manual-begin ()
  (interactive)
  (setq company--manual-action t)
  (unwind-protect
      (let ((company-minimum-prefix-length 0))
        (or company-candidates
           (company-auto-begin)))
    (unless company-candidates
      (setq company--manual-action nil))))

(defun company-begin-backend (backend &optional callback)
  "Start a completion at point using BACKEND."
  (interactive (let ((val (completing-read "Company backend: "
                                           obarray
                                           'functionp nil "company-")))
                 (when val
                   (list (intern val)))))
  (when (setq company-callback callback)
    (add-hook 'company-completion-finished-hook company-callback nil t))
  (add-hook 'company-completion-cancelled-hook 'company-remove-callback nil t)
  (add-hook 'company-completion-finished-hook 'company-remove-callback nil t)
  (setq company-backend backend)
  ;; Return non-nil if active.
  (or (company-manual-begin)
     (user-error "Cannot complete at point")))


(defun company-grab-symbol ()
  "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty string."
  (if (looking-at "\\_>")
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                (point)))
    (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
      "")))

(declare-function yas--table-hash "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas--template-content "yasnippet")
(declare-function yas--template-expand-env "yasnippet")
(declare-function yas--warning "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas--require-template-specific-condition-p "yasnippet")
(declare-function yas--template-can-expand-p "yasnippet")
(declare-function yas--template-condition "yasnippet")

(defvar company-yasnippet-annotation-fn
  (lambda (name)
    (concat
     (unless company-tooltip-align-annotations " -> ")
     name))
  "Function to format completion annotation.
It has to accept one argument: the snippet's name.")

(defun company-yasnippet--key-prefixes ()
  ;; Mostly copied from `yas--templates-for-key-at-point'.
  (defvar yas-key-syntaxes)
  (save-excursion
    (let ((original (point))
          (methods yas-key-syntaxes)
          prefixes
          method)
      (while methods
        (unless (eq method (car methods))
          (goto-char original))
        (setq method (car methods))
        (cond ((stringp method)
               (skip-syntax-backward method)
               (setq methods (cdr methods)))
              ((functionp method)
               (unless (eq (funcall method original)
                           'again)
                 (setq methods (cdr methods))))
              (t
               (setq methods (cdr methods))
               (yas--warning "Invalid element `%s' in `yas-key-syntaxes'" method)))
        (let ((prefix (buffer-substring-no-properties (point) original)))
          (unless (equal prefix (car prefixes))
            (push prefix prefixes))))
      prefixes)))

(defun company-yasnippet--candidates (prefix)
  ;; Process the prefixes in reverse: unlike Yasnippet, we look for prefix
  ;; matches, so the longest prefix with any matches should be the most useful.
  (cl-loop with tables = (yas--get-snippet-tables)
           for key-prefix in (company-yasnippet--key-prefixes)
           ;; Only consider keys at least as long as the symbol at point.
           when (>= (length key-prefix) (length prefix))
           thereis (company-yasnippet--completions-for-prefix prefix
                                                              key-prefix
                                                              tables)))

(defun company-yasnippet--completions-for-prefix (prefix key-prefix tables)
  (cl-mapcan
   (lambda (table)
     (let ((keyhash (yas--table-hash table))
           (requirement (yas--require-template-specific-condition-p))
           res)
       (when keyhash
         (maphash
          (lambda (key value)
            (when (and (stringp key)
                     (string-prefix-p key-prefix key))
              (maphash
               (lambda (name template)
                 (when (yas--template-can-expand-p
                        (yas--template-condition template) requirement)
                   (push
                    (propertize key
                                'yas-annotation name
                                'yas-template template
                                'yas-prefix-offset (- (length key-prefix)
                                                      (length prefix)))
                    res)))
               value)))
          keyhash))
       res))
   tables))

(defun company-yasnippet--doc (arg)
  (let ((template (get-text-property 0 'yas-template arg))
        (mode major-mode)
        (file-name (buffer-file-name)))
    (defvar yas-prompt-functions)
    (with-current-buffer (company-doc-buffer)
      (let ((buffer-file-name file-name))
        (yas-minor-mode 1)
        (setq-local yas-prompt-functions '(yas-no-prompt))
        (condition-case error
            (yas-expand-snippet (yas--template-content template))
          (error
           (message "%s"  (error-message-string error))))
        (delay-mode-hooks
          (let ((inhibit-message t))
            (if (eq mode 'web-mode)
                (progn
                  (setq mode 'html-mode)
                  (funcall mode))
              (funcall mode)))
          (ignore-errors (font-lock-ensure))))
      (current-buffer))))

(defun company-yasnippet (command &optional arg &rest ignore)
  "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='js-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)
"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-yasnippet))
    (prefix
     ;; Should probably use `yas--current-key', but that's bound to be slower.
     ;; How many trigger keys start with non-symbol characters anyway?
     (and (bound-and-true-p yas-minor-mode)
        (company-grab-symbol)))
    (annotation
     (funcall company-yasnippet-annotation-fn
              (get-text-property 0 'yas-annotation arg)))
    (candidates (company-yasnippet--candidates arg))
    (doc-buffer (company-yasnippet--doc arg))
    (no-cache t)
    (kind 'snippet)
    (post-completion
     (let ((template (get-text-property 0 'yas-template arg))
           (prefix-offset (get-text-property 0 'yas-prefix-offset arg)))
       (yas-expand-snippet (yas--template-content template)
                           (- (point) (length arg) prefix-offset)
                           (point)
                           (yas--template-expand-env template))))))
