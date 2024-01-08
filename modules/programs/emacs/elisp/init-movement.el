;; Jump around
(use-package avy
  :ensure t
  :after devil
  :bind
  (("C-j" . cory/avy)
   :map isearch-mode-map
   ("C-j" . avy-isearch))

  :custom
  (avy-keys '(?a ?o ?e ?u ?d ?h ?t ?n ?s ?, ?p ?r))
  (avy-timeout-seconds 0.20)
  (avy-single-candidate-jump nil)
  (avy-dispatch-alist '(([?. ?x] . avy-action-kill-move)
			([?. ?k] . avy-action-kill-line)
			([?. ?, ?k] . avy-action-kill-from-start-of-line)
			([?. ?, ?, ?k] . avy-action-kill-whole-line)
			;; (?j . avy-action-teleport)
			([?. ?j] . avy-action-teleport-whole-line)
			([?. ?, ? ] . avy-action-mark)
			([?. ? ] . avy-action-mark-to-char)
			([?. ?c] . avy-action-copy)
			([?. ?v] . avy-action-yank)
			([?. ?, ?$] . avy-action-jinx)
			([?. ?, ?z] . avy-action-zap-to-char)
			;; (?  . avy-action-embark)
			;; (?= . avy-action-define)
			([?. ?i] . avy-action-helpful)
			;; (?x . avy-action-exchange)
			))
  (avy-escape-chars '(?\e ?\C-g ?g))

  :config
  (defun cory/avy ()
    (interactive)
    (call-interactively #'avy-goto-word-1))

  ;; Redefine to make Avy work with Devil

  (defun avy-read (tree display-fn cleanup-fn)
    "Select a leaf from TREE using consecutive `read-key'.

DISPLAY-FN should take CHAR and LEAF and signify that LEAFs
associated with CHAR will be selected if CHAR is pressed.  This is
commonly done by adding a CHAR overlay at LEAF position.

CLEANUP-FN should take no arguments and remove the effects of
multiple DISPLAY-FN invocations."
    (catch 'done
      (setq avy-current-path "")
      (while tree
	(let ((avy--leafs nil))
          (avy-traverse tree
			(lambda (path leaf)
                          (push (cons path leaf) avy--leafs)))
          (dolist (x avy--leafs)
            (funcall display-fn (car x) (cdr x))))
	(let* ((char-seq (devil--aget 'key (devil--read-key devil-prompt "")))
	       (char (aref char-seq 0))
               window
               branch)
          (funcall cleanup-fn)
          (if (setq window (avy-mouse-event-window char))
              (throw 'done (cons char window))
            (if (setq branch (assoc char tree))
		(progn
                  ;; Ensure avy-current-path stores the full path prior to
                  ;; exit so other packages can utilize its value.
                  (setq avy-current-path
			(concat avy-current-path (string (avy--key-to-char char))))
                  (if (eq (car (setq tree (cdr branch))) 'leaf)
                      (throw 'done (cdr tree))))
              (progn
		(funcall avy-handler-function (if (length= char-seq 1) char char-seq)))))))))

  ;; To repeat Avy actions
  ;; Taken from https://ag91.github.io/blog/2022/04/20/repeat-with-me-avy-actions-are-awesome/

  (defun cory/avy--read-candidates ()
    (let ((re-builder #'regexp-quote)
          break overlays regex)
      (unwind-protect
          (progn
            (avy--make-backgrounds
             (avy-window-list))
            ;; Unhighlight
            (dolist (ov overlays)
              (delete-overlay ov))
            (setq overlays nil)
            ;; Highlight
            (when (>= (length avy-text) 1)
              (let ((case-fold-search
                     (or avy-case-fold-search (string= avy-text (downcase avy-text))))
                    found)
		(avy-dowindows current-prefix-arg
                  (dolist (pair (avy--find-visible-regions
				 (window-start)
				 (window-end (selected-window) t)))
                    (save-excursion
                      (goto-char (car pair))
                      (setq regex (funcall re-builder avy-text))
                      (while (re-search-forward regex (cdr pair) t)
			(unless (not (avy--visible-p (1- (point))))
                          (let* ((idx (if (= (length (match-data)) 4) 1 0))
				 (ov (make-overlay
                                      (match-beginning idx) (match-end idx))))
                            (setq found t)
                            (push ov overlays)
                            (overlay-put
                             ov 'window (selected-window))
                            (overlay-put
                             ov 'face 'avy-goto-char-timer-face)))))))
		;; No matches at all, so there's surely a typo in the input.
		(unless found (beep))))
            (nreverse (mapcar (lambda (ov)
				(cons (cons (overlay-start ov)
                                            (overlay-end ov))
                                      (overlay-get ov 'window)))
                              overlays)))
	(dolist (ov overlays)
          (delete-overlay ov))
	(avy--done))))

  (defun cory/avy-repeat-action ()
    (setq avy--old-cands (cory/avy--read-candidates))
    (avy-process avy--old-cands))

  ;; Most of the below was copied from and/or inspired by
  ;; https://github.com/xl666/avy-conf/blob/main/avy.org

  ;; Need to use C- now because all letters of the alphabet are taken
  ;; Rebind default avy actions
  ;; (setf (alist-get ?\C-y avy-dispatch-alist) 'avy-action-yank
  ;; 	(alist-get ?\M-w avy-dispatch-alist) 'avy-action-copy
  ;; 	(alist-get ?\C-k avy-dispatch-alist) 'avy-action-kill-move
  ;; 	(alist-get (kbd "<C-m>") avy-dispatch-alist) 'avy-action-teleport)

  ;; Avy helper functions for both generic and complex avy actions
  (defun avy-generic-command-action (action-f)
    "Excecutes action-f at point and stays"
    (save-excursion
      (goto-char pt)
      (funcall action-f))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-generic-command-action-no-stay (action-f)
    "Excecutes action-f at point and returns to original position"
    (goto-char pt)
    (funcall action-f)
    t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char (+ 1 pt)))

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point)))

  (defun avy-action-jinx (pt)
    (let ((on (or jinx-mode global-jinx-mode)))
      (avy-generic-command-action #'jinx-correct)
      (unless on (jinx-mode -1))))

  (defun avy-action-kill-line (pt)
    (goto-char pt)
    (kill-line)
    t)

  (defun avy-action-kill-from-start-of-line (pt)
    (goto-char pt)
    (cory/kill-from-start-of-line)
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    (cory/avy-repeat-action)
    t)

  (defun avy-action-define (pt)
    (save-excursion
      (goto-char pt)
      (dictionary-search-dwim)))

  (defun avy-action-embark (pt)
    (unwind-protect (avy-generic-command-action #'embark-act)) t)
  ;; (setf (alist-get (kbd "<C-i>") avy-dispatch-alist) 'avy-action-embark)

  ;; Show help in avy dispatch with ?
  (defun avy-show-dispatch-help ()
    (let* ((len (length "avy-action-"))
           (fw (frame-width))
           (raw-strings (mapcar
			 (lambda (x)
			   (format "%2s: %-19s"
				   (propertize
				    (char-to-string (car x))
				    'face 'aw-key-face)
				   (substring (symbol-name (cdr x)) len)))
			 avy-dispatch-alist))
           (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
           (strings-len (length raw-strings))
           (per-row (floor fw max-len))
           display-strings)
      (cl-loop for string in raw-strings
               for N from 1 to strings-len do
               (push (concat string " ") display-strings)
               (when (= (mod N per-row) 0) (push "\n" display-strings)))
      (message "%s" (apply #'concat (nreverse display-strings))))))

;; (use-package avy-zap
;;   :bind
;;   (("M-z" . avy-zap-to-char-dwim)
;;    ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package ace-link
  :bind
  (("M-o" . ace-link))
  :init
  ;; Binds `o' to ace-link in the supported modes
  (ace-link-setup-default))
