;; Jump around
(use-package avy
  :ensure t
  :bind
  (;; ("M-g g" . avy-goto-line)
   ;; ("M-g c" . avy-goto-char-in-line)
   ;; ("M-g m" . avy-move-line)
   ("<C-m>" . avy-goto-char-timer)
   ("C-S-m" . avy-pop-mark)
   ;; ("C-j" . avy-goto-end-of-line-num)
   ;; ("C-S-j" . avy-goto-line-num)
   ("C-M-s" . isearch-forward-other-window)
   ("C-M-r" . isearch-backward-other-window)
   :map isearch-mode-map
   ("<C-m>" . avy-isearch))

  :custom
  ;; (setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
  ;;                     ?a ?s ?d ?f ?g ?h ?j
  ;;                     ?k ?l ?' ?x ?c ?v ?b
  ;;                     ?n ?, ?/))
  ;; (avy-keys (append (string-to-list "aoehsfb")
  ;; 		    (string-to-list "ulrpdc")
  ;; 		    (string-to-list "qjvg")
  ;; 		    (string-to-list (upcase "aoehsfb"))
  ;; 		    (string-to-list (upcase "ulrpdc"))
  ;; 		    (string-to-list (upcase "qjvg"))
  ;; 		    (number-sequence ?, ?')))
  ;; (avy-keys (nconc (number-sequence ?a ?z)
  ;; 		   (number-sequence ?A ?Z)))
  (avy-keys (append (string-to-list "atenisubopyflmc")
		    (string-to-list (upcase "atenisubopyflmc"))))
  (avy-timeout-seconds 0.25)

  :config

  ;; Most of the below was copied from and/or inspired by
  ;; https://github.com/xl666/avy-conf/blob/main/avy.org

  ;; Need to use C- now because all letters of the alphabet are taken
  ;; Rebind default avy actions
  (setf (alist-get ?\C-y avy-dispatch-alist) 'avy-action-yank
	(alist-get ?\M-w avy-dispatch-alist) 'avy-action-copy
	(alist-get ?\C-k avy-dispatch-alist) 'avy-action-kill-move
	(alist-get (kbd "<C-m>") avy-dispatch-alist) 'avy-action-teleport)

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

  ;;; Actions from "Avy can do anything"

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char (+ 1 pt)))
  (setf (alist-get 67108896 avy-dispatch-alist) 'avy-action-mark-to-char) ; C-SPC

  (defun avy-action-helpful (pt)
    (avy-generic-command-action #'helpful-at-point))
  (setf (alist-get ?\C-h avy-dispatch-alist) 'avy-action-helpful)

  (defun avy-action-flyspell (pt)
    (avy-generic-command-action #'flyspell-auto-correct-word))
  (setf (alist-get 67108923 avy-dispatch-alist) 'avy-action-flyspell) ; C-;

  (defun avy-action-kill-whole-line (pt)
    (avy-generic-command-action #'kill-whole-line))
  (setf (alist-get (kbd "C-M-k") avy-dispatch-alist) 'avy-action-kill-whole-line)

  (defun avy-action-copy-whole-line (pt)
    (avy-generic-command-action (lambda () (cl-destructuring-bind (start . end)
					  (bounds-of-thing-at-point 'line)
					(copy-region-as-kill start end)))))
  (setf (alist-get (kbd "C-M-w") avy-dispatch-alist) 'avy-action-copy-whole-line)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank)) t)
  (setf (alist-get (kbd "C-M-y") avy-dispatch-alist) 'avy-action-yank-whole-line)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (setf (alist-get (kbd "C-M-t") avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun avy-action-define (pt)
    (avy-generic-command-action #'dictionary-search-dwim))
  (setf (alist-get (kbd "C-=") avy-dispatch-alist) 'avy-action-define)

  (defun avy-action-embark (pt)
    (unwind-protect (avy-generic-command-action #'embark-act)) t)
  (setf (alist-get (kbd "<C-i>") avy-dispatch-alist) 'avy-action-embark)

  ;;; New behavior

  ;; Open org link (only relevant for org files)
  (defun avy-action-open-at-point (pt)
    (goto-char pt)
    (org-open-at-point)
    t)
  (setf (alist-get ?\C-o avy-dispatch-alist) 'avy-action-open-at-point)

  ;; Clone line below
  (defun avy-action-clone-line (pt)
    (goto-char pt)
    (move-beginning-of-line 1)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end))
    (yank)
    (indent-for-tab-command)
    t)
  (setf (alist-get ?\C-l avy-dispatch-alist) 'avy-action-clone-line)

  ;;; Regions

  ;; - The idea is to be able to act in arbitrary regions without the need of manually marking a region
  ;; - It relies on two basic operations:
  ;;   - First mark the beginning of the region with a set-point action
  ;;   - Then apply a region action selecting the end of the region
  ;; - Region actions are the same as Original actions but for regions
  ;; - Region actions take the original code of avy actions as much as possible
  ;; - A necessary hack is to simulate region selection instead of using direct functions like `copy-region-as-kill' as those functions do not allow to manipulate regions if parenthesis or other syntax elements are not balanced. This has a weird behavior in modes like emacs-lisp so I’m not sure if it is a syntax problem

  ;; set-point-action
  (defun avy-action-mark-point (pt)
    "Sets a point for other commands"
    (setq my-avy-point pt)
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    (message "Point set!"))
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-mark-point)

  ;; Common region functions
  (defun avy--quick-mark-region (pt)
    "Intermediate function to mark regions, used in region actions"
    (when (> my-avy-point pt)
      (progn
	(setf aux pt)
	(setf pt my-avy-point)
	(setf my-avy-point aux)))
    (goto-char my-avy-point)
    (set-mark my-avy-point)
    (activate-mark)
    (goto-char (+ 1 pt))
    (setq my-avy-point nil))

  (defun avy--return-point-region-action ()
    "Makes sure that the point returns to its original place even if it is in another window"
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat))))

  (defun avy--check-for-region-errors ()
    "Cheks if set point action was previously made, cleans action otherwise"
    (progn (message "No point set")
           (avy--return-point-region-action)
           nil))

  ;; Region actions
  (defun avy-action-copy-region (pt)
    "Copy region and stays"
    (if my-avy-point
	(progn
          (save-excursion
            (avy--quick-mark-region pt)
            (call-interactively 'kill-ring-save))
          (avy--return-point-region-action)
          (message "Copied: %s" (current-kill 0))
          t)
      (avy--check-for-region-errors)))
  (setf (alist-get ?\M-W avy-dispatch-alist) 'avy-action-copy-region)

  (defun avy-action-yank-region (pt)
    "Yank region and stays"
    (avy-action-copy-region pt)
    (yank)
    t)
  (setf (alist-get 33554457 avy-dispatch-alist) 'avy-action-yank-region) ; C-Y

  (defun avy-action-kill-region-move (pt)
    "Kills a region and moves"
    (if my-avy-point
	(progn
          (avy--quick-mark-region pt)
          (call-interactively 'kill-region)
          (message "Killed: %s" (current-kill 0))
          (point)
          t)
      (avy--check-for-region-errors)))
  (setf (alist-get 33554443 avy-dispatch-alist) 'avy-action-kill-region-move) ; C-K

  (defun avy-action-teleport-region (pt)
    "Teleports an arbitrary region using my-avy-point"
    (if my-avy-point
	(progn
	  (save-excursion
            (avy--quick-mark-region pt)
            (call-interactively 'kill-region))
	  (select-window
	   (cdr
            (ring-ref avy-ring 0)))
	  (yank)
	  t)
      (avy--check-for-region-errors)))
  (setf (alist-get 33554452 avy-dispatch-alist) 'avy-action-teleport-region) ; C-T

  ;;; Quick char actions
  ;; For some modes it is useful to have a shortcut for a common character, for example parenthesis in emacs-lisp

  ;; Basic funcion
  (defun avy-goto-quick-char (char &optional arg)
    "Simulates char press for filtering"
    (interactive (list char
                       current-prefix-arg))
    (avy-with avy-goto-char
      (avy-jump

       (regexp-quote (string char)))))

  ;; `emacs-lisp-mode'
  (defun avy-goto-parenthesis ()
    "Filter avy selecton with open parenthesis"
    (interactive)
    (avy-goto-quick-char 40)) ;; (
  ;; TODO Pick another keybind for paren jump
  ;; (define-key emacs-lisp-mode-map (kbd "S-SPC") 'avy-goto-parenthesis)

  ;;; TODO Auto actions and compounds

  ;;; LSP

  ;; (defun avy-action-lsp-help (pt)
  ;;   (avy-generic-command-action #'lsp-describe-thing-at-point))
  ;; (setf (alist-get 16777320 avy-dispatch-alist) 'avy-action-lsp-help) ; H-h

  (defun avy-action-lsp-goto-definition (pt)
    (avy-generic-command-action-no-stay #'xref-find-definitions))
  (setf (alist-get (kbd "M-.") avy-dispatch-alist) 'avy-action-lsp-goto-definition) ; M-.

  (defun avy-action-lsp-goto-references (pt)
    (avy-generic-command-action-no-stay #'xref-find-references))
  (setf (alist-get (kbd "M-?") avy-dispatch-alist) 'avy-action-lsp-goto-references) ; M-?

  (defun avy-action-lsp-rename (pt)
    (avy-generic-command-action
     (lambda () (call-interactively 'eglot-rename))))
  (setf (alist-get 16777330 avy-dispatch-alist) 'avy-action-lsp-rename) ; C-r

  ;;; Functions

  ;; Avy + Isearch
  ;; Isearch in other windows
  (defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
	(let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

  (defun isearch-backward-other-window (prefix)
    "Function to isearch-backward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
	(let ((next (if prefix 1 -1)))
          (other-window next)
          (isearch-backward)
          (other-window (- next))))))

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
      (message "%s" (apply #'concat (nreverse display-strings)))))

  (defun avy-goto-line-num (&optional arg)
    (interactive "p")
    (let ((avy-keys (number-sequence ?0 ?9)))
      (avy-goto-line arg)))

  (defun avy-goto-end-of-line-num (&optional arg)
    (interactive "p")
    (let ((avy-keys (number-sequence ?0 ?9)))
      (avy-goto-end-of-line arg))))

(use-package avy-zap
  :bind
  (("M-z" . avy-zap-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package ace-link
  :bind
  (("M-o" . ace-link))
  :init
  ;; Binds `o' to ace-link in the supported modes
  (ace-link-setup-default))
