
;; Undo
(use-package undo-tree
  :defer 1
  :diminish undo-tree-mode
  :bind
  (:map undo-tree-map
   ("C-x u"   . undo-tree-visualize)
   ("C-/"     . undo-tree-undo)
   ("C-x C-u" . undo-tree-visualize-redo)
   ("C-?"     . undo-tree-redo))
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name
	      (concat user-emacs-directory "undo")))))
  :init
  (global-undo-tree-mode))

(use-package embark
  :ensure t
  :bind
  (("<C-i>" . embark-act) ; pick some comfortable binding
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d" . dragon-drop))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  (embark-quit-after-action nil)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (defun search-in-source-graph (text))
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
				 (concat "dragon-drag-and-drop " file))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package crux
  :bind (([(control return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
	 ("C-c u" . crux-view-url)
	 ;; ("C-c e" . crux-eval-and-replace)
	 ("C-x 4 t" . crux-transpose-windows)
	 ("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c D" . crux-duplicate-and-comment-current-line-or-region)
	 ;; ("C-c k" . crux-kill-other-buffers)
	 ("C-^" . crux-top-join-line)
	 ("C-k" . crux-kill-and-join-forward-2)
	 ([remap kill-whole-line]. crux-kill-whole-line)
         ("C-a"   . crux-move-beginning-of-line))
  :config
  ;; TODO need to detect when the point is at the beginning of indentation
  (defun crux-kill-and-join-backward ()
    (interactive)
    (if (and (save-mark-and-excursion
	    (let ((orig-point (point)))
	      (move-beginning-of-line 1)
	      (while (looking-at "[[:space:]\t]")
		(forward-char 1))
	      (= orig-point (point))))
	  (not (eolp)))
	(delete-indentation)
      (kill-line 0)
      (indent-according-to-mode)))

  (defun crux-kill-and-join-forward-2 (&optional arg)
    "If ARG is given, kill backwards. Otherwise kill forwards."
    (interactive "P")
    (if (not arg)
	(crux-kill-and-join-forward)
      (crux-kill-and-join-backward))))

;; better comment-dwim
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Smartparens
(use-package smartparens
  :defer 1
  :hook ((
          emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode
	  racket-mode racket-repl-mode scheme-mode geiser-repl-mode json-mode
          ) . smartparens-strict-mode)
  :bind (:map smartparens-mode-map
         ;; This is the paredit mode map minus a few key bindings
         ;; that I use in other modes (e.g. M-?)
         ("C-M-f" . sp-forward-sexp) ;; navigation
         ("C-M-b" . sp-backward-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-n" . sp-up-sexp)
         ("M-S" . sp-splice-sexp) ;; depth-changing commands
         ("M-R" . sp-splice-sexp-killing-around)
         ("M-(" . sp-wrap-round)
         ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
         ("M-<right>" . sp-forward-slurp-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("M-<left>" . sp-forward-barf-sexp)
         ("C-(" . sp-backward-slurp-sexp)
         ("M-S-<left>" . sp-backward-slurp-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("M-S-<right>" . sp-backward-barf-sexp)
         ("M-S" . sp-split-sexp) ;; misc
         ("M-j" . sp-join-sexp)
	 )
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)

  ;; Don't insert annoying colon after Python def
  (setq sp-python-insert-colon-in-function-definitions nil)

  ;; Always highlight matching parens
  (show-smartparens-global-mode +1)

  ;; Blink matching parens
  (setq blink-matching-paren t)

  (defun whole-line-or-region-sp-kill-region (prefix)
    "Call `sp-kill-region' on region or PREFIX whole lines."
    (interactive "*p")
    (whole-line-or-region-wrap-beg-end 'sp-kill-region prefix))

  ;; Create keybindings to wrap symbol/region in pairs
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
       (interactive "P")
       (sp-wrap-with-pair ,s)))
  (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
  (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
  (define-key prog-mode-map (kbd "M-{") (prelude-wrap-with "{"))
  (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))
  (define-key prog-mode-map (kbd "M-'") (prelude-wrap-with "'"))
  (define-key prog-mode-map (kbd "M-`") (prelude-wrap-with "`"))

  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "[" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "(" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))

  ;; Don't include semicolon ; when slurping
  (add-to-list 'sp-sexp-suffix '(java-mode regexp ""))
  (add-to-list 'sp-sexp-suffix '(c-mode regexp ""))
  (add-to-list 'sp-sexp-suffix '(c++-mode regexp ""))
  (add-to-list 'sp-sexp-suffix '(nix-mode regexp ""))

  ;; use smartparens-mode everywhere
  (smartparens-global-mode))

;; Smart-region: Smart region selection
;; Smart region guesses what you want to select by one command:
;; - If you call this command multiple times at the same position,
;;   it expands the selected region (with `er/expand-region’).
;; - Else, if you move from the mark and call this command,
;;   it selects the region rectangular (with `rectangle-mark-mode’).
;; - Else, if you move from the mark and call this command at the same column as
;;   mark, it adds a cursor to each line (with `mc/edit-lines’).

(use-package expand-region
  :defer t)

(use-package smart-region
  ;; C-SPC is smart-region
  :bind (([remap set-mark-command] . smart-region)))

;; Personal custom multi-edit package
;; Very similar to meow's beacon-mode
(use-package macrursors
  :custom
  (macrursors-preapply-command
   (lambda ()
     (corfu-mode -1)
     (goggles-mode -1)
     (beacon-mode -1)))
  (macrursors-postapply-command
   (lambda ()
     (corfu-mode 1)
     (goggles-mode 1)
     (beacon-mode 1)))
  :config
  (define-prefix-command 'macrursors-mark-map)
  (global-set-key (kbd "C-;") 'macrursors-mark-map)
  (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-instances-of)
  (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-instances-of)
  (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
  (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
  (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
  (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
  (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
  (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
  (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines))


;; Multiple cursors
;; (use-package multiple-cursors
;;   :bind (;; ("C-c m" . mc/mark-all-dwim)
;; 	 ("C-c m" . mc/mark-all-like-this)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)
;; 	 ("C-M-<" . mc/mark-all-in-region-regexp)
;; 	 ("C-M->" . mc/edit-lines)
;;          :map mc/keymap
;;          ("C-x v" . mc/vertical-align-with-space)
;;          ("C-x n" . mc-hide-unmatched-lines-mode))
;;   :custom
;;   (mc/always-run-for-all t)
;;   :config
;;   (global-unset-key (kbd "M-<down-mouse-1>"))
;;   (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;;   (with-eval-after-load 'multiple-cursors-core
;;     ;; Immediately load mc list
;;     (mc/load-lists)
;;     ;; Define keys
;;     (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
;;     (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
;;     (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this)
;;     (define-key mc/keymap (kbd "<return>") nil))

;;   (setq mc/max-cursors 201)
;;   (defun mc/create-fake-cursor-at-point (&optional id)
;;     "Overriding mc/create-fake-cursor-at-point to save hidden cursors"
;;     (unless mc--max-cursors-original
;;       (setq mc--max-cursors-original mc/max-cursors))
;;     (if mc/max-cursors
;; 	(if (< (mc/num-cursors) mc/max-cursors)
;;             (let ((overlay (mc/make-cursor-overlay-at-point)))
;; 	      (overlay-put overlay 'mc-id (or id (mc/create-cursor-id)))
;; 	      (overlay-put overlay 'type 'fake-cursor)
;; 	      (overlay-put overlay 'priority 100)
;; 	      (mc/store-current-state-in-overlay overlay)
;; 	      (when (use-region-p)
;; 		(overlay-put overlay 'region-overlay
;;                              (mc/make-region-overlay-between-point-and-mark)))
;; 	      overlay)
;;           (if (use-region-p)
;; 	      (progn (setq-local cory/mc-hidden-cursors (cons (cons (point-marker) (copy-marker (mark-marker))) cory/mc-hidden-cursors)))
;;             (setq-local cory/mc-hidden-cursors (cons (cons (point-marker) (point-marker)) cory/mc-hidden-cursors))))
;;       (let ((overlay (mc/make-cursor-overlay-at-point)))
;; 	(overlay-put overlay 'mc-id (or id (mc/create-cursor-id)))
;; 	(overlay-put overlay 'type 'fake-cursor)
;; 	(overlay-put overlay 'priority 100)
;; 	(mc/store-current-state-in-overlay overlay)
;; 	(when (use-region-p)
;;           (overlay-put overlay 'region-overlay
;; 		       (mc/make-region-overlay-between-point-and-mark)))
;; 	overlay)))


;;   (defun mc--maybe-set-killed-rectangle ()
;;     "There are some bugs regarding saving to kill ring for
;;  rectangle editing, but I don't use it, so just ignore it")

;;   (defun cory/apply-macro-for-the-next-hidden-cursor ()
;;     "Apply kmacro to the next hidden cursor"
;;     (interactive)
;;     (let* ((next-cursor (car cory/mc-hidden-cursors))
;;            (p (car next-cursor))
;;            (m (cdr next-cursor)))
;;       (if (not (= p m))
;;           (progn
;;             (push-mark m t nil)
;;             (goto-char p)
;;             (activate-mark))
;; 	(goto-char p))
;;       (call-last-kbd-macro)
;;       (setq-local cory/mc-hidden-cursors (cdr cory/mc-hidden-cursors))))

;;   (defun mc/remove-cursor-at-point-if-exist ()
;;     "Remove cursors at point, either fake or real."
;;     (interactive)
;;     (let ((removed nil))
;;       (cl-loop for cursor in (mc/all-fake-cursors)
;;                for start = (overlay-start cursor)
;;                do (when (= start (point))
;; 		    (mc/remove-fake-cursor cursor)
;; 		    (setq-local removed t)))
;;       removed))

;;   (defun cory/mc-remove-current-cursor ()
;;     "blabla"
;;     (interactive)
;;     (let ((old-point (point)))
;;       (when (not (call-interactively 'mc/remove-cursor-at-point-if-exist))
;; 	(if (mc/last-fake-cursor-before (point))
;;             (call-interactively 'mc/cycle-backward)
;;           (call-interactively 'mc/cycle-forward))
;; 	(setq-local new-point (point))
;; 	(goto-char old-point)
;; 	(call-interactively 'mc/remove-cursor-at-point-if-exist)
;; 	(goto-char new-point))))

;;   (defun cory/apply-macro-for-all-hidden-cursors ()
;;     "Apply kmacro to all hidden cursors"
;;     (interactive)
;;     (while cory/mc-hidden-cursors
;;       (let* ((next-cursor (car cory/mc-hidden-cursors))
;;              (p (car next-cursor))
;;              (m (cdr next-cursor)))
;; 	(if (not (= p m))
;;             (progn
;; 	      (push-mark m t t)
;; 	      (goto-char p))
;;           (goto-char p))
;; 	(call-last-kbd-macro)
;; 	(setq-local cory/mc-hidden-cursors (cdr cory/mc-hidden-cursors)))))

;;   (defun mc/mark-all-like-this ()
;;     "example: override mc/mark-all-like-this to make it work with hidden cursors"
;;     (interactive)
;;     (unless (region-active-p)
;;       (error "Mark a region to match first."))
;;     (mc/remove-fake-cursors)
;;     (setq-local cory/mc-hidden-cursors nil)
;;     (let ((master (point))
;;           (case-fold-search nil)
;;           (point-first (< (point) (mark)))
;;           (re (regexp-opt (mc/region-strings) mc/enclose-search-term)))
;;       (mc/save-excursion
;;        (goto-char 0)
;;        (while (search-forward-regexp re nil t)
;; 	 (push-mark (match-beginning 0))
;; 	 (when point-first (exchange-point-and-mark))
;; 	 (unless (and (= master (point)))
;;            (mc/create-fake-cursor-at-point))
;; 	 (when point-first (exchange-point-and-mark)))))
;;     (if (> (mc/num-cursors) 1)
;; 	(multiple-cursors-mode 1)
;;       (mc/disable-multiple-cursors-mode))
;;     (when cory/mc-hidden-cursors
;;       (cory/mc-remove-current-cursor)
;;       (setq-local cory/mc-hidden-cursors (reverse cory/mc-hidden-cursors))
;;       (message "Visible cursors threshold is hit, hidden cursors created, please record macro to apply changes to them"))))

;; Visual regex replacement
(use-package visual-regexp
  :bind
  (("C-c c" . cory/replace)
   ("C-c C" . vr/query-replace)
   ;; for multiple-cursors
   ("C-c M" . vr/mc-mark))
  :config
  (defun cory/replace ()
    (interactive)
    ;; If region is active, only replace the region
    (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
        (call-interactively 'vr/replace)
      (save-excursion
	(goto-char 0)
	(call-interactively 'vr/replace)))))

;; Move text
(use-package move-text
  :bind (([(control shift up)]   . move-text-up)
         ([(control shift down)] . move-text-down)
         ([(meta shift up)]      . move-text-up)
         ([(meta shift down)]    . move-text-down)
	 ("C-S-n" . move-text-down)
	 ("C-S-p" . move-text-up)))

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

;; ;; Copy text as Discord/GitHub/etc formatted code
;; (use-package copy-as-format
;;   :bind
;;   (("C-c c c" . copy-as-format)
;;    ("C-c c g" . copy-as-format-github)
;;    ("C-c c t" . copy-as-format-markdown-table)
;;    ("C-c c m" . copy-as-format-markdown)
;;    ("C-c c o" . copy-as-format-org-mode)
;;    ("C-c c d" . copy-as-format-slack)
;;    ("C-c c v" . org-copy-visible))
;;   :config
;;   (setq copy-as-format-default "slack")
;;   (defun copy-as-format--markdown-table (text _multiline)
;;     (s-replace "--+--" "--|--" text))
;;   (add-to-list 'copy-as-format-format-alist '("markdown-table" copy-as-format--markdown-table)))

;; Code folding
;; (dolist (mode '(c-mode-common-hook
;; 		emacs-lisp-mode-hook
;; 		lisp-mode-hook
;; 		clojure-mode-hook
;; 		clojurescript-mode-hook
;; 		clojurec-mode-hook
;; 		java-mode-hook
;; 		perl-mode-hook
;; 		sh-mode-hook
;; 		nix-mode-hook))
;;   (add-hook mode 'hs-minor-mode))
;; (global-set-key (kbd "C-+") 'hs-toggle-hiding)

(use-package origami
  :bind
  (("C-=" . origami-toggle-node)
   ("C-+" . origami-show-only-node)
   ;; ("C-+" . origami-recursively-toggle-node)
   )
  :config
  (global-origami-mode))
