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
  (("<C-i>" . embark-act)
   ([remap describe-bindings] . cory/embark-bindings)
   :map embark-file-map
   ("C-d" . cory/dragon-drop))
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
  (defun cory/dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
				 (concat "dragon-drag-and-drop " file)))
  (defun cory/embark-bindings (no-global)
    "Wrapper for `embark-bindings' to take substring completion."
    (interactive "P")
    (let ((completion-styles '(substring)))
      (embark-bindings no-global))))

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
	 ("C-k" . cory/kill-line)
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

  (defun cory/kill-line (&optional arg)
    "If ARG is given, kill backwards. Otherwise kill forwards."
    (interactive "P")
    (if (not arg)
	(crux-kill-and-join-forward)
      (crux-kill-and-join-backward))))

;; better comment-dwim
(use-package comment-dwim-2
  :bind ("M-;" . cory/comment-dwim)
  :config
  (defun cory/comment-dwim ()
    (interactive)
    (when (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (let ((region (buffer-substring-no-properties (mark) (point))))
        (kill-ring-save nil nil region)))
    (call-interactively #'comment-dwim-2)))

;; Paredit
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode
	  lisp-mode lisp-data-mode
	  clojure-mode cider-repl-mode
	  racket-mode racket-repl-mode
	  scheme-mode geiser-repl-mode
	  json-mode nix-mode
	  c-mode cpp-mode
	  java-mode javascript-mode)
	 . enable-paredit-mode)
  :bind
  (:map paredit-mode-map
   ("{"   . paredit-open-curly)
   ("}"   . paredit-close-curly)
   ("M-{" . paredit-wrap-curly)
   ("M-}" . paredit-close-curly-and-newline)
   ("M-;" . cory/comment-dwim)
   ("RET" . cory/newline-dwim)
   ("C-k" . cory/paredit-kill))
  :config
  ;; TODO fix prefix argument behavior
  (defun cory/paredit-kill (&optional argument)
    "Kill a line as if with `kill-line', but respecting delimiters.
In a string, act exactly as `kill-line' but do not kill past the
  closing string delimiter.
On a line with no S-expressions on it starting after the point or
  within a comment, act exactly as `kill-line'.
Otherwise, kill all S-expressions that start after the point.
Prefix arguments will enact the same behavior that `cory/kill-line'
enacts."
    (interactive "P")
    (cond ((paredit-in-string-p)
	   (paredit-kill-line-in-string))
	  ((paredit-in-comment-p)
	   (paredit-kill-line-in-comment))
	  ((save-excursion (paredit-skip-whitespace t (point-at-eol))
			   (or (eolp) (eq (char-after) ?\; )))
					;** Be careful about trailing backslashes.
	   (if (paredit-in-char-p)
	       (backward-char))
	   (cory/kill-line argument))
	  (t (paredit-kill-sexps-on-line))))

  (defun cory/paredit-semicolon (f &rest args)
    (if (region-active-p)
	(comment-region (region-beginning) (region-end))
      (apply f args)))
  (advice-add 'paredit-semicolon :around #'cory/paredit-semicolon))

;; (use-package cedit)

;; Smartparens
;; (use-package smartparens
;;   :defer 1
;;   :hook ((
;;           emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode
;; 	  racket-mode racket-repl-mode scheme-mode geiser-repl-mode json-mode
;;           ) . smartparens-strict-mode)
;;   :bind (:map smartparens-mode-map
;;          ;; This is the paredit mode map minus a few key bindings
;;          ;; that I use in other modes (e.g. M-?)
;;          ("C-M-f" . sp-forward-sexp) ;; navigation
;;          ("C-M-b" . sp-backward-sexp)
;;          ("C-M-u" . sp-backward-up-sexp)
;;          ("C-M-d" . sp-down-sexp)
;;          ("C-M-p" . sp-backward-down-sexp)
;;          ("C-M-n" . sp-up-sexp)
;;          ("M-S" . sp-splice-sexp) ;; depth-changing commands
;;          ("M-R" . sp-splice-sexp-killing-around)
;;          ("M-(" . sp-wrap-round)
;;          ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
;;          ("M-<right>" . sp-forward-slurp-sexp)
;;          ("C-}" . sp-forward-barf-sexp)
;;          ("M-<left>" . sp-forward-barf-sexp)
;;          ("C-(" . sp-backward-slurp-sexp)
;;          ("M-S-<left>" . sp-backward-slurp-sexp)
;;          ("C-{" . sp-backward-barf-sexp)
;;          ("M-S-<right>" . sp-backward-barf-sexp)
;;          ("M-S" . sp-split-sexp) ;; misc
;;          ("M-j" . sp-join-sexp)
;; 	 )
;;   :config
;;   (require 'smartparens-config)
;;   (setq sp-base-key-bindings 'paredit)
;;   (setq sp-autoskip-closing-pair 'always)

;;   ;; Don't insert annoying colon after Python def
;;   (setq sp-python-insert-colon-in-function-definitions nil)

;;   ;; Always highlight matching parens
;;   (show-smartparens-global-mode +1)

;;   ;; Blink matching parens
;;   (setq blink-matching-paren t)

;;   (defun whole-line-or-region-sp-kill-region (prefix)
;;     "Call `sp-kill-region' on region or PREFIX whole lines."
;;     (interactive "*p")
;;     (whole-line-or-region-wrap-beg-end 'sp-kill-region prefix))

;;   ;; Create keybindings to wrap symbol/region in pairs
;;   (defun prelude-wrap-with (s)
;;     "Create a wrapper function for smartparens using S."
;;     `(lambda (&optional arg)
;;        (interactive "P")
;;        (sp-wrap-with-pair ,s)))
;;   (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
;;   (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
;;   (define-key prog-mode-map (kbd "M-{") (prelude-wrap-with "{"))
;;   (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))
;;   (define-key prog-mode-map (kbd "M-'") (prelude-wrap-with "'"))
;;   (define-key prog-mode-map (kbd "M-`") (prelude-wrap-with "`"))

;;   ;; smart curly braces
;;   (sp-pair "{" nil :post-handlers
;;            '(((lambda (&rest _ignored)
;;                 (crux-smart-open-line-above)) "RET")))
;;   (sp-pair "[" nil :post-handlers
;;            '(((lambda (&rest _ignored)
;;                 (crux-smart-open-line-above)) "RET")))
;;   (sp-pair "(" nil :post-handlers
;;            '(((lambda (&rest _ignored)
;;                 (crux-smart-open-line-above)) "RET")))

;;   ;; Don't include semicolon ; when slurping
;;   (add-to-list 'sp-sexp-suffix '(java-mode regexp ""))
;;   (add-to-list 'sp-sexp-suffix '(c-mode regexp ""))
;;   (add-to-list 'sp-sexp-suffix '(c++-mode regexp ""))
;;   (add-to-list 'sp-sexp-suffix '(nix-mode regexp ""))

;;   ;; use smartparens-mode everywhere
;;   (smartparens-global-mode))

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
  (global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
  (global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
  (global-set-key (kbd "C-;") 'macrursors-mark-map)
  (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
  (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
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
  (("M-%" . cory/replace)
   ("C-M-%" . cory/replace))
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

;; Color picker
;; (use-package webkit-color-picker
;;   :ensure t
;;   :bind (("C-c p" . webkit-color-picker-show)))

;;; Easier macro handling
;; (use-package kmacro-x
;;   :ensure t
;;   :init (kmacro-x-atomic-undo-mode 1)
;;   :bind ("C-c k" . kmacro-x-mc-region))

;;; Delete selection mode
(delete-selection-mode 1)
