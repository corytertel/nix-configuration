;; Indentation
(require 'cc-engine)
(defun cory/c-lineup-methods (langelem)
  (save-excursion
    (back-to-indentation)
    (when (eq ?. (char-after))
      (let ((limit (c-langelem-pos langelem)) (depth 1))
        (catch 'done
          (while (and (c-syntactic-skip-backward "^?." limit t)
		    (not (bobp)))
            (backward-char)
            (cond ((eq (char-after) ?.)
                   ;; If we've found a second period, decrease depth.  If we've
                   ;; reached zero, we've found the one we were looking for.
                   (when (zerop (setq depth (1- depth)))
                     (throw 'done (vector (current-column)))))
                  ((or (eq ?: (char-before)) (eq ?? (char-before)))
                   ;; Step over `..' operator.  We don't have to
                   ;; handle `..' here but doing so saves an iteration.
                   (if (eq (point) limit)
		       (throw 'done nil)
                     (goto-char (1- (point)))))
                  ((setq depth (1+ depth))))))))))

(c-add-style "cory/java"
	     '("java"
	       (c-basic-offset . 2)
	       (c-offsets-alist
		(arglist-intro . +)
		(arglist-close . 0)
		(case-label . +)
		(statement-cont . (first cory/c-lineup-methods c-lineup-ternary-bodies ++)))))

(setq c-default-style '((java-mode . "cory/java")
                        (awk-mode  . "awk")
                        (c++-mode  . "stroustrup")
                        (c-mode    . "stroustrup")
                        (other     . "gnu")))

;; (electric-pair-mode 1)

;;; C Mode Minimak binds
(with-eval-after-load 'cc-mode
  ;; (define-key c-mode-base-map (kbd "#") nil)
  ;; (define-key c-mode-base-map (kbd "(") nil)
  ;; (define-key c-mode-base-map (kbd ")") nil)
  ;; (define-key c-mode-base-map (kbd "*") nil)
  ;; (define-key c-mode-base-map (kbd ",") nil)
  ;; (define-key c-mode-base-map (kbd "/") nil)
  ;; (define-key c-mode-base-map (kbd ":") nil)
  ;; (define-key c-mode-base-map (kbd ";") nil)
  ;; (define-key c-mode-base-map (kbd "DEL") nil)
  ;; (define-key c-mode-base-map (kbd "{") nil)
  ;; (define-key c-mode-base-map (kbd "}") nil)
  (define-key c-mode-base-map (kbd "C-M-a") nil)
  (define-key c-mode-base-map (kbd "C-M-b") #'c-beginning-of-defun)
  (define-key c-mode-base-map (kbd "C-M-e") nil)
  (define-key c-mode-base-map (kbd "C-M-y") #'c-end-of-defun)
  (define-key c-mode-base-map (kbd "C-c C-n") nil)
  (define-key c-mode-base-map (kbd "C-c C-e") #'c-forward-conditional)
  (define-key c-mode-base-map (kbd "C-c C-p") nil)
  (define-key c-mode-base-map (kbd "C-c C-i") #'c-backward-conditional))

;; Undo
(use-package undo-tree
  :defer 1
  :diminish undo-tree-mode
  :bind
  (:map undo-tree-map
   ("C-x u"   . undo-tree-visualize)
   ("C-z"     . undo-tree-undo)
   ("C-_"     . undo-tree-undo)
   ("C-x C-u" . undo-tree-visualize-redo)
   ("C-S-z"     . undo-tree-redo)
   ("M-_"     . undo-tree-redo)
   ("<undo>" . undo-tree-undo)
   ("<redo>" . undo-tree-redo)
   ("C-/" . nil)
   ("C-?" . nil))
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
  :disabled t
  :ensure t
  :bind
  (("M-w" . embark-act)
   ([remap describe-bindings] . cory/embark-bindings))
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
  (defun cory/embark-bindings (no-global)
    "Wrapper for `embark-bindings' to take substring completion."
    (interactive "P")
    (let ((completion-styles '(substring)))
      (embark-bindings no-global))))

(use-package embark-consult
  :disabled t
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package crux
  :bind (([(control return)] . crux-smart-open-line)
         ([(meta return)] . crux-smart-open-line-above)
	 ("C-c u" . crux-view-url)
	 ;; ("C-c e" . crux-eval-and-replace)
	 ("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c D" . crux-duplicate-and-comment-current-line-or-region)
	 ;; ("C-c k" . crux-kill-other-buffers)
	 ("C-^" . crux-top-join-line)
	 ([remap kill-line] . cory/kill-line)
	 ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-b" . crux-move-beginning-of-line)
         ("<home>" . crux-move-beginning-of-line)
	 :map ctl-x-4-map
	 ("t" . crux-transpose-windows))
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
  :bind ([remap comment-dwim] . comment-dwim-2))

;; Paredit
(use-package paredit
  :disabled t
  :ensure t
  :hook ((emacs-lisp-mode
	  lisp-mode lisp-data-mode
	  clojure-mode cider-repl-mode
	  racket-mode racket-repl-mode
	  scheme-mode geiser-repl-mode)
	 . enable-paredit-mode)
  :bind
  (:map paredit-mode-map
   ("{"   . paredit-open-curly)
   ("}"   . paredit-close-curly)
   ("M-{" . paredit-wrap-curly)
   ("M-}" . paredit-close-curly-and-newline)
   ("M-;" . nil)
   ("RET" . nil)
   ("C-k" . cory/paredit-kill)
   ;; Minimak
   ("C-j" . nil)
   ("C-M-b" . nil)
   ("C-M-f" . nil)
   ("C-M-n" . nil)
   ("C-M-p" . nil)
   ("C-M-j" . paredit-backward)
   ("C-M-l" . paredit-forward)
   ("C-M-i" . paredit-backward-down)
   ("C-M-e" . paredit-forward-up)
   ("C-c C-M-l" . nil)
   ("C-c C-M-p" . paredit-recenter-on-sexp))
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

(use-package smartparens
  :after clojure-mode
  :hook
  ((prog-mode web-mode sgml-mode) . smartparens-mode)
  ((emacs-lisp-mode lisp-mode scheme-mode clojure-mode) . smartparens-strict-mode)
  :custom
  (sp-navigate-consider-stringlike-sexp t)
  (sp-autoskip-closing-pair 'always)
  ;; Don't insert annoying colon after Python def
  (sp-python-insert-colon-in-function-definitions nil)
  :bind
  (:map smartparens-mode-map
   ("C-(" . sp-backward-slurp-sexp)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-backward-barf-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<down>" . sp-splice-sexp-killing-forward)
   ("M-<up>" . sp-splice-sexp-killing-backward)
   ("C-M-d" . sp-down-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-i" . sp-backward-down-sexp)
   ("C-M-j" . sp-backward-sexp)
   ("C-M-l" . sp-forward-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("M-J" . sp-join-sexp)
   ("M-S" . sp-split-sexp)
   ("M-d" . sp-kill-word)
   ("M-q" . sp-indent-defun)
   ("M-r" . sp-raise-sexp)
   ("M-s" . sp-splice-sexp)
   :map emacs-lisp-mode-map
   (";" . sp-comment)
   :map scheme-mode-map
   (";" . sp-comment)
   :map lisp-mode-map
   (";" . sp-comment)
   :map clojure-mode-map
   (";" . sp-comment))
  :init
  (require 'scheme)
  (require 'smartparens-config)
  :config
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

  ;; Rid of annoying highlight
  (set-face-attribute 'sp-pair-overlay-face nil
		      :inherit 'unspecified))

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

;; Multi-edit package
(use-package macrursors
  :config
  (require 'macrursors-select)
  (add-hook 'macrursors-pre-finish-hook
	    (lambda ()
	      (aggressive-indent-mode -1)
	      (corfu-mode -1)))
  (add-hook 'macrursors-post-finish-hook
	    (lambda ()
	      (aggressive-indent-mode 1)
	      (corfu-mode 1)))
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
  (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines)
  (define-key macrursors-mark-map (kbd "C-SPC") #'macrursors-select)
  (define-key macrursors-mark-map (kbd "SPC") #'macrursors-select)
  (define-key isearch-mode-map (kbd "C-;") #'macrursors-mark-from-isearch)
  (define-key isearch-mode-map (kbd "C->") #'macrursors-mark-next-from-isearch)
  (define-key isearch-mode-map (kbd "C-<") #'macrursors-mark-previous-from-isearch))

;; Visual regex replacement
(use-package visual-regexp
  :bind
  (([remap query-replace] . cory/replace)
   ([remap query-replace-regexp] . cory/replace))
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
  :disabled t
  :bind
  (("C-=" . origami-toggle-node)
   ("C-+" . origami-show-only-node)
   ;; ("C-+" . origami-recursively-toggle-node)
   )
  :config
  (global-origami-mode))

;;; Delete selection mode
(delete-selection-mode 1)
