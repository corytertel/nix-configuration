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
  (define-key c-mode-base-map (kbd "C-c C-n") nil)
  (define-key c-mode-base-map (kbd "C-c <C-h>") #'c-forward-conditional)
  (define-key c-mode-base-map (kbd "C-c C-p") nil)
  (define-key c-mode-base-map (kbd "C-c C-t") #'c-backward-conditional))

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
         ("C-a" . crux-move-beginning-of-line)
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

;; Structural editing
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
   ;; ("C-<left>" . sp-forward-barf-sexp)
   ;; ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<down>" . sp-splice-sexp-killing-forward)
   ("M-<up>" . sp-splice-sexp-killing-backward)
   ("C-M-y" . sp-down-sexp)
   ("C-M-h" . sp-up-sexp)
   ("C-M-t" . sp-backward-down-sexp)
   ("C-M-d" . sp-backward-sexp)
   ("C-M-n" . sp-forward-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("M-J" . sp-join-sexp)
   ("M-S" . sp-split-sexp)
   ("M-y" . sp-kill-word)
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
		      :inherit 'unspecified)

  ;; Web mode
  (sp-local-tag '(web-mode) "<" "<_>" "</_>"
		:transform 'sp-match-sgml-tags
		:post-handlers
		'(((lambda (&rest _ignored)
                     (crux-smart-open-line-above)) "RET"))))

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
  (defvar cory/macrursors-stored-modes '())
  (add-hook 'macrursors-pre-finish-hook
	    (lambda ()
	      (dolist (mode '(aggressive-indent-mode
			      corfu-mode))
		(when (eval mode)
		  (add-to-list 'cory/macrursors-stored-modes mode)
		  (funcall mode -1)))))
  (add-hook 'macrursors-post-finish-hook
	    (lambda ()
	      (dolist (mode cory/macrursors-stored-modes)
		(funcall mode 1))
	      (setq cory/macrursors-stored-modes '())))
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
  (define-key isearch-mode-map (kbd "C-<") #'macrursors-mark-previous-from-isearch)
  (require 'macrursors-select))

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
;; (use-package move-text
;;   :bind (("M-h" . move-text-down)
;; 	 ("M-t" . move-text-up)))

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

;; Delete selection mode
(delete-selection-mode 1)

;; God mode
;; (use-package god-mode
;;   :custom
;;   (god-mode-alist
;;    '((nil . "C-")
;;      ("." . "M-")
;;      (">" . "C-M-")))
;;   (god-exempt-major-modes nil)
;;   (god-exempt-predicates nil)
;;   (god-mode-enable-function-key-translation nil)
;;   :config
;;   (defun cory/god-mode-update-cursor-type ()
;;     (setq cursor-type (if god-local-mode 'box 'hollow)))

;;   (add-hook 'post-command-hook #'cory/god-mode-update-cursor-type)

;;   (global-set-key (kbd ",") #'god-mode-all)
;;   (global-set-key (kbd "C-x C-0") #'delete-window)
;;   (global-set-key (kbd "C-x C-1") #'delete-other-windows)
;;   (global-set-key (kbd "C-x C-2") #'split-and-follow-below)
;;   (global-set-key (kbd "C-x C-3") #'split-and-follow-right)

;;   (setq god-local-mode-map
;; 	(let ((map (make-sparse-keymap)))
;; 	  (suppress-keymap map t)
;; 	  (define-key map [remap self-insert-command] 'god-mode-self-insert)
;; 	  (let ((i ?\s))
;; 	    (while (< i 256)
;;               (define-key map (vector i) 'god-mode-self-insert)
;;               (setq i (1+ i))))
;; 	  (when god-mode-enable-function-key-translation
;; 	    (dotimes (i 35)
;;               (define-key map (vector (god-mode-make-f-key (1+ i))) 'god-mode-self-insert)
;;               (define-key map (vector (god-mode-make-f-key (1+ i) t)) 'god-mode-self-insert)))
;; 	  (define-key map (kbd "DEL") nil)
;; 	  ;; (define-key map (kbd "C-h k") #'god-mode-describe-key)
;; 	  (define-key map (kbd ",")
;; 	    (lambda () (interactive) (insert ?,) (god-mode-all)))
;; 	  (define-key map (kbd "SPC")
;; 	    (lambda () (interactive) (insert ?,) (insert ? ) (god-mode-all)))
;; 	  (define-key map (kbd "g") #'god-mode-all)
;; 	  (define-key map (kbd "'") #'repeat)
;; 	  map)))

;; Devil mode
(use-package devil
  :bind
  (("C-h k" . devil-describe-key))
  :config
  (defun devil--invalid-key-p (translated-key)
    "Return t iff TRANSLATED-KEY is an invalid Emacs key sequence."
    (catch 'break
      (dolist (chunk (split-string translated-key " "))
	(when (or (string= chunk "")
		 (not (string-match-p "^\\(?:[ACHMSs]-\\)*\\([^-]*\\|<.*>\\)$" chunk))
		 (string-match-p "\\([ACHMSs]-\\)[^ ]*\\1" chunk))
          (throw 'break t)))))
  (global-devil-mode 1)
  (add-hook 'server-after-make-frame-hook
	    (lambda ()
	      (global-devil-mode 1)))
  :custom
  (devil-key ".")
  (devil-repeatable-keys nil)
  (devil-translations
   (list (cons "%k , ," "C-M-")
         (cons "%k , %k" "M-.")
         (cons "%k , '" "M-")
         (cons "%k ," "M-")
         (cons "%k %k" "%k")
         (cons "%k '" "C-")
	 (cons "%k x" "C-q")
	 (cons "%k q" "C-x")
	 (cons "%k c" "C-w")
	 (cons "%k w" "C-c")
	 (cons "%k h" "<C-h>")
	 (cons "%k i" "C-h")
         (cons "%k"  "C-"))))
