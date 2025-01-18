;;; Keybinding Fix

(define-key function-key-map
  [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map
  [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map
  [(meta control shift iso-lefttab)] [(meta control shift tab)])

;;; VIPER

;; (setq viper-mode t
;;       viper-inhibit-startup-message t
;;       viper-expert-level 5
;;       viper-want-ctl-h-help t
;;       ;; viper-ex-style-motion nil ; allow l,h to cross line boundaries
;;       )
;; (require 'viper)

;; ;; (setq-default inhibit-startup-screen t) does not work
;; (defun display-startup-screen (&optional concise)
;;   (ignore))

;; ;; Relative line numbers
;; (display-line-numbers-mode)
;; (setq display-line-numbers 'relative)

;;; Evil

;; I do not use evil-collection because I do not want a package which (heavily) modifies
;; the global state of my emacs upon loading. Evil-collection has vast side-effects and
;; does not give the user much control over those side effects. I want to pick which side
;; effects I want and heavily customize them. Thus, I do all of my own manual configuration,
;; and I'm much happier with the result. It is closer to what I envision integration to be that
;; what evil-collection could ever accomplish.

;; For each rebind, we need to be careful to preserve the expected behavior.
;; i.e. adding to macro, switching evil states, etc

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-tree)
  (evil-move-beyond-eol t)
  (evil-cross-lines t)
  :init
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)

  ;; Relative line numbers
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers 'relative)

  ;; Cursor
  ;; (setq evil-default-cursor 'hollow)
  (setq evil-default-cursor '(box "black"))
  (setq evil-normal-state-cursor '(box "black"))
  (setq evil-visual-state-cursor '(box "black"))
  (setq evil-motion-state-cursor '(box "black"))
  (setq evil-insert-state-cursor '((bar . 2) "green"))

  ;; Enable C-[
  (define-key evil-insert-state-map (kbd "C-[") #'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-[") #'evil-normal-state)

  ;; Make TAB have the same behavior as in emacs
  (define-key evil-normal-state-map (kbd "TAB") #'indent-for-tab-command)
  (define-key evil-visual-state-map (kbd "TAB") #'indent-for-tab-command)

  ;; Make A also indent
  (defun evil-append-line (count &optional vcount)
    "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
    (interactive "p")
    (indent-for-tab-command)
    (if (and visual-line-mode
             evil-respect-visual-line-mode)
        (evil-end-of-visual-line)
      (evil-move-end-of-line))
    (setq evil-insert-count count
          evil-insert-lines nil
          evil-insert-vcount
          (and vcount
               (> vcount 1)
               (list (line-number-at-pos)
                     #'end-of-line
                     vcount)))
    (evil-insert-state 1))

  ;; Make :q not quit emacs
  (global-set-key [remap evil-quit] #'kill-buffer-and-window)
  (global-set-key [remap evil-save-and-close] #'cory/kill-buffer-and-window-and-save)

  ;; Make : trigger M-x instead
  ;; (define-key evil-motion-state-map (kbd ":") #'evil-execute-extended-command)
  (define-key evil-motion-state-map (kbd ":")
    (lambda ()
      (interactive)
      ;; You will want vertico-preselect to be 'prompt for this command, or else you it
      ;; will be unintutive to submit evil-commands
      (let ((vertico-preselect 'prompt))
        (call-interactively #'evil-execute-extended-command))))
  (define-key evil-motion-state-map (kbd "C-w :") #'execute-extended-command)

  ;; Make M-x trigger in insert mode
  ;; (define-key evil-insert-state-map (kbd "M-x") #'execute-extended-command)

  ;; Make vim sexp based instead of line based in lisp modes to make it more intuitive to work with

  ;; TODO add more emacs integration into the evil-ex commands
  ;; TODO in the future try to automate this
  ;; Manually define aliases so M-x has the completion of evil-ex

  ;; "read", "delete", "yank", "put", "substitute", "mark", "<", ">", "=", "sort",
  ;; "undo", and "print" cannot be aliased due to naming conficts

  ;; Make :q not quit emacs
  ;; (defalias 'quit #'kill-buffer-and-window)
  ;; (defalias 'q #'kill-buffer-and-window)
  ;; (defalias 'wq #'cory/kill-buffer-and-window-and-save)
  ;; (defalias 'quitall #'kill-all-buffers-and-windows)
  ;; (defalias 'quita #'kill-all-buffers-and-windows)
  ;; (defalias 'qall #'kill-all-buffers-and-windows)
  ;; (defalias 'qa #'kill-all-buffers-and-windows)
  ;; ;; (defalias 'cquit #'evil-quit-all-with-error-code)
  ;; ;; (defalias 'cq #'evil-quit-all-with-error-code)
  ;; ;; (defalias 'wqall #'evil-save-and-quit)
  ;; ;; (defalias 'wqa #'evil-save-and-quit)
  ;; ;; (defalias 'xall #'evil-save-and-quit)
  ;; ;; (defalias 'xa #'evil-save-and-quit)

  ;; (defalias 'edit #'evil-edit)
  ;; (defalias 'e #'evil-edit)
  ;; (defalias 'write #'evil-write)
  ;; (defalias 'w #'evil-write)
  ;; (defalias 'update #'evil-update)
  ;; (defalias 'up #'evil-update)
  ;; (defalias 'wall #'evil-write-all)
  ;; (defalias 'wa #'evil-write-all)
  ;; (defalias 'saveas #'evil-save)
  ;; (defalias 'sav #'evil-save)
  ;; (defalias 'r #'evil-read)
  ;; (defalias 'buffer #'evil-buffer)
  ;; (defalias 'b #'evil-buffer)
  ;; (defalias 'bnext #'evil-next-buffer)
  ;; (defalias 'bn #'evil-next-buffer)
  ;; (defalias 'bprevious #'evil-prev-buffer)
  ;; (defalias 'bp #'evil-prev-buffer)
  ;; (defalias 'bNext #'evil-prev-buffer)
  ;; (defalias 'bN #'evil-prev-buffer)
  ;; (defalias 'sbuffer #'evil-split-buffer)
  ;; (defalias 'sb #'evil-split-buffer)
  ;; (defalias 'sbnext #'evil-split-next-buffer)
  ;; (defalias 'sbn #'evil-split-next-buffer)
  ;; (defalias 'sbprevious #'evil-split-prev-buffer)
  ;; (defalias 'sbp #'evil-split-prev-buffer)
  ;; (defalias 'sbNext #'evil-split-prev-buffer)
  ;; (defalias 'sbN #'evil-split-prev-buffer)
  ;; (defalias 'buffers #'buffer-menu)
  ;; (defalias 'files #'evil-show-files)
  ;; (defalias 'ls #'buffer-menu)
  ;; (defalias 'change #'evil-change)
  ;; (defalias 'c #'evil-change)
  ;; (defalias 'copy #'evil-copy)
  ;; (defalias 'co #'evil-copy)
  ;; (defalias 't #'evil-copy)
  ;; (defalias 'move #'evil-move)
  ;; (defalias 'm #'evil-move)
  ;; (defalias 'd #'evil-ex-delete)
  ;; (defalias 'y #'evil-ex-yank)
  ;; (defalias 'pu #'evil-ex-put)
  ;; (defalias 'goto #'evil-goto-char)
  ;; (defalias 'go #'evil-goto-char)
  ;; (defalias 'join #'evil-ex-join)
  ;; (defalias 'j #'evil-ex-join)
  ;; (defalias 'left #'evil-align-left)
  ;; (defalias 'le #'evil-align-left)
  ;; (defalias 'right #'evil-align-right)
  ;; (defalias 'ri #'evil-align-right)
  ;; (defalias 'center #'evil-align-center)
  ;; (defalias 'ce #'evil-align-center)
  ;; (defalias 'split #'evil-window-split)
  ;; (defalias 'sp #'evil-window-split)
  ;; (defalias 'vsplit #'evil-window-vsplit)
  ;; (defalias 'vs #'evil-window-vsplit)
  ;; (defalias 'new #'evil-window-new)
  ;; (defalias 'enew #'evil-buffer-new)
  ;; (defalias 'ene #'evil-buffer-new)
  ;; (defalias 'vnew #'evil-window-vnew)
  ;; (defalias 'vne #'evil-window-vnew)
  ;; (defalias 'close #'evil-window-delete)
  ;; (defalias 'clo #'evil-window-delete)
  ;; (defalias 'only #'delete-other-windows)
  ;; (defalias 'on #'delete-other-windows)
  ;; (defalias 'xit #'evil-save-modified-and-close)
  ;; (defalias 'x #'evil-save-modified-and-close)
  ;; (defalias 'exit #'evil-save-modified-and-close)
  ;; (defalias 'exi #'evil-save-modified-and-close)
  ;; (defalias 'bdelete #'evil-delete-buffer)
  ;; (defalias 'bd #'evil-delete-buffer)
  ;; (defalias 'bwipeout #'evil-delete-buffer)
  ;; (defalias 'bw #'evil-delete-buffer)
  ;; (defalias 'global #'evil-ex-global)
  ;; (defalias 'g #'evil-ex-global)
  ;; (defalias 'vglobal #'evil-ex-global-inverted)
  ;; (defalias 'v #'evil-ex-global-inverted)
  ;; (defalias 'normal #'evil-ex-normal)
  ;; (defalias 'norm #'evil-ex-normal)
  ;; (defalias 's #'evil-ex-substitute)
  ;; (defalias '& #'evil-ex-repeat-substitute)
  ;; (defalias '&& #'evil-ex-repeat-substitute-with-flags)
  ;; (defalias '~ #'evil-ex-repeat-substitute-with-search)
  ;; (defalias '~& #'evil-ex-repeat-substitute-with-search-and-flags)
  ;; (defalias 'match #'evil-ex-match)
  ;; (defalias 'mat #'evil-ex-match)
  ;; (defalias 'registers #'evil-show-registers)
  ;; (defalias 'display #'evil-show-registers)
  ;; (defalias 'di #'evil-show-registers)
  ;; (defalias 'ma #'evil-set-col-0-mark)
  ;; (defalias 'marks #'evil-show-marks)
  ;; (defalias 'delmarks #'evil-delete-marks)
  ;; (defalias 'delm #'evil-delete-marks)
  ;; (defalias 'jumps #'evil-show-jumps)
  ;; (defalias 'ju #'evil-show-jumps)
  ;; (defalias 'nohlsearch #'evil-ex-nohighlight)
  ;; (defalias 'noh #'evil-ex-nohighlight)
  ;; (defalias 'file #'evil-show-file-info)
  ;; (defalias 'f #'evil-show-file-info)
  ;; (defalias '! #'evil-shell-command)
  ;; (defalias '@: #'evil-ex-repeat)
  ;; (defalias 'make #'evil-make)
  ;; (defalias 'mak #'evil-make)
  ;; (defalias 'cc #'evil-goto-error)
  ;; (defalias 'cfirst #'first-error)
  ;; (defalias 'cfir #'first-error)
  ;; (defalias 'crewind #'first-error)
  ;; (defalias 'cr #'first-error)
  ;; (defalias 'cnext #'next-error)
  ;; (defalias 'cn #'next-error)
  ;; (defalias 'cprevious #'previous-error)
  ;; (defalias 'cp #'previous-error)
  ;; (defalias 'set-initial-state #'evil-ex-set-initial-state)
  ;; (defalias 'show-digraphs #'evil-ex-show-digraphs)
  ;; (defalias 'sor #'evil-ex-sort)
  ;; (defalias 'resize #'evil-ex-resize)
  ;; (defalias 'res #'evil-ex-resize)
  ;; (defalias 'u #'evil-undo)
  ;; (defalias 'redo #'evil-redo)
  ;; (defalias 'red #'evil-redo)
  ;; (defalias 'p #'evil-ex-print)
  ;; (defalias 'Print #'evil-ex-print)
  ;; (defalias 'P #'evil-ex-print)
  ;; (defalias 'number #'evil-ex-numbered-print)
  ;; (defalias 'nu #'evil-ex-numbered-print)
  ;; (defalias '\# #'evil-ex-numbered-print)
  ;; (defalias 'z #'evil-ex-z)
  ;; (defalias 'retab #'evil-retab)
  ;; (defalias 'ret #'evil-retab)
  ;; (defalias 'tabnew #'tab-bar-new-tab)
  ;; (defalias 'tabclose #'tab-bar-close-tab)
  ;; (defalias 'tabc #'tab-bar-close-tab)
  ;; (defalias 'tabonly #'tab-bar-close-other-tabs)
  ;; (defalias 'tabo #'tab-bar-close-other-tabs)
  ;; (defalias 'tabnext #'evil-tab-next)
  ;; (defalias 'tabn #'evil-tab-next)
  ;; (defalias 'tabprevious #'tab-bar-switch-to-prev-tab)
  ;; (defalias 'tabp #'tab-bar-switch-to-prev-tab)
  ;; (defalias 'undolist #'undo-tree-visualize)
  ;; (defalias 'undol #'undo-tree-visualize)
  ;; (defalias 'ul #'undo-tree-visualize)
  )

;;; Use devil as "leader key"

;; Using devil instead of any other "roll your own" leader key allows you
;; to reuse emacs binds. Now you have integration with all emacs packages
;; out of the box, no setup required. RYO leader keys require you to manually
;; rebind everything (a lot of meaningless work) and the result is esoteric.

;; TLDR devil makes evil compatible with the vanilla Emacs keymap (and packages which expect it)
;; No need for "specialized" packages for evil anymore

(use-package devil
  :after evil
  :bind
  (:map evil-motion-state-map
   ("SPC" . devil)
   ("C-h k" . devil-helpful-key))
  :config
  (defun devil-helpful-key ()
    "Describe a Devil key sequence with helpful."
    (interactive)
    (devil--log "Activated with %s" (key-description (this-command-keys)))
    (let* ((result (devil--read-key devil-describe-prompt (vector)))
           (key (devil--aget 'key result))
           (translated-key (devil--aget 'translated-key result))
           (binding (devil--aget 'binding result)))
      (devil--log "Read key: %s => %s => %s => %s"
                  key (key-description key) translated-key binding)
      (if translated-key
          (helpful-key (kbd translated-key))
  	;; Create a transient keymap to describe special key sequence.
  	(let* ((virtual-keymap (make-sparse-keymap))
               (exit-function (set-transient-map virtual-keymap)))
          (define-key virtual-keymap key binding)
          (helpful-key key)
          (funcall exit-function)))))
  :custom
  (devil-key " ")
  (devil-repeatable-keys nil)
  (devil-global-sets-buffer-default t)
  ;; ;; Use vim leader as a way to execute emacs binds without control
  ;; (devil-translations
  ;;  '(("%k %k %k" . "C-M-")
  ;;    ("%k %k" . "M-")
  ;;    ("%k" . "C-")))
  ;; (devil-special-keys
  ;;  '(("%k %k %k %k" . ignore)
  ;;    ("%k <escape>" . ignore)
  ;;    ("%k %k <escape>" . ignore)
  ;;    ("%k %k %k <escape>" . ignore)
  ;;    ("%k x <escape>" . ignore)
  ;;    ("%k x %k <escape>" . ignore)
  ;;    ("%k c <escape>" . ignore)
  ;;    ("%k c %k <escape>" . ignore)
  ;;    ("%k h <escape>" . ignore)
  ;;    ("%k %k g <escape>" . ignore)
  ;;    ("%k %k f <escape>" . ignore))
  ;; Use C-c binds as the leader map
  (devil-translations
   '(("%k" . "C-c")))
  (devil-special-keys
   '(("%k %k" . ignore)
     ("%k <escape>" . ignore))))

;; Leader keys
(cory/define-keys
 global-map
 ("C-c o" . find-file)
 ("C-c e" . eval-last-sexp-command)
 ("C-c b" . tabspaces-switch-to-buffer))

;;; Vim training

;; (use-package evil-motion-trainer
;;   :after evil
;;   :config
;;   (global-evil-motion-trainer-mode 1)
;;   (setq evil-motion-trainer-threshold 5))

;;; Nice to have Keybinds

(cory/define-keys
 global-map
 ("C-x k" . kill-this-buffer)
 ("C-x K" . kill-buffer)
 ("C-x C-b" . ibuffer)
 ("C-c w" . woman)
 ("C-'"   . repeat)
 ("C-<f4>" . kill-this-buffer))

;; (cory/define-keys
;;  evil-normal-state-map
;;  ("C-b" . switch-to-buffer))

(global-set-key (kbd "M-f") search-map)

(cory/define-keys
 minibuffer-mode-map
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 minibuffer-local-completion-map
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 isearch-mode-map
 ("TAB" . isearch-complete)
 ("C-s" . nil)
 ("C-f" . isearch-repeat-forward)
 ("M-s '" . nil)
 ("M-s C-e" . nil)
 ("M-s M-<" . nil)
 ("M-s M->" . nil)
 ("M-s SPC" . nil)
 ("M-s _" . nil)
 ("M-s c" . nil)
 ("M-s e" . nil)
 ("M-s h l" . nil)
 ("M-s h r" . nil)
 ("M-s i" . nil)
 ("M-s o" . nil)
 ("M-s r" . nil)
 ("M-s w" . nil)
 ("M-f '" . isearch-toggle-char-fold)
 ("M-f C-e" . isearch-yank-line)
 ("M-f M-<" . isearch-beginning-of-buffer)
 ("M-f M->" . isearch-end-of-buffer)
 ("M-f SPC" . isearch-toggle-lax-whitespace)
 ("M-f _" . isearch-toggle-symbol)
 ("M-f c" . isearch-toggle-case-fold)
 ("M-f e" . isearch-edit-string)
 ("M-f h l" . isearch-highlight-lines-matching-regexp)
 ("M-f h r" . isearch-highlight-regexp)
 ("M-f i" . isearch-toggle-invisible)
 ("M-f o" . isearch-occur)
 ("M-f r" . isearch-toggle-regexp)
 ("M-f w" . isearch-toggle-word))

;;; CUA Compatibility

(cua-mode 1)

(cory/define-keys
 global-map
 ("C-o" . find-file)
 ("C-s" . save-buffer)
 ("C-S-s" . write-file))

(cory/define-keys
 evil-normal-state-map
 ("C-o" . find-file))

(cory/define-keys
 evil-motion-state-map
 ("C-f" . isearch-forward))

;;; Scroll Keybinds

(global-set-key (kbd "C-<mouse-4>") #'ignore)
(global-set-key (kbd "C-<mouse-5>") #'ignore)
(global-set-key (kbd "C-<wheel-down>") #'ignore)
(global-set-key (kbd "C-<wheel-up>") #'ignore)
(global-set-key (kbd "C-M-<mouse-4>") #'ignore)
(global-set-key (kbd "C-M-<mouse-5>") #'ignore)
(global-set-key (kbd "C-M-<wheel-down>") #'ignore)
(global-set-key (kbd "C-M-<wheel-up>") #'ignore)

;;; Escape as keyboard-quit

(define-key global-map [escape] #'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
