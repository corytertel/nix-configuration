;;; Keybinding Fix

(define-key function-key-map
  [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map
  [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map
  [(meta control shift iso-lefttab)] [(meta control shift tab)])

;;
;; --- CUA Keybinds ---
;;

(defmacro cory/define-keys (map &rest l)
  ""
  `(dolist (pair ',l)
     (define-key ,map (kbd (car pair)) (cdr pair))))

(define-key input-decode-map [?\C-q] [?\C-x])
(define-key input-decode-map [?\C-x] [?\C-q])
(define-key input-decode-map [?\C-j] [?\C-c])
(define-key input-decode-map [?\C-c] [?\C-j])
(define-key input-decode-map [?\C-i] [?\C-h])
(define-key input-decode-map [?\C-h] [C-h])

(global-set-key (kbd "M-f") search-map)

;; global binds
(cory/define-keys
 global-map
 ("<left>" . backward-char)
 ("<right>" . forward-char)
 ("C-<left>" . backward-word)
 ("C-<right>" . forward-word)
 ("C-e" . move-end-of-line)
 ("C-t" . previous-line)
 ("C-o" . find-file)
 ("C-l" . recenter-top-bottom)
 ("C-s" . save-buffer)
 ("C-S-s" . write-file)
 ("C-f" . isearch-forward)
 ("<find>" . isearch-forward)
 ("C-d" . backward-char)
 ("<C-h>" . next-line)
 ("C-n" . forward-char)
 ("C-z" . undo-only)
 ("C-S-z" . undo-redo)
 ("<undo>" . undo-only)
 ("<redo>" . undo-redo)
 ("C-q" . kill-region)
 ("C-j" . kill-ring-save)
 ("C-v" . yank)
 ("C-a" . move-beginning-of-line)
 ("C-p" . scroll-up-command)
 ("<next>" . scroll-up-command)
 ("C-/" . nil)
 ("C-\\" . quoted-insert)
 ("M-w" . nil)
 ("M-e" . forward-sentence)
 ("M-l" . downcase-word)
 ("M-j" . default-indent-new-line)
 ("M-s" . tab-to-tab-stop)
 ("M-d" . backward-word)
 ("M-n" . forward-word)
 ("M-v" . yank-pop)
 ("M-a" . backward-sentence)
 ("M-p" . scroll-down-command)
 ("<prior>" . scroll-down-command)
 ("C-M-t" . backward-list)
 ("C-M-j" . default-indent-new-line)
 ("C-M-a" . beginning-of-defun)
 ("C-M-h" . forward-list)
 ("C-M-f" . isearch-forward-regexp)
 ("M-<find>" . isearch-forward-regexp)
 ("C-M-d" . backward-sexp)
 ("C-M-n" . forward-sexp)
 ("C-M-p" . scroll-other-window)
 ("C-M-S-p" . scroll-other-window-down)
 ("C-M-l" . reposition-window)
 ("C-M-s" . completion-at-point)
 ("C-M-v" . nil)
 ("C-M-S-v" . nil)
 ("C-M-w" . nil)
 ("C-M-e" . end-of-defun)
 ("C-M-x" . append-next-kill)
 ("C-M-\\" . toggle-input-method)
 ("C-x <down>" . eval-last-sexp)
 ("C-x <home>" . list-buffers)
 ("C-b" . switch-to-buffer)
 ("M-b" . transpose-words)
 ("C-M-b" . transpose-sexps)
 ("C-x C-t" . nil)
 ("C-x C-b" . transpose-lines)
 ("C-w" . nil)
 ("C-y" . delete-char)
 ("M-y" . kill-word)
 ("C-M-y" . down-list)
 ("M-i" . mark-paragraph)
 ("C-M-i" . mark-defun)
 ("C-x i" . mark-whole-buffer)
 ("C-x h" . insert-file))

;; minibuffer binds
(cory/define-keys
 minibuffer-mode-map
 ("C-j" . nil)
 ("M-n" . nil)
 ("M-p" . nil)
 ("M-h" . next-history-element)
 ("M-t" . previous-history-element)
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 minibuffer-local-completion-map
 ("C-j" . nil)
 ("M-p" . switch-to-completions)
 ("M-n" . nil)
 ("M-v" . nil)
 ("M-t" . previous-history-element)
 ("M-h" . next-history-element)
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 minibuffer-local-must-match-map
 ("C-j" . nil))

(cory/define-keys
 read--expression-map
 ("C-j" . nil))

(cory/define-keys
 completion-list-mode-map
 ("n" . nil)
 ("p" . nil)
 ("h" . next-completion)
 ("t" . previous-completion))

;; isearch binds
(cory/define-keys
 isearch-mode-map
 ("TAB" . isearch-complete)
 ("C-M-i" . nil)
 ("C-M-w" . nil)
 ("C-M-y" . nil)
 ("C-M-x" . isearch-yank-symbol-or-char)
 ("C-M-v" . isearch-yank-char)
 ("C-M-s" . nil)
 ("C-s" . nil)
 ("C-f" . isearch-repeat-forward)
 ("C-M-f" . isearch-repeat-forward)
 ("<find>" . isearch-repeat-forward)
 ("C-w" . nil)
 ("C-x" . isearch-yank-word-or-char)
 ("C-y" . nil)
 ("C-v" . isearch-yank-kill)
 ("M-e" . isearch-edit-string)
 ("M-p" . nil)
 ("M-n" . nil)
 ("M-t" . isearch-ring-retreat)
 ("M-h" . isearch-ring-advance)
 ("M-v" . isearch-yank-pop-only)
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

;; kmacro binds
(with-eval-after-load 'kmacro
  (define-key kmacro-keymap (kbd "C-n") nil)
  (define-key kmacro-keymap (kbd "C-p") nil)
  (define-key kmacro-keymap (kbd "C-t") #'kmacro-cycle-ring-previous)
  (define-key kmacro-keymap (kbd "<C-h>") #'kmacro-cycle-ring-next)
  (define-key kmacro-keymap (kbd "C-e") #'kmacro-edit-macro-repeat)
  (define-key kmacro-keymap (kbd "<up>") #'kmacro-cycle-ring-previous)
  (define-key kmacro-keymap (kbd "<down>") #'kmacro-cycle-ring-next))

;; info binds
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "C-M-h") #'Info-next-reference)
  (define-key Info-mode-map (kbd "S") nil)
  (define-key Info-mode-map (kbd "F") #'Info-search-case-sensitively)
  (define-key Info-mode-map (kbd "n") nil)
  (define-key Info-mode-map (kbd "e") #'end-of-buffer)
  (define-key Info-mode-map (kbd "h") #'Info-next)
  (define-key Info-mode-map (kbd "i") #'Info-index)
  (define-key Info-mode-map (kbd "t") #'Info-prev)
  (define-key Info-mode-map (kbd "s") #'Info-follow-reference)
  (define-key Info-mode-map (kbd "f") #'Info-search))

;; tetris binds
(with-eval-after-load 'tetris
  (define-key tetris-mode-map (kbd "h") #'tetris-move-down)
  (define-key tetris-mode-map (kbd "t") #'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "d") #'tetris-move-left)
  (define-key tetris-mode-map (kbd "n") #'tetris-move-right))

;; goto binds
(cory/define-keys
 goto-map
 ("M-n" . nil)
 ("M-p" . nil)
 ("n" . nil)
 ("p" . nil)
 ("M-h" . next-error)
 ("M-t" . previous-error)
 ("h" . next-error)
 ("t" . previous-error))

;; c binds
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-M-i") #'c-mark-function)
  (define-key c-mode-base-map (kbd "C-M-h") nil)
  (define-key c-mode-base-map (kbd "C-y") #'c-electric-delete-forward)
  (define-key c-mode-base-map (kbd "C-d") nil)
  (define-key c-mode-base-map (kbd "C-c C-n") nil)
  (define-key c-mode-base-map (kbd "C-c <C-h>") #'c-forward-conditional)
  (define-key c-mode-base-map (kbd "C-c C-p") nil)
  (define-key c-mode-base-map (kbd "C-c C-t") #'c-backward-conditional))

;; elisp binds
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil)
  (define-key emacs-lisp-mode-map (kbd "C-M-s") #'completion-at-point))

;;
;; --- GENERAL KEYBINDS ---
;;

;;; Basic Keybinds

(global-set-key [remap default-indent-new-line] #'join-line)
(global-set-key [remap capitalize-word] #'capitalize-dwim)
(global-set-key [remap downcase-word] #'downcase-dwim)
(global-set-key [remap upcase-word] #'upcase-dwim)
(global-set-key [remap list-buffers] #'ibuffer)
(global-set-key [remap mark-word] #'cory/mark-word)
(global-set-key [remap mark-sexp] #'cory/mark-sexp)
(global-set-key [remap scroll-up-command] #'cory/scroll-down)
(global-set-key [remap scroll-down-command] #'cory/scroll-up)
(global-set-key [remap beginning-of-buffer] #'cory/beginning-of-workspace)
(global-set-key [remap end-of-buffer] #'cory/end-of-workspace)
(global-set-key [remap kill-buffer] #'kill-this-buffer)
(global-set-key [remap yank] #'cory/yank)

;; TODO `k' and `SPC' key variation in text and prog modes
;; `M-a' and `M-e' binds in prog modes
;; The swaping of `'' and `x' and `,' and `c'

(cory/define-keys
 global-map
 ("C-M-j" . cory/join-next-line)
 ("C-x K" . kill-buffer)
 ("C-c w" . woman)
 ("C-'"   . repeat)
 ("C-c F" . cory/create-tmp-file)
 ("S-SPC" . cory/insert-space)
 ("C-c q" . quit-window)
 ("C-c x" . xref-find-references-and-replace)
 ("M-S-SPC" . cycle-spacing)
 ("S-<delete>" . kill-whole-line)
 ("C-M-k" . kill-whole-line)
 ("M-SPC" . cory/mark-sexp)
 ("C-M-SPC" . cory/mark-line)
 ("C-M-<return>" . open-line)
 ("C-<f4>" . kill-this-buffer)
 ("<deletechar>" . delete-char)
 ("C-x c" . set-goal-column)
 ("C-x C-n" . make-frame-command))

;; Scroll Keybinds
(global-set-key (kbd "C-<mouse-4>") #'ignore)
(global-set-key (kbd "C-<mouse-5>") #'ignore)
(global-set-key (kbd "C-<wheel-down>") #'ignore)
(global-set-key (kbd "C-<wheel-up>") #'ignore)
(global-set-key (kbd "C-M-<mouse-4>") #'ignore)
(global-set-key (kbd "C-M-<mouse-5>") #'ignore)
(global-set-key (kbd "C-M-<wheel-down>") #'ignore)
(global-set-key (kbd "C-M-<wheel-up>") #'ignore)

;;; General Programming Keybinds
;; (define-key prog-mode-map [remap newline] #'cory/newline-dwim)
(define-key prog-mode-map [remap backward-sentence] #'cory/beginning-of-list)
(define-key prog-mode-map [remap forward-sentence] #'cory/end-of-list)
(define-key prog-mode-map [remap mark-paragraph] #'cory/mark-list)
(define-key prog-mode-map [remap kill-sentence] #'cory/kill-from-start-of-line)

;;; General Text Keybinds
(define-key text-mode-map [remap cory/mark-sexp] #'cory/mark-word)
(define-key text-mode-map [remap mark-paragraph] #'cory/mark-sentence)
(define-key text-mode-map [remap beginning-of-defun] #'backward-paragraph)
(define-key text-mode-map [remap end-of-defun] #'forward-paragraph)
(define-key text-mode-map [remap mark-defun] #'mark-paragraph)
(define-key text-mode-map (kbd "C-M-i") nil)
(define-key text-mode-map (kbd "C-M-s") #'ispell-complete-word)

;;; Escape as keyboard-quit
(define-key global-map [escape] #'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; Misc useful functions
(global-set-key (kbd "C-x x k") 'cory/delete-current-buffer-file)
(global-set-key (kbd "C-x x r") 'cory/rename-current-buffer-file)
