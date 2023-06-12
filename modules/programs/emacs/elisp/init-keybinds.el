;;; Keybinding Fix

;; (define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])
(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    ;; (define-key input-decode-map [?\C-m] [C-m])
	    (define-key input-decode-map [?\C-i] [C-i])))

;; Now:
;; (equal (kbd "TAB") (kbd "C-i"))   ; -> t
;; (equal (kbd "TAB") (kbd "<C-i>")) ; -> nil
;; (equal (kbd "RET") (kbd "C-m"))   ; -> t
;; (equal (kbd "RET") (kbd "<C-m>")) ; -> nil

(define-key function-key-map
  [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map
  [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map
  [(meta control shift iso-lefttab)] [(meta control shift tab)])

;;
;; --- CUA Keybinds ---
;;

;; List of things to rebind to make Emacs have CUA:
;; - global binds
;; - minibuffer binds
;; - search-map binds
;; - goto-map binds
;; - isearch binds
;; - kmacro-binds
;; - dired binds
;; - eww binds?
;; - sgml binds
;; - paredit binds
;; - org-mode binds
;; - org agenda binds
;; - ibuffer binds
;; - eshell binds
;; - repeat maps
;; - helpful binds
;; - geiser binds
;; - info binds

(defmacro cory/define-keys (map &rest l)
  ""
  `(dolist (pair ',l)
     (define-key ,map (kbd (car pair)) (cdr pair))))

;; Swap "C-q" and "C-x"
;; Swap "C-w" and "C-c"
(keyboard-translate ?\C-q ?\C-x)
(keyboard-translate ?\C-x ?\C-q)
(keyboard-translate ?\C-w ?\C-c)
(keyboard-translate ?\C-c ?\C-w)
(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (keyboard-translate ?\C-q ?\C-x)
	    (keyboard-translate ?\C-x ?\C-q)
	    (keyboard-translate ?\C-w ?\C-c)
	    (keyboard-translate ?\C-c ?\C-w)))
;; (global-set-key "\C-q" ctl-x-map)
;; (global-set-key "\C-x" 'kill-region)

(global-set-key (kbd "M-f") search-map)

;; global binds
(cory/define-keys
 global-map
 ("<left>" . backward-char)
 ("<right>" . forward-char)
 ("C-e" . move-end-of-line)
 ("<C-k>" . previous-line)
 ("C-o" . find-file)
 ("C-p" . recenter-top-bottom)
 ("C-b" . switch-to-buffer)
 ("C-s" . save-buffer)
 ("C-S-s" . write-file)
 ("C-f" . isearch-forward)
 ("<find>" . isearch-forward)
 ("C-h" . backward-char)
 ("C-j" . next-line)
 ("C-l" . forward-char)
 ("C-z" . undo-only)
 ("C-S-z" . undo-redo)
 ("<undo>" . undo-only)
 ("<redo>" . undo-redo)
 ("C-q" . kill-region)
 ("C-w" . kill-ring-save)
 ("C-v" . yank)
 ("C-a" . move-beginning-of-line)
 ("C-n" . scroll-up-command)
 ("C-/" . nil)
 ("C-\\" . quoted-insert)
 ("M-w" . nil)
 ("M-e" . forward-sentence)
 ("M-p" . downcase-word)
 ("M-b" . default-indent-new-line)
 ("M-s" . tab-to-tab-stop)
 ("M-h" . backward-word)
 ("M-l" . forward-word)
 ("M-v" . yank-pop)
 ("M-a" . backward-sentence)
 ("M-n" . scroll-down-command)
 ("C-M-k" . backward-list)
 ("C-M-b" . default-indent-new-line)
 ("C-M-a" . beginning-of-defun)
 ("C-M-j" . forward-list)
 ("C-M-f" . isearch-forward-regexp)
 ("M-<find>" . isearch-forward-regexp)
 ("C-M-h" . backward-sexp)
 ("C-M-l" . forward-sexp)
 ("C-M-n" . scroll-other-window)
 ("C-M-S-n" . scroll-other-window-down)
 ("C-M-p" . reposition-window)
 ("C-M-s" . completion-at-point)
 ("C-M-v" . nil)
 ("C-M-S-v" . nil)
 ("C-M-w" . nil)
 ("C-M-e" . end-of-defun)
 ("C-M-x" . append-next-kill)
 ("C-M-\\" . toggle-input-method)
 ("M-i"   . mark-paragraph)
 ("C-M-i" . mark-defun))

;; minibuffer binds
(cory/define-keys
 minibuffer-mode-map
 ("C-j" . nil)
 ("M-n" . nil)
 ("M-p" . nil)
 ("M-j" . next-history-element)
 ("M-k" . previous-history-element)
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 minibuffer-local-completion-map
 ("C-j" . nil)
 ("M-n" . switch-to-completions)
 ("M-p" . nil)
 ("M-v" . nil)
 ("M-k" . previous-history-element)
 ("M-j" . next-history-element)
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
 ("j" . next-completion)
 ("k" . previous-completion))

;; isearch binds
(cory/define-keys
 isearch-mode-map
 ("TAB" . isearch-complete)
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
 ("M-k" . isearch-ring-retreat)
 ("M-j" . isearch-ring-advance)
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
  (define-key kmacro-keymap (kbd "C-k") #'kmacro-cycle-ring-previous)
  (define-key kmacro-keymap (kbd "C-j") #'kmacro-cycle-ring-next)
  (define-key kmacro-keymap (kbd "C-e") #'kmacro-edit-macro-repeat))

;; info binds
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "C-M-j") #'Info-next-reference)
  (define-key Info-mode-map (kbd "S") nil)
  (define-key Info-mode-map (kbd "F") #'Info-search-case-sensitively)
  (define-key Info-mode-map (kbd "n") nil)
  (define-key Info-mode-map (kbd "e") #'end-of-buffer)
  (define-key Info-mode-map (kbd "j") #'Info-next)
  (define-key Info-mode-map (kbd "p") #'Info-index)
  (define-key Info-mode-map (kbd "k") #'Info-prev)
  (define-key Info-mode-map (kbd "s") #'Info-follow-reference)
  (define-key Info-mode-map (kbd "f") #'Info-search))

;; tetris binds
(with-eval-after-load 'tetris
  (define-key tetris-mode-map (kbd "j") #'tetris-move-down)
  (define-key tetris-mode-map (kbd "k") #'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "l") #'tetris-move-right)
  (define-key tetris-mode-map (kbd "h") #'tetris-move-left))

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
(global-set-key [remap find-file] #'cory/find-file)

;; TODO `k' and `SPC' key variation in text and prog modes
;; `M-b' and `M-y' binds in prog modes
;; The swaping of `q' and `x' and `w' and `c'

(cory/define-keys
 global-map
 ("C-M-b" . cory/join-next-line)
 ("C-x K" . kill-buffer)
 ("C-c w" . woman)
 ("C-'"   . repeat)
 ("C-c F" . cory/create-tmp-file)
 ("S-SPC" . cory/insert-space)
 ("C-c q" . quit-window)
 ("C-c x" . xref-find-references-and-replace)
 ("M-S-SPC" . cycle-spacing)
 ("S-<delete>" . kill-whole-line)
 ("C-M-y" . kill-whole-line)
 ("M-SPC" . cory/mark-sexp)
 ("C-M-SPC" . cory/mark-line)
 ("C-M-<return>" . open-line)
 ("C-<f4>" . kill-this-buffer)
 ("<deletechar>" . delete-char))

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

;;; Misc
(global-set-key (kbd "C-x x k") 'cory/delete-current-buffer-file)
(global-set-key (kbd "C-x x r") 'cory/rename-current-buffer-file)
