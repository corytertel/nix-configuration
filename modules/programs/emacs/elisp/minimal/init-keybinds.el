;;
;; --- KEYBINDING FIX ---
;;
(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])
(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (define-key input-decode-map [?\C-m] [C-m])
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
;; (global-set-key "\C-q" ctl-x-map)
;; (global-set-key "\C-x" 'kill-region)

(global-set-key (kbd "M-f") search-map)

;; global binds
(cory/define-keys
 global-map
 ("<left>" . backward-char)
 ("<right>" . forward-char)
 ("C-y" . move-end-of-line)
 ("<C-i>" . previous-line)
 ("C-o" . find-file)
 ("C-p" . recenter-top-bottom)
 ("C-a" . mark-whole-buffer)
 ("C-s" . save-buffer)
 ("C-S-s" . write-file)
 ("C-f" . isearch-forward)
 ("<find>" . isearch-forward)
 ("C-j" . backward-char)
 ("C-e" . next-line)
 ("C-l" . forward-char)
 ("C-z" . undo-only)
 ("C-S-z" . undo-redo)
 ("<undo>" . undo-only)
 ("<redo>" . undo-redo)
 ("C-q" . kill-region)
 ("C-w" . kill-ring-save)
 ("C-v" . yank)
 ("C-b" . move-beginning-of-line)
 ("C-n" . scroll-up-command)
 ("C-/" . nil)
 ("C-\\" . quoted-insert)
 ("M-w" . nil)
 ("M-y" . forward-sentence)
 ("M-i" . backward-paragraph)
 ("M-p" . downcase-word)
 ("M-a" . default-indent-new-line)
 ("M-s" . tab-to-tab-stop)
 ("M-j" . backward-word)
 ("M-e" . forward-paragraph)
 ("M-l" . forward-word)
 ("M-v" . yank-pop)
 ("M-b" . backward-sentence)
 ("M-n" . scroll-down-command)
 ("C-M-i" . backward-list)
 ("C-M-a" . default-indent-new-line)
 ("C-M-b" . beginning-of-defun)
 ("C-M-e" . forward-list)
 ("C-M-f" . isearch-forward-regexp)
 ("M-<find>" . isearch-forward-regexp)
 ("C-M-j" . backward-sexp)
 ("C-M-l" . forward-sexp)
 ("C-M-n" . scroll-other-window)
 ("C-M-S-n" . scroll-other-window-down)
 ("C-M-p" . reposition-window)
 ("C-M-s" . completion-at-point)
 ("C-M-v" . nil)
 ("C-M-S-v" . nil)
 ("C-M-w" . nil)
 ("C-M-y" . end-of-defun)
 ("C-M-x" . append-next-kill))

;; minibuffer binds
(cory/define-keys
 minibuffer-mode-map
 ("C-j" . nil)
 ("M-n" . nil)
 ("M-p" . nil)
 ("M-e" . next-history-element)
 ("M-i" . previous-history-element)
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 minibuffer-local-completion-map
 ("C-j" . nil)
 ("M-n" . switch-to-completions)
 ("M-p" . nil)
 ("M-v" . nil)
 ("M-i" . previous-history-element)
 ("M-e" . next-history-element)
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
 ("e" . next-completion)
 ("i" . previous-completion))

;; isearch binds
(cory/define-keys
 isearch-mode-map
 ("TAB" . isearch-complete)
 ("C-M-i" . nil)
 ("C-M-w" . nil)
 ("C-M-y" . nil)
 ("C-M-x" . isearch-yank-symbol-or-char)
 ("C-M-v" . isearch-yank-char)
 ("C-s" . nil)
 ("C-f" . isearch-repeat-forward)
 ("C-w" . nil)
 ("C-x" . isearch-yank-word-or-char)
 ("C-y" . nil)
 ("C-v" . isearch-yank-kill)
 ("M-y" . isearch-edit-string)
 ("M-p" . nil)
 ("M-n" . nil)
 ("M-i" . isearch-ring-retreat)
 ("M-e" . isearch-ring-advance)
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
 ("M-f C-y" . isearch-yank-line)
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
  (define-key kmacro-keymap (kbd "C-i") #'kmacro-cycle-ring-previous)
  (define-key kmacro-keymap (kbd "C-e") #'kmacro-cycle-ring-next)
  (define-key kmacro-keymap (kbd "C-y") #'kmacro-edit-macro-repeat))

;; info binds
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "C-M-e") #'Info-next-reference)
  (define-key Info-mode-map (kbd "S") nil)
  (define-key Info-mode-map (kbd "F") #'Info-search-case-sensitively)
  (define-key Info-mode-map (kbd "n") nil)
  (define-key Info-mode-map (kbd "y") #'end-of-buffer)
  (define-key Info-mode-map (kbd "e") #'Info-next)
  (define-key Info-mode-map (kbd "p") #'Info-index)
  (define-key Info-mode-map (kbd "i") #'Info-prev)
  (define-key Info-mode-map (kbd "s") #'Info-follow-reference)
  (define-key Info-mode-map (kbd "f") #'Info-search))

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

(cory/define-keys
 global-map
 ("C-x K" . kill-buffer)
 ("C-c w" . woman)
 ("C-'"   . repeat)
 ("C-c F" . cory/create-tmp-file)
 ("S-SPC" . cory/insert-space)
 ("C-c q" . quit-window)
 ("C-c x" . xref-find-references-and-replace)
 ("M-S-SPC" . cycle-spacing))

;; Scroll Keybinds
;; (global-set-key (kbd "<mouse-4>") #'previous-line)
;; (global-set-key (kbd "<mouse-5>") #'next-line)
;; (global-set-key (kbd "<mouse-6>") #'backward-char)
;; (global-set-key (kbd "<mouse-7>") #'forward-char)
;; (global-set-key [left-fringe mouse-4] #'previous-line)
;; (global-set-key [left-fringe mouse-5] #'next-line)
;; (global-set-key [left-fringe mouse-6] #'backward-char)
;; (global-set-key [left-fringe mouse-7] #'forward-char)
;; (global-set-key [left-margin mouse-4] #'previous-line)
;; (global-set-key [left-margin mouse-5] #'next-line)
;; (global-set-key [left-margin mouse-6] #'backward-char)
;; (global-set-key [left-margin mouse-7] #'forward-char)
;; (global-set-key [right-fringe mouse-4] #'previous-line)
;; (global-set-key [right-fringe mouse-5] #'next-line)
;; (global-set-key [right-fringe mouse-6] #'backward-char)
;; (global-set-key [right-fringe mouse-7] #'forward-char)
;; (global-set-key [right-margin mouse-4] #'previous-line)
;; (global-set-key [right-margin mouse-5] #'next-line)
;; (global-set-key [right-margin mouse-6] #'backward-char)
;; (global-set-key [right-margin mouse-7] #'forward-char)
;; (global-set-key [left-fringe mouse-1]  #'cory/mouse-goto-bol)
;; (global-set-key [right-margin mouse-1] #'cory/mouse-goto-eol)

;;; General Programming Keybinds
(define-key prog-mode-map [remap newline] #'cory/newline-dwim)
(define-key prog-mode-map [remap backward-sentence] #'cory/beginning-of-list)
(define-key prog-mode-map [remap forward-sentence] #'cory/end-of-list)
(define-key prog-mode-map [remap mark-paragraph] #'cory/mark-list)

;;; General Text Keybinds
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

;;; Repeat Maps

(defun repeaters-define-maps (rlist)
  "Define an arbitrary number of repeater maps.
Maps are defined based on the lists passed through RLIST, a
quoted list containing ‘repeat-map’ definitions.  Each definition
is itself a list containing the following items:
NAME is a string designating the unique portion of the
repeat-map’s name (to be constructed into the form
‘repeaters-NAME-rep-map’ as the name of the symbol for the map).
One or more command ENTRIES made up of the following:
    The COMMAND’s symbol;
    One or more string representations of KEY-SEQUENCES which
    may be used to invoke the command when the ‘repeat-map’ is
    active;
    Optionally, the KEYWORD ‘:exitonly’ may follow the key sequences.
A single map definition may include any number of these command
entry constructs.
If a command construct ends with the ‘:exitonly’ keyword, the map
can invoke the command, but the command will *not* invoke that
map.
However, if the keyword is omitted, the command will bring up the
‘repeat-map’ whenever it is called using one of the keysequences
given in the ‘repeat-map’.  A given command may only store a
single map within its ‘repeat-map’ property, although a command
can be called from multiple repeat-maps.
Taking advantage of this fact, one may chain related repeat-maps
together in sequence."
  (while rlist
    (let* ((block (pop rlist))
           (mapname (concat "repeaters-" (pop block) "-rep-map")))
      (set (intern mapname)
           (let ((map (make-sparse-keymap))
                 (thing (pop block)))
             (while block
               (let ((thingnext (pop block)))
                 (while (stringp thingnext)
                   (define-key map (kbd thingnext) thing)
                   (setq thingnext (pop block)))
                 (if (eq thingnext :exitonly)
                     (setq thing (pop block))
                   (progn (put thing 'repeat-map (intern mapname))
                          (setq thing thingnext)))))
             map)))))

(defvar repeaters-maps
  '(("buffer-switch"
     previous-buffer                   "C-x C-<left>" "C-x <left>" "C-<left>" "<left>" "i"
     next-buffer                       "C-x C-<right>" "C-x <right>" "C-<right>" "<right>" "e")

    ("calendar-nav"
     calendar-forward-day              "C-l" "l"
     calendar-backward-day             "C-j" "j"
     calendar-forward-week             "C-e" "e"
     calendar-backward-week            "<C-i>" "i"
     calendar-forward-month            "M-}" "}" "]"
     calendar-backward-month           "M-{" "{" "["
     calendar-forward-year             "C-x ]"
     calendar-backward-year            "C-x [")

    ("char-line-nav"
     backward-char                     "C-j" "j" "<left>"
     forward-char                      "C-l" "l" "<right>"
     next-line                         "C-e" "e" "<down>"
     previous-line                     "<C-i>" "i" "<up>"
     move-beginning-of-line            "C-b" "b" "<home>"
     move-end-of-line                  "C-y" "y" "<end>")

    ("defun-nav"
     beginning-of-defun                "C-M-b" "M-b" "b" "ESC M-b"
     end-of-defun                      "C-M-y" "M-y" "y" "ESC M-y")

    ("del-char"
     delete-char                       "C-d" "d")

    ("sexp-nav"
     backward-sexp                     "C-M-j" "j" "ESC M-j"
     forward-sexp                      "C-M-l" "l" "ESC M-l")

    ("paragraph-nav"
     backward-paragraph                "C-<up>" "<up>" "M-{" "M-[" "{" "["
     forward-paragraph                 "C-<down>" "<down>" "M-}" "M-]" "}" "]")

    ("sentence-nav"
     backward-sentence                 "M-b" "b"
     forward-sentence                  "M-y" "y"
     back-to-indentation               "M-m" "m"                     :exitonly)

    ("page-nav"
     backward-page                     "C-x [" "["
     forward-page                      "C-x ]" "]")

    ("list-nav"
     backward-list                     "C-M-i" "i" "ESC M-i"
     forward-list                      "C-M-e" "e" "ESC M-e"
     backward-up-list                  "C-M-<up>" "C-M-u" "<up>" "u" "ESC M-u"
     down-list                         "C-M-<down>" "C-M-d" "<down>" "d" "ESC M-d")

    ("error-nav"
     next-error                        "C-x `" "`" "M-g M-e" "M-g e" "e"
     previous-error                    "M-g M-i" "M-i" "i")

    ("mid-top-bottom-move"
     recenter-top-bottom               "C-p" "p"
     move-to-window-line-top-bottom    "M-r" "r"
     back-to-indentation               "M-m" "m"                     :exitonly)

    ("fix-case"
     upcase-word                       "M-u" "u"

     ;; Easy way to manually set title case
     downcase-word                     "M-p" "p" "d"
     capitalize-word                   "M-c" "c")

    ("kill-word"
     kill-word                         "M-d" "M-<delete>" "d")

    ("kill-line"
     kill-line                         "C-k" "k")

    ("kill-sentence"
     kill-sentence                     "M-k" "k"
     backward-kill-sentence            "C-x DEL" "DEL")

    ("kill-sexp"
     kill-sexp                         "C-M-k" "k" "ESC M-k")

    ;; Yank same text repeatedly with “C-v v v v”...
    ("yank-only"
     yank                              "C-v" "v"
     yank-pop                          "M-v" "e"                     :exitonly)

    ;; Cycle through the kill-ring with “C-v e e e”...
    ;; You can reverse direction too “C-v e e C-- e e”
    ("yank-popping"
     yank-pop                          "M-v" "v" "e")

    ("kmacro-cycle"
     kmacro-cycle-ring-next            "C-x C-k C-e" "C-e" "e"
     kmacro-cycle-ring-previous        "C-x C-k C-i" "<C-i>" "i")

    ("tab-bar-nav"
     tab-next                          "C-x t o" "o" "e"
     tab-previous                      "C-x t O" "O" "i")

    ("transpose-chars"
     transpose-chars                    "C-t" "t")

    ("transpose-words"
     transpose-words                   "M-t" "t")

    ("transpose-sexps"
     transpose-sexps                   "C-M-t" "t" "ESC M-t")

    ("transpose-lines"
     transpose-lines                   "C-x C-t" "t")

    ;; M-< for beginning-of-buffer brings up this map, since you can
    ;; only scroll a buffer up when at its beginning.
    ("scroll-up"
     scroll-up-command                 "C-n" "n"
     beginning-of-buffer               "M-<" "<"
     end-of-buffer                     "M->" ">"                     :exitonly
     scroll-down-command               "M-n"                         :exitonly)

    ;; M-> for end-of buffer brings up this map, since you can only
    ;; scroll a buffer down when at its end.
    ("scroll-down"
     scroll-down-command               "M-n" "n"
     end-of-buffer                     "M->" ">"
     beginning-of-buffer               "M-<" "<"                     :exitonly
     scroll-up-command                 "C-n"                         :exitonly)

    ("scroll-otherwin"
     scroll-other-window               "C-M-n" "n" "ESC M-n"
     beginning-of-buffer-other-window  "M-<home>" "<"
     end-of-buffer-other-window        "M-<end>" ">"                 :exitonly
     scroll-other-window-down          "C-M-S-n" "M-n" "ESC M-N" "N" :exitonly)

    ("scroll-otherwin-down"
     scroll-other-window-down          "C-M-S-n" "M-n" "n" "ESC M-N" "N"
     end-of-buffer-other-window        "M-<end>" ">"
     beginning-of-buffer-other-window  "M-<home>" "<"                :exitonly
     scroll-other-window               "C-M-n" "C-n" "ESC M-n"       :exitonly)

    ("scroll-sideways"
     scroll-left                       "C-x <" "<"
     scroll-right                      "C-x >" ">")

    ("hippie-exp"
     ;; For navigating through expansion candidates. You can revert
     ;; to the original string by prefixing the next hippie-expand
     ;; invocation with universal-argument (“C-u /”).
     hippie-expand                     "M-/" "/")

    ("search-nav"
     isearch-repeat-forward            "C-f" "f" "C-M-f" "ESC M-f"
     isearch-repeat-backward           "C-r" "r" "C-M-r" "ESC M-r"
     isearch-exit                      "<enter>" "<return>" "RET"    :exitonly)

    ("undo-only-redo"
     undo-only                         "C-x u" "C-_" "_" "C-z" "z"
     undo-redo                         "C-S-z" "Z" "r")

    ;; Repeat Maps for Org-Mode
    ("org-nav"
     org-backward-heading-same-level   "C-c C-j" "C-j" "j"
     org-forward-heading-same-level    "C-c C-l" "C-l" "l"
     org-previous-visible-heading      "C-c C-i" "<C-i>" "i"
     org-next-visible-heading          "C-c C-e" "C-e" "e"
     outline-up-heading                "C-c C-u" "C-u" "u")

    ("org-editing"
     org-metadown                      "M-<down>" "<down>"
     org-metaup                        "M-<up>" "<up>"
     org-demote-subtree                "C->" ">"
     org-promote-subtree               "C-<" "<")

    ("org-task"
     org-todo                          "C-c C-t" "C-t" "t"
     org-priority                      "C-c ," ","
     org-time-stamp                    "C-c ." "."
     org-schedule                      "C-c C-s" "C-s" "s"
     org-deadline                      "C-c C-d" "C-d" "d")

    ("word-nav"
     backward-word                     "M-j" "j"
     forward-word                      "M-l" "l")

    ("set-mark"
     smart-region                      "C-SPC" "SPC")

    ("macrursors"
     macrursors-mark-next-instance-of "C->" ">" "e"
     macrursors-mark-next-instance-of "C-<" "<" "i"))

  "List of lists containing repeater-map definitions.
This must be in the form required by the
‘repeaters-define-maps’ function.")

(repeaters-define-maps repeaters-maps)
(setq repeat-exit-key "g"
      repeat-exit-timeout 30)
(repeat-mode)

;;; Misc useful functions

(defun describe-all-keymaps ()
  "Describe all keymaps in currently-defined variables."
  (interactive)
  (with-output-to-temp-buffer "*keymaps*"
    (let (symbs seen)
      (mapatoms (lambda (s)
                  (when (and (boundp s) (keymapp (symbol-value s)))
                    (push (indirect-variable s) symbs))))
      (dolist (keymap symbs)
        (unless (memq keymap seen)
          (princ (format "* %s\n\n" keymap))
          (princ (substitute-command-keys (format "\\{%s}" keymap)))
          (princ (format "\f\n%s\n\n" (make-string (min 80 (window-width)) ?-)))
          (push keymap seen))))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'describe-all-keymaps)))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x x k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x x r") 'rename-current-buffer-file)
