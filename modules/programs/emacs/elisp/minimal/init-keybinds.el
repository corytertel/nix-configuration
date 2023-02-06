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
;; - dired binds
;; - eww binds?
;; - sgml binds
;; - paredit binds

(defmacro cory/define-keys (map &rest l)
  ""
  `(dolist (pair ',l)
     (define-key ,map (kbd (car pair)) (cdr pair))))

;; Swap "C-w" and "C-x"
(keyboard-translate ?\C-w ?\C-x)
(keyboard-translate ?\C-x ?\C-w)
(keyboard-translate ?\C-r ?\C-c)
(keyboard-translate ?\C-c ?\C-r)
;; (global-set-key "\C-w" ctl-x-map)
;; (global-set-key "\C-x" 'kill-region)

(cory/define-keys
 global-map
 ("C-y" . move-end-of-line)
 ("<C-i>" . previous-line)
 ("C-o" . find-file)
 ("C-p" . recenter-top-bottom)
 ("C-a" . mark-whole-buffer)
 ("C-s" . save-buffer)
 ("C-S" . write-file)
 ("C-f" . isearch-forward)
 ("C-j" . backward-char)
 ("C-e" . next-line)
 ("C-l" . forward-char)
 ("C-z" . undo-only)
 ("C-Z" . undo-redo)
 ("C-w" . kill-region)
 ("C-r" . kill-ring-save)
 ("C-v" . yank)
 ("C-b" . beginning-of-line)
 ("C-n" . scroll-up-command)
 ("C-/" . open-line)
 ("M-w" . nil)
 ("M-y" . forward-sentence)
 ("M-i" . backward-paragraph)
 ("M-p" . downcase-word)
 ("M-a" . default-indent-new-line)
 ("M-s" . tab-to-tab-stop)
 ;; ("M-f" . search-map)
 ("M-j" . backward-word)
 ("M-e" . forward-paragraph)
 ("M-l" . forward-word)
 ("M-v" . yank-pop)
 ("M-b" . backward-sentence)
 ("M-n" . scroll-down-command))

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
 ("RET"   . cory/newline-dwim)
 ;; ("C-x k" . kill-this-buffer)
 ("C-x K" . kill-buffer)
 ;; ("C-x C-b" . ibuffer)
 ("C-c w" . woman)
 ("C-'"   . repeat)
 ;; ("C-s"   . cory/search-forward-dwim)
 ;; ("C-r"   . cory/search-backward-dwim)
 ;; ("C-M-s" . cory/isearch-forward-resume)
 ;; ("C-M-r" . cory/isearch-backward-resume)
 ;; ("C-v"   . cory/scroll-down-half-page)
 ;; ("M-v"   . cory/scroll-up-half-page)
 ;; ("C-v"   . cory/scroll-down)
 ;; ("M-v"   . cory/scroll-up)
 ("C-c F" . cory/create-tmp-file)
 ("S-SPC" . cory/insert-space)
 ;; ("M-@"   . cory/mark-word)
 ;; ("C-M-SPC" . cory/mark-sexp)
 ("C-c q" . quit-window)
 ;; ("M-j"   . join-line)
 ;; ("M-c"   . capitalize-dwim)
 ;; ("M-u"   . upcase-dwim)
 ;; ("M-l"   . downcase-dwim)
 ("C-c x" . xref-find-references-and-replace)
 ;; ("M-<"   . cory/beginning-of-workspace)
 ;; ("M->"   . cory/end-of-workspace)
 ("M-S-SPC" . cycle-spacing)
 ;; Grab Keybinds
 ("M-SPC"     . cory/grab)
 ("C-c SPC"   . cory/swap-grab)
 ("C-c S-SPC" . cory/sync-grab)
 ;; Scroll Keybinds
 ("<mouse-4>" . previous-line)
 ("<mouse-5>" . next-line)
 ("<mouse-6>" . backward-char)
 ("<mouse-7>" . forward-char))

(global-set-key [left-fringe mouse-4] #'previous-line)
(global-set-key [left-fringe mouse-5] #'next-line)
(global-set-key [left-fringe mouse-6] #'backward-char)
(global-set-key [left-fringe mouse-7] #'forward-char)
(global-set-key [left-margin mouse-4] #'previous-line)
(global-set-key [left-margin mouse-5] #'next-line)
(global-set-key [left-margin mouse-6] #'backward-char)
(global-set-key [left-margin mouse-7] #'forward-char)
(global-set-key [right-fringe mouse-4] #'previous-line)
(global-set-key [right-fringe mouse-5] #'next-line)
(global-set-key [right-fringe mouse-6] #'backward-char)
(global-set-key [right-fringe mouse-7] #'forward-char)
(global-set-key [right-margin mouse-4] #'previous-line)
(global-set-key [right-margin mouse-5] #'next-line)
(global-set-key [right-margin mouse-6] #'backward-char)
(global-set-key [right-margin mouse-7] #'forward-char)

(global-set-key [left-fringe mouse-1]  #'cory/mouse-goto-bol)
(global-set-key [right-margin mouse-1] #'cory/mouse-goto-eol)

;;; General Programming Keybinds
(cory/define-keys
 prog-mode-map
 ;; List binds
 ("M-a" . cory/beginning-of-list)
 ("M-e" . cory/end-of-list)
 ("M-h" . cory/mark-list))

;;; General Text Keybinds
(cory/define-keys
 text-mode-map
 ;;; Prose binds
 ("C-M-a" . backward-paragraph)
 ("C-M-e" . forward-paragraph)
 ("C-M-h" . mark-paragraph))

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
     previous-buffer                   "C-x C-<left>" "C-x <left>" "C-<left>" "<left>" "p"
     next-buffer                       "C-x C-<right>" "C-x <right>" "C-<right>" "<right>" "n")

    ("calendar-nav"
     calendar-forward-day              "C-f" "f"
     calendar-backward-day             "C-b" "b"
     calendar-forward-week             "C-n" "n"
     calendar-backward-week            "C-p" "p"
     calendar-forward-month            "M-}" "}" "]"
     calendar-backward-month           "M-{" "{" "["
     calendar-forward-year             "C-x ]"
     calendar-backward-year            "C-x [")

    ("char-line-nav"
     backward-char                     "C-b" "b"
     forward-char                      "C-f" "f"
     next-line                         "C-n" "n"
     previous-line                     "C-p" "p")

    ("defun-nav"
     beginning-of-defun                "C-M-a" "M-a" "a" "ESC M-a"
     end-of-defun                      "C-M-e" "M-e" "e" "ESC M-e")

    ("del-char"
     delete-char                       "C-d" "d")

    ("sexp-nav"
     backward-sexp                     "C-M-b" "b" "ESC M-b"
     forward-sexp                      "C-M-f" "f" "ESC M-f")

    ("paragraph-nav"
     backward-paragraph                "C-<up>" "<up>" "M-{" "M-[" "{" "["
     forward-paragraph                 "C-<down>" "<down>" "M-}" "M-]" "}" "]")

    ("sentence-nav"
     backward-sentence                 "M-a" "a"
     forward-sentence                  "M-e" "e"
     back-to-indentation               "M-m" "m"                     :exitonly)

    ("in-line-nav"
     move-end-of-line                  "C-a" "a"
     move-end-of-line                  "C-e" "e")

    ("page-nav"
     backward-page                     "C-x [" "["
     forward-page                      "C-x ]" "]")

    ("list-nav"
     backward-list                     "C-M-p" "p" "ESC M-p"
     forward-list                      "C-M-n" "n" "ESC M-n"
     backward-up-list                  "C-M-<up>" "C-M-u" "<up>" "u" "ESC M-u"
     down-list                         "C-M-<down>" "C-M-d" "<down>" "d" "ESC M-d")

    ("error-nav"
     next-error                        "C-x `" "`" "M-g M-n" "M-g n" "n"
     previous-error                    "M-g M-p" "M-p" "p")

    ("mid-top-bottom-move"
     recenter-top-bottom               "C-l" "l"
     move-to-window-line-top-bottom    "M-r" "r"
     back-to-indentation               "M-m" "m"                     :exitonly)

    ("fix-case"
     upcase-word                       "M-u" "u"

     ;; Easy way to manually set title case
     downcase-word                     "M-l" "l" "d"
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

    ;; Yank same text repeatedly with “C-y y y y”...
    ("yank-only"
     yank                              "C-y" "y"
     yank-pop                          "M-y" "n"                     :exitonly)

    ;; Cycle through the kill-ring with “C-y n n n”...
    ;; You can reverse direction too “C-y n n C-- n n”
    ("yank-popping"
     yank-pop                          "M-y" "y" "n")

    ("kmacro-cycle"
     kmacro-cycle-ring-next            "C-x C-k C-n" "C-n" "n"
     kmacro-cycle-ring-previous        "C-x C-k C-p" "C-p" "p")

    ("tab-bar-nav"
     tab-next                          "C-x t o" "o" "n"
     tab-previous                      "C-x t O" "O" "p")

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
     scroll-up-command                 "C-v" "v"
     beginning-of-buffer               "M-<" "<"
     end-of-buffer                     "M->" ">"                     :exitonly
     scroll-down-command               "M-v"                         :exitonly)

    ;; M-> for end-of buffer brings up this map, since you can only
    ;; scroll a buffer down when at its end.
    ("scroll-down"
     scroll-down-command               "M-v" "v"
     end-of-buffer                     "M->" ">"
     beginning-of-buffer               "M-<" "<"                     :exitonly
     scroll-up-command                 "C-v"                         :exitonly)

    ("scroll-otherwin"
     scroll-other-window               "C-M-v" "v" "ESC M-v"
     beginning-of-buffer-other-window  "M-<home>" "<"
     end-of-buffer-other-window        "M-<end>" ">"                 :exitonly
     scroll-other-window-down          "C-M-S-v" "M-v" "ESC M-V" "V" :exitonly)

    ("scroll-otherwin-down"
     scroll-other-window-down          "C-M-S-v" "M-v" "v" "ESC M-V" "V"
     end-of-buffer-other-window        "M-<end>" ">"
     beginning-of-buffer-other-window  "M-<home>" "<"                :exitonly
     scroll-other-window               "C-M-v" "C-v" "ESC M-v"       :exitonly)

    ("scroll-sideways"
     scroll-left                       "C-x <" "<"
     scroll-right                      "C-x >" ">")

    ("hippie-exp"
     ;; For navigating through expansion candidates. You can revert
     ;; to the original string by prefixing the next hippie-expand
     ;; invocation with universal-argument (“C-u /”).
     hippie-expand                     "M-/" "/")

    ("search-nav"
     isearch-repeat-forward            "C-s" "s" "C-M-s" "ESC M-s"
     isearch-repeat-backward           "C-r" "r" "C-M-r" "ESC M-r"
     isearch-exit                      "<enter>" "<return>" "RET"    :exitonly)

    ("undo-only-redo"
     undo-only                         "C-x u" "C-_" "_" "C-/" "/"
     undo-redo                         "C-?" "?" "r")

    ;; Repeat Maps for Org-Mode
    ("org-nav"
     org-backward-heading-same-level   "C-c C-b" "C-b" "b"
     org-forward-heading-same-level    "C-c C-f" "C-f" "f"
     org-previous-visible-heading      "C-c C-p" "C-p" "p"
     org-next-visible-heading          "C-c C-n" "C-n" "n"
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
     backward-word                     "M-b" "b"
     forward-word                      "M-f" "f")

    ("set-mark"
     smart-region                      "C-SPC" "SPC"))

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
