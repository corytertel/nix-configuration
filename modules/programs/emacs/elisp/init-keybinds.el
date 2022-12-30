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

;; Visual Keybinding Info
(use-package which-key
  :disabled t
  :init
  (which-key-mode)
  :diminish which-key-mode
  :custom
  ;; (which-key-idle-delay 0.00000001)
  (which-key-idle-delay 1.0))

;; Better help information
(use-package helpful
  :ensure
  :bind
  ([remap describe-command]  . helpful-command)
  ([remap describe-key]      . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

;; Hydras
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "Scales text."
  ("n" text-scale-increase "in")
  ("p" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(defhydra hydra-window-resize (:timeout 4)
  "Resizes window."
  ("p" shrink-window 5 "shrink vertically")
  ("n" enlarge-window 5 "enlarge vertically")
  ("b" shrink-window-horizontally 5 "shrink horizontally")
  ("f" enlarge-window-horizontally 5 "enlarge horizontally")
  ("q" nil "finished" :exit t))

;;
;; --- GENERAL KEYBINDS ---
;;

;;; Scroll functions

(defun cory/scroll-down-half-page ()
  "Scroll down half a page while keeping the cursor centered."
  (interactive)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
	  ((= ln lmax) (recenter (window-end)))
	  (t (progn
               (move-to-window-line -1)
               (recenter))))))

(defun cory/scroll-up-half-page ()
  "Scroll up half a page while keeping the cursor centered."
  (interactive)
  (previous-line)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
	  ((= ln lmax) (move-to-window-line nil))
	  (t (progn
               (move-to-window-line 0)
               (recenter))))))

(put 'cory/scroll-down-half-page 'scroll-command t)
(put 'cory/scroll-up-half-page 'scroll-command t)

;;; Selection functions

(defmacro cory/def-bounds-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (car bounds))
	 (set-mark-command nil)
	 (goto-char (cdr bounds))))))

(defmacro cory/def-beginning-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (car bounds))))))

(defmacro cory/def-end-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (cdr bounds))))))

(cory/def-bounds-of-thing cory/mark-word 'word)
(cory/def-bounds-of-thing cory/mark-list 'list)
(cory/def-bounds-of-thing cory/mark-symbol 'symbol)
(cory/def-bounds-of-thing cory/mark-sexp 'sexp)
(cory/def-bounds-of-thing cory/mark-number 'number)
(cory/def-bounds-of-thing cory/mark-sentence 'sentence)
(cory/def-bounds-of-thing cory/mark-url 'url)
(cory/def-bounds-of-thing cory/mark-email 'email)
(cory/def-bounds-of-thing cory/mark-line 'line)

(cory/def-beginning-of-thing cory/beginning-of-word 'word)
(cory/def-beginning-of-thing cory/beginning-of-list 'list)
(cory/def-beginning-of-thing cory/beginning-of-symbol 'symbol)
(cory/def-beginning-of-thing cory/beginning-of-sexp 'sexp)
(cory/def-beginning-of-thing cory/beginning-of-number 'number)
(cory/def-beginning-of-thing cory/beginning-of-sentence 'sentence)
(cory/def-beginning-of-thing cory/beginning-of-url 'url)
(cory/def-beginning-of-thing cory/beginning-of-email 'email)
(cory/def-beginning-of-thing cory/beginning-of-line 'line)

(cory/def-end-of-thing cory/end-of-word 'word)
(cory/def-end-of-thing cory/end-of-list 'list)
(cory/def-end-of-thing cory/end-of-symbol 'symbol)
(cory/def-end-of-thing cory/end-of-sexp 'sexp)
(cory/def-end-of-thing cory/end-of-number 'number)
(cory/def-end-of-thing cory/end-of-sentence 'sentence)
(cory/def-end-of-thing cory/end-of-url 'url)
(cory/def-end-of-thing cory/end-of-email 'email)
(cory/def-end-of-thing cory/end-of-line 'line)

(defun cory/beginning-of-workspace ()
  "If a secondary selection is active, goto the beginning of it.
Else, goto the beginning of the buffer."
  (interactive)
  (if (and
      (secondary-selection-exist-p)
      (< (overlay-start mouse-secondary-overlay)
	 (overlay-end mouse-secondary-overlay))
      (<= (overlay-start mouse-secondary-overlay)
	 (point)
	 (overlay-end mouse-secondary-overlay)))
      (goto-char (overlay-start mouse-secondary-overlay))
    (goto-char (point-min))))

(defun cory/end-of-workspace ()
  "If a secondary selection is active, goto the end of it.
Else, goto the end of the buffer."
  (interactive)
  (if (and
      (secondary-selection-exist-p)
      (< (overlay-start mouse-secondary-overlay)
	 (overlay-end mouse-secondary-overlay))
      (<= (overlay-start mouse-secondary-overlay)
	 (point)
	 (overlay-end mouse-secondary-overlay)))
      (goto-char (- (overlay-end mouse-secondary-overlay) 1))
    (goto-char (point-max))))

;;; Grab functions

(defun cory/second-sel-set-string (string)
  (cond
   ((cory/second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (goto-char (overlay-start mouse-secondary-overlay))
      (delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
      (insert string)))
   ((markerp mouse-secondary-start)
    (with-current-buffer (marker-buffer mouse-secondary-start)
      (goto-char (marker-position mouse-secondary-start))
      (insert string)))))

(defun cory/second-sel-get-string ()
  (when (cory/second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (buffer-substring-no-properties
       (overlay-start mouse-secondary-overlay)
       (overlay-end mouse-secondary-overlay)))))

(defun cory/second-sel-buffer ()
  (and (overlayp mouse-secondary-overlay)
     (overlay-buffer mouse-secondary-overlay)))

(defun cory/grab ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (if (region-active-p)
      (secondary-selection-from-region)
    (progn
      (delete-overlay mouse-secondary-overlay)
      (setq mouse-secondary-start (make-marker))
      (move-marker mouse-secondary-start (point))))
  (deactivate-mark t))

(defun cory/swap-grab ()
  "Swap region and secondary selection."
  (interactive)
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (region-str (when (region-active-p) (buffer-substring-no-properties rbeg rend)))
         (sel-str (cory/second-sel-get-string))
         (next-marker (make-marker)))
    (when region-str (delete-region rbeg rend))
    (when sel-str (insert sel-str))
    (move-marker next-marker (point))
    (cory/second-sel-set-string (or region-str ""))
    (when (overlayp mouse-secondary-overlay)
      (delete-overlay mouse-secondary-overlay))
    (setq mouse-secondary-start next-marker)
    (deactivate-mark t)))

(defun cory/sync-grab ()
  "Sync secondary selection with current region."
  (interactive)
  (when (region-active-p)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (region-str (buffer-substring-no-properties rbeg rend))
           (next-marker (make-marker)))
      (move-marker next-marker (point))
      (cory/second-sel-set-string region-str)
      (when (overlayp mouse-secondary-overlay)
	(delete-overlay mouse-secondary-overlay))
      (setq mouse-secondary-start next-marker)
      (deactivate-mark t))))

;;; Misc functions

(defun cory/create-tmp-file ()
  (interactive)
  (find-file (concat temporary-file-directory (read-string "New tmp file:"))))

(defun cory/insert-space ()
  "Insert a space."
  (interactive)
  (self-insert-command 1 ? )
  (backward-char))

;;; Basic Keybinds

;; Swap "C-h" and "C-x", so it's easier to type on Dvorak layout
;; (keyboard-translate (kbd "C-h") (kbd "C-x"))
;; (keyboard-translate (kbd "C-x") (kbd "C-h"))

(dolist (pair '(("C-x k"   kill-this-buffer)
		("C-x K"   kill-buffer)
		("C-c w"   woman)
		;; ("C-x u"   undo-only)
		;; ("C-/"     undo-only)
		;; ("C-x C-u" undo-redo)
		;; ("C-?"     undo-redo)
		("C-'"     repeat)
		("C-s"     cory/search-forward-dwim)
		("C-r"     cory/search-backward-dwim)
		("C-M-s"   cory/isearch-forward-resume)
		("C-M-r"   cory/isearch-backward-resume)
		("C-v"     cory/scroll-down-half-page)
		("M-v"     cory/scroll-up-half-page)
		("C-c F"   cory/create-tmp-file)
		("C-c e"   eww)
		("S-SPC"   cory/insert-space)
		("C-c q"   quit-window)
		("C-j"     join-line)
		("C-c x"   xref-find-references-and-replace)
		("M-<"     cory/beginning-of-workspace)
		("M->"     cory/end-of-workspace)))
  (global-set-key (kbd (car pair)) (cadr pair)))

;;; Selection Keybinds

(define-prefix-command 'bounds-of-thing-map)
(global-set-key (kbd "C-.") 'bounds-of-thing-map)
(define-key bounds-of-thing-map (kbd "w") #'cory/mark-word)
(define-key bounds-of-thing-map (kbd "l") #'cory/mark-list)
(define-key bounds-of-thing-map (kbd "s") #'cory/mark-symbol)
(define-key bounds-of-thing-map (kbd "e") #'cory/mark-sexp)
(define-key bounds-of-thing-map (kbd "f") #'mark-defun)
(define-key bounds-of-thing-map (kbd "n") #'cory/mark-number)
(define-key bounds-of-thing-map (kbd ".") #'cory/mark-sentence)
(define-key bounds-of-thing-map (kbd "u") #'cory/mark-url)
(define-key bounds-of-thing-map (kbd "m") #'cory/mark-email)
(define-key bounds-of-thing-map (kbd "r") #'cory/mark-line)
(define-key bounds-of-thing-map (kbd "b") #'mark-whole-buffer)
(define-key bounds-of-thing-map (kbd "p") #'mark-paragraph)

(define-prefix-command 'beginning-of-thing-map)
(global-set-key (kbd "C-<") 'beginning-of-thing-map)
(define-key beginning-of-thing-map (kbd "w") #'cory/beginning-of-word)
(define-key beginning-of-thing-map (kbd "l") #'cory/beginning-of-list)
(define-key beginning-of-thing-map (kbd "s") #'cory/beginning-of-symbol)
(define-key beginning-of-thing-map (kbd "e") #'cory/beginning-of-sexp)
(define-key beginning-of-thing-map (kbd "f") #'beginning-of-defun)
(define-key beginning-of-thing-map (kbd "n") #'cory/beginning-of-number)
(define-key beginning-of-thing-map (kbd ".") #'cory/beginning-of-sentence)
(define-key beginning-of-thing-map (kbd "u") #'cory/beginning-of-url)
(define-key beginning-of-thing-map (kbd "m") #'cory/beginning-of-email)
(define-key beginning-of-thing-map (kbd "r") #'beginning-of-line)
(define-key beginning-of-thing-map (kbd "b") #'beginning-of-buffer)
(define-key beginning-of-thing-map (kbd "p") #'backward-paragraph)

(define-prefix-command 'end-of-thing-map)
(global-set-key (kbd "C->") 'end-of-thing-map)
(define-key end-of-thing-map (kbd "w") #'cory/end-of-word)
(define-key end-of-thing-map (kbd "l") #'cory/end-of-list)
(define-key end-of-thing-map (kbd "s") #'cory/end-of-symbol)
(define-key end-of-thing-map (kbd "e") #'cory/end-of-sexp)
(define-key end-of-thing-map (kbd "f") #'end-of-defun)
(define-key end-of-thing-map (kbd "n") #'cory/end-of-number)
(define-key end-of-thing-map (kbd ".") #'cory/end-of-sentence)
(define-key end-of-thing-map (kbd "u") #'cory/end-of-url)
(define-key end-of-thing-map (kbd "m") #'cory/end-of-email)
(define-key end-of-thing-map (kbd "r") #'end-of-line)
(define-key end-of-thing-map (kbd "b") #'end-of-buffer)
(define-key end-of-thing-map (kbd "p") #'forward-paragraph)

;;; Grab Keybinds

(global-set-key (kbd "C-c SPC") #'cory/grab)
(global-set-key (kbd "C-c C-SPC") #'cory/swap-grab)
(global-set-key (kbd "C-c M-SPC") #'cory/sync-grab)

;;; Lisp Keybinds

;; FIXME
(dolist (map (list emacs-lisp-mode-map
		   lisp-mode-map lisp-data-mode-map
		   clojure-mode-map ;; cider-repl-mode-map
		   ;; racket-mode-map racket-repl-mode-map
		   ;; scheme-mode-map geiser-repl-mode-map
		   ))
  (define-key map (kbd "M-a") 'backward-list)
  (define-key map (kbd "M-e") 'forward-list)
  (define-key map (kbd "M-h") 'cory/mark-list))

;;; Easier macro handling
;; (use-package kmacro-x
;;   :ensure t
;;   :init (kmacro-x-atomic-undo-mode 1)
;;   :bind ("C-c k" . kmacro-x-mc-region))

;;; Delete selection mode
(delete-selection-mode 1)

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
