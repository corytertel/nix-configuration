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
  '(("navigation"
     backward-char                     "C-d" "d" "<left>"
     forward-char                      "C-n" "n" "<right>"
     next-line                         "<C-h>" "h" "<down>"
     previous-line                     "C-t" "t" "<up>"
     crux-move-beginning-of-line       "C-a" "a" "<home>"
     end-of-visual-line                "C-e" "e" "<end>"
     exchange-point-and-mark           "C-x C-x"
     cory/scroll-down                  "C-p" "p"                     :exitonly
     cory/scroll-up                    "M-p"                         :exitonly
     cory/beginning-of-workspace       "M-<"                         :exitonly
     cory/end-of-workspace             "M->"                         :exitonly
     cory/avy                          "C-w" "w")

    ("rectangle-nav"
     rectangle-mark-mode               "C-x SPC"
     rectangle-previous-line           "C-t" "t" "<up>"
     rectangle-next-line               "<C-h>" "h" "<down>"
     rectangle-backward-char           "C-d" "d" "<left>"
     rectangle-forward-char            "C-n" "n" "<right>"
     open-rectangle                    "C-o" "o"
     string-rectangle                  "C-b" "b"
     rectangle-exchange-point-and-mark "C-x C-x")

    ("defun-nav"
     beginning-of-defun                "C-M-a" "a"
     end-of-defun                      "C-M-e" "e"
     mark-defun                        "C-M-i" "i")

    ("c-defun-nav"
     c-beginning-of-defun              "C-M-a" "a"
     c-end-of-defun                    "C-M-e" "e"
     c-mark-function                   "C-M-i" "i")

    ("del-char"
     delete-char                       "C-y" "y" "<deletechar>")

    ("sexp-nav"
     backward-sexp                     "C-M-d" "d" "ESC M-d"
     forward-sexp                      "C-M-n" "n" "ESC M-n")

    ("paragraph-nav"
     backward-paragraph                "M-t" "t" "C-M-a" "a" "M-{" "M-[" "{" "["
     forward-paragraph                 "M-h" "h" "C-M-e" "e" "M-}" "M-]" "}" "]")

    ("sentence-nav"
     backward-sentence                 "M-a" "a"
     forward-sentence                  "M-e" "e")

    ("org-sentence-nav"
     org-backward-sentence             "M-a" "a"
     org-forward-sentence              "M-e" "e")

    ("page-nav"
     backward-page                     "C-x [" "["
     forward-page                      "C-x ]" "]")

    ("list-nav"
     backward-list                     "C-M-t" "t"
     forward-list                      "C-M-h" "h"
     backward-up-list                  "C-M-u" "u"
     down-list                         "C-M-y" "y")

    ("mid-top-bottom-move"
     recenter-top-bottom               "C-l" "l"
     move-to-window-line-top-bottom    "M-r" "r")

    ("fix-case"
     upcase-word                       "M-u" "u"

     ;; Easy way to manually set title case
     downcase-word                     "M-l" "l"
     capitalize-word                   "M-c" "c")

    ("kill-word"
     kill-word                         "M-y" "C-<delete>" "y")

    ("kill-line"
     kill-line                         "C-k" "k" "<deleteline>")

    ("kill-visual-line"
     kill-visual-line                  "C-k" "k" "<deleteline>")

    ("org-kill-line"
     org-kill-line                     "C-k" "k" "<deleteline>")

    ("sp-kill-line"
     sp-kill-hybrid-sexp               "C-k" "k" "<deleteline>")

    ("kill-whole-line"
     crux-kill-whole-line              "C-M-k" "k" "C-S-<backspace>" "<backspace>" "S-<delete>" "<delete>")

    ("sp-kill-whole-line"
     sp-kill-whole-line                "C-M-k" "k" "C-S-<backspace>" "<backspace>" "S-<delete>" "<delete>")

    ("kill-sentence"
     kill-sentence                     "M-k" "k"
     backward-kill-sentence            "C-x DEL" "DEL")

    ("kill-sexp"
     kill-sexp                         "C-M-k" "k" "ESC M-k")

    ("backward-kill-word"
     backward-kill-word                "C-<backspace>" "<backspace>" "M-DEL" "DEL")

    ("smartparens-backward-kill-word"
     sp-backward-kill-word             "C-<backspace>" "<backspace>" "M-DEL" "DEL")

    ;; Yank same text repeatedly with “C-v v v v”...
    ("yank-only"
     yank                              "C-v" "v"
     yank-pop                          "M-v" "e"                     :exitonly)

    ;; Cycle through the kill-ring with “C-v h h h h”...
    ;; You can reverse direction too “C-v h h C-- h h”
    ("yank-popping"
     yank-pop                          "M-v" "v" "e")

    ("kmacro-cycle"
     kmacro-cycle-ring-next            "C-x C-k <C-h>" "<C-h>" "h"
     kmacro-cycle-ring-previous        "C-x C-k C-t" "C-t" "t")

    ("tab-bar-nav"
     tab-next                          "C-x t o" "o" "h"
     tab-previous                      "C-x t O" "O" "t")

    ("transpose-chars"
     transpose-chars                    "C-b" "b")

    ("transpose-words"
     transpose-words                   "M-b" "b")

    ("transpose-sexps"
     transpose-sexps                   "C-M-b" "b" "ESC M-b")

    ("transpose-lines"
     transpose-lines                   "C-x C-b" "b")

    ("scroll-down"
     cory/scroll-down                  "C-p" "p" "<next>"
     cory/beginning-of-workspace       "M-<"
     cory/end-of-workspace             "M->"                         :exitonly
     cory/scroll-up                    "M-p" "<prior>"               :exitonly
     ;; same as navigation
     backward-char                     "C-d" "d" "<left>"            :exitonly
     forward-char                      "C-n" "n" "<right>"           :exitonly
     next-line                         "<C-h>" "h" "<down>"            :exitonly
     previous-line                     "C-t" "t" "<up>"              :exitonly
     crux-move-beginning-of-line       "C-a" "a" "<home>"            :exitonly
     end-of-visual-line                "C-e" "e" "<end>"             :exitonly
     cory/mark-line                    "C-M-SPC"                     :exitonly
     exchange-point-and-mark           "C-x C-x"                     :exitonly
     cory/avy                          "C-w" "w"                     :exitonly)

    ("scroll-up"
     cory/scroll-up                    "M-p" "p" "<prior>"
     cory/end-of-workspace             "M->"
     cory/beginning-of-workspace       "M-<"                         :exitonly
     cory/scroll-down                  "C-p" "<next>"                :exitonly
     ;; same as navigation
     backward-char                     "C-d" "d" "<left>"            :exitonly
     forward-char                      "C-n" "n" "<right>"           :exitonly
     next-line                         "<C-h>" "h" "<down>"            :exitonly
     previous-line                     "C-t" "t" "<up>"              :exitonly
     crux-move-beginning-of-line       "C-a" "a" "<home>"            :exitonly
     end-of-visual-line                "C-e" "e" "<end>"             :exitonly
     cory/mark-line                    "C-M-SPC"                     :exitonly
     exchange-point-and-mark           "C-x C-x"                     :exitonly
     cory/avy                          "C-w" "w"                     :exitonly)

    ("search-nav"
     isearch-repeat-forward            "C-f" "f" "C-M-f" "ESC M-f"
     isearch-repeat-backward           "C-r" "r" "C-M-r" "ESC M-r"
     isearch-exit                      "<enter>" "<return>" "RET"    :exitonly)

    ("undo"
     undo-tree-undo                    "C-_" "_" "C-z" "z"
     undo-tree-redo                    "C-S-z" "Z")

    ;; Repeat Maps for Org-Mode
    ("org-nav"
     org-backward-heading-same-level   "C-c C-d" "C-d" "d"
     org-forward-heading-same-level    "C-c C-n" "C-n" "n"
     org-previous-visible-heading      "C-c C-t" "C-t" "t"
     org-next-visible-heading          "C-c <C-h>" "<C-h>" "h"
     outline-up-heading                "C-c C-u" "C-u" "u"
     org-next-block                    "C-c M-n" "M-n" "N"
     org-previous-block                "C-c M-d" "M-d" "D"
     org-goto                          "C-c C-g" "g"
     org-refile                        "C-c C-w" "r")

    ("org-editing"
     org-metadown                      "M-<down>" "<down>" "h"
     org-metaup                        "M-<up>" "<up>" "t"
     org-metaleft                      "M-<left>" "<left>" "d"
     org-metaright                     "M-<right>" "<right>" "n"
     org-shiftmetaleft                 "M-S-<left>" "S-<left>" "D"
     org-shiftmetaright                "M-S-<right>" "S-<right>" "N"
     org-demote-subtree                "C->" ">"
     org-promote-subtree               "C-<" "<")

    ("org-task"
     org-todo                          "C-c C-t" "C-t" "t"
     org-priority                      "C-c ," ","
     org-time-stamp                    "C-c ." ;; "."
     org-schedule                      "C-c C-s" "C-s" "s"
     org-deadline                      "C-c C-d" "C-d" "d")

    ("word-nav"
     backward-word                     "M-d" "d" "C-<left>"
     forward-word                      "M-n" "n" "C-<right>")

    ("set-mark"
     backward-char                     "C-d" "d" "<left>"            :exitonly
     forward-char                      "C-n" "n" "<right>"           :exitonly
     next-line                         "<C-h>" "h" "<down>"          :exitonly
     previous-line                     "C-t" "t" "<up>"              :exitonly
     crux-move-beginning-of-line       "C-a" "a" "<home>"            :exitonly
     end-of-visual-line                "C-e" "e" "<end>"             :exitonly
     smart-region                      "C-SPC" "SPC"
     cory/mark-line                    "C-M-SPC"
     exchange-point-and-mark           "C-x C-x"                     :exitonly
     cory/avy                          "C-w" "w"                     :exitonly)

    ;; ("mark-line"
    ;;  cory/mark-line                    "C-M-SPC" "SPC")

    ("mark-list"
     cory/mark-list                    "M-i" "i")

    ("web-mode-element-select"
     web-mode-element-select           "M-i" "i")

    ("repeat"
     repeat                            "C-'" "'")

    ("macrursors"
     macrursors-mark-next-instance-of     "C->" ">"
     macrursors-mark-previous-instance-of "C-<" "<")

    ("macrursors-select"
     macrursors-select                 "C-; SPC" "SPC")

    ;; ("vertico"
    ;;  vertico-previous                  "<C-h>" "h" "<down>" "<backtab>"
    ;;  vertico-next                      "C-t" "t" "<up>" "TAB"
    ;;  vertico-grid-scroll-down          "C-p" "p" "<next>"
    ;;  vertico-grid-scroll-up            "M-p" "<prior>")
    ("vertico"
     vertico-next                      "<C-h>" "h" "<down>" "TAB"
     vertico-previous                  "C-t" "t" "<up>" "<backtab>"
     vertico-grid-scroll-up            "C-p" "p" "<next>"
     vertico-grid-scroll-down          "M-p" "<prior>")

    ("smartparens-nav"
     sp-forward-sexp                   "C-M-n" "n"
     sp-backward-sexp                  "C-M-d" "d"
     sp-down-sexp                      "C-M-y" "y"
     sp-backward-down-sexp             "C-M-t" "t"
     sp-up-sexp                        "C-M-h" "h"
     sp-backward-up-sexp               "C-M-u" "u")

    ("smartparens-del-char"
     sp-delete-char                    "C-y" "y" "<deletechar>")

    ("org-del-char"
     org-delete-char                   "C-y" "y" "<deletechar>")

    ("smartparens-kill-word"
     sp-kill-word                      "M-y" "y" "C-<delete>")

    ("corfu"
     corfu-next                        "TAB" "<tab>" "h"
     corfu-previous                    "S-TAB" "<backtab>" "t")

    ("error-nav"
     next-error                        "M-g M-h" "M-g h" "h" "C-x `" "`"
     previous-error                    "M-g M-t" "M-g t" "t")

    ("flymake-error-nav"
     flymake-goto-next-error           "M-g M-h" "M-g h" "h" "C-x `" "`"
     flymake-goto-prev-error           "M-g M-t" "M-g t" "t")

    ;; ("jinx"
    ;;  jinx-next                         "M-g M-h" "M-g h" "h" "C-x `" "`"
    ;;  jinx-previous                     "M-g M-t" "M-g t" "t"
    ;;  jinx-correct                      "M-$" "$" :exitonly)

    ("smartparens-parens"
     sp-backward-slurp-sexp            "C-(" "("
     sp-forward-slurp-sexp             "C-)" ")")

    ("kill-this-buffer"
     kill-this-buffer                  "C-<f4>" "<f4>" "C-x k" "k")

    ("join-line"
     join-line                         "M-j" "j")

    ("cory/join-next-line"
     cory/join-next-line               "C-M-j" "j"))

  "List of lists containing repeater-map definitions.
This must be in the form required by the
‘repeaters-define-maps’ function.")

(repeaters-define-maps repeaters-maps)
(setq repeat-exit-key "g"
      repeat-exit-timeout 30
      repeat-echo-function (lambda (keymap)
			     ;; (repeat-echo-message keymap)
			     (cond
			      (keymap
			       (set-face-attribute 'mode-line nil
						   :foreground "#141404"
						   :background "#ffdac0")
			       (set-face-attribute 'mode-line-inactive nil
						   :foreground "#141404"
						   :background "#ffffff"))
			      (t
			       ;; (set-face-attribute 'mode-line nil
			       ;; 			   :foreground "#141404"
			       ;; 			   :background "#c0daff")
			       (set-face-attribute 'mode-line nil
						   :foreground "#141404"
						   :background "#ffffff")
			       (set-face-attribute 'mode-line-inactive nil
						   :foreground "#141404"
						   :background "#ffffff")))))

;; ;; Disable the built-in repeat-mode hinting
;; (setq repeat-echo-function #'ignore)

;; (defun repeat-help--embark-indicate ()
;;   (if-let ((cmd (or this-command real-this-command))
;;            (keymap (or repeat-map
;;                       (repeat--command-property 'repeat-map))))
;;       (run-at-time
;;        0 nil
;;        (lambda ()
;;          (let* ((bufname "*Repeat Commands*")
;;                 (embark-verbose-indicator-buffer-sections
;;                  '(bindings))
;;                 (embark--verbose-indicator-buffer bufname)
;;                 (embark-verbose-indicator-display-action
;;                  '(display-buffer-at-bottom
;;                    (window-height . fit-window-to-buffer)
;;                    (window-parameters . ((no-other-window . t)
;;                                          (mode-line-format))))))
;;            (funcall
;;             (embark-verbose-indicator)
;;             (symbol-value keymap))
;;            (setq other-window-scroll-buffer (get-buffer bufname)))))
;;     (when-let ((win
;;                 (get-buffer-window
;;                  "*Repeat Commands*" 'visible)))
;;       (kill-buffer (window-buffer win))
;;       (delete-window win))))

;; (advice-add 'repeat-post-hook :after #'repeat-help--embark-indicate)

(repeat-mode t)
