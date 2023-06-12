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
     previous-buffer                   "C-x C-<left>" "C-x <left>" "C-<left>" "<left>" "k"
     next-buffer                       "C-x C-<right>" "C-x <right>" "C-<right>" "<right>" "j")

    ("calendar-nav"
     calendar-forward-day              "C-l" "l"
     calendar-backward-day             "C-h" "h"
     calendar-forward-week             "C-j" "j"
     calendar-backward-week            "C-k" "k"
     calendar-forward-month            "M-}" "}" "]"
     calendar-backward-month           "M-{" "{" "["
     calendar-forward-year             "C-x ]"
     calendar-backward-year            "C-x [")

    ("navigation"
     backward-char                     "C-h" "h" "<left>"
     forward-char                      "C-l" "l" "<right>"
     next-line                         "C-j" "j" "<down>"
     previous-line                     "C-k" "k" "<up>"
     crux-move-beginning-of-line       "C-a" "a" "<home>"
     end-of-visual-line                "C-e" "e" "<end>"
     smart-region                      "C-SPC"
     cory/mark-line                    "C-M-SPC"
     exchange-point-and-mark           "C-x C-x" "q"
     cory/scroll-down                  "C-n" "n"                     :exitonly
     cory/beginning-of-workspace       "M-<" "<"                     :exitonly
     cory/end-of-workspace             "M->" ">"                     :exitonly
     cory/scroll-up                    "M-n"                         :exitonly)

    ("rectangle-nav"
     rectangle-mark-mode               "C-x SPC"
     rectangle-previous-line           "C-k" "k" "<up>"
     rectangle-next-line               "C-j" "j" "<down>"
     rectangle-backward-char           "C-h" "h" "<left>"
     rectangle-forward-char            "C-l" "l" "<right>"
     open-rectangle                    "C-o" "o"
     string-rectangle                  "C-t" "t"
     rectangle-exchange-point-and-mark "C-x C-x" "q")

    ("defun-nav"
     beginning-of-defun                "C-M-a" "M-a" "a" "ESC M-a"
     end-of-defun                      "C-M-e" "M-e" "e" "ESC M-e")

    ("del-char"
     delete-char                       "C-d" "d" "<deletechar>")

    ("sexp-nav"
     backward-sexp                     "C-M-h" "h" "ESC M-h"
     forward-sexp                      "C-M-l" "l" "ESC M-l")

    ("paragraph-nav"
     backward-paragraph                "C-<up>" "<up>" "M-{" "M-[" "{" "["
     forward-paragraph                 "C-<down>" "<down>" "M-}" "M-]" "}" "]")

    ("sentence-nav"
     backward-sentence                 "M-a" "a"
     forward-sentence                  "M-e" "e"
     back-to-indentation               "M-m" "m"                     :exitonly)

    ("page-nav"
     backward-page                     "C-x [" "["
     forward-page                      "C-x ]" "]")

    ("list-nav"
     backward-list                     "C-M-k" "k" "ESC M-k"
     forward-list                      "C-M-j" "j" "ESC M-j"
     backward-up-list                  "C-M-<up>" "C-M-u" "<up>" "u" "ESC M-u"
     down-list                         "C-M-<down>" "C-M-d" "<down>" "d" "ESC M-d")

    ("error-nav"
     next-error                        "C-x `" "`" "M-g M-j" "M-g j" "j"
     previous-error                    "M-g M-k" "M-k" "k")

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
     kill-line                         "C-y" "y")

    ("kill-sentence"
     kill-sentence                     "M-y" "y"
     backward-kill-sentence            "C-x DEL" "DEL")

    ("kill-sexp"
     kill-sexp                         "C-M-y" "y" "ESC M-y")

    ;; Yank same text repeatedly with “C-v v v v”...
    ("yank-only"
     yank                              "C-v" "v"
     yank-pop                          "M-v" "j"                     :exitonly)

    ;; Cycle through the kill-ring with “C-v j j j”...
    ;; You can reverse direction too “C-v j j C-- j j”
    ("yank-popping"
     yank-pop                          "M-v" "v" "j")

    ("kmacro-cycle"
     kmacro-cycle-ring-next            "C-x C-k C-j" "C-j" "j"
     kmacro-cycle-ring-previous        "C-x C-k C-k" "C-k" "k")

    ("tab-bar-nav"
     tab-next                          "C-x t o" "o" "j"
     tab-previous                      "C-x t O" "O" "k")

    ("transpose-chars"
     transpose-chars                    "C-t" "t")

    ("transpose-words"
     transpose-words                   "M-t" "t")

    ("transpose-sexps"
     transpose-sexps                   "C-M-t" "t" "ESC M-t")

    ("transpose-lines"
     transpose-lines                   "C-x C-t" "t")

    ("scroll-down"
     cory/scroll-down                  "C-n" "n"
     cory/beginning-of-workspace       "M-<" "<"
     cory/end-of-workspace             "M->" ">"                     :exitonly
     cory/scroll-up                    "M-n"                         :exitonly
     ;; same as navigation
     backward-char                     "C-h" "h" "<left>"            :exitonly
     forward-char                      "C-l" "l" "<right>"           :exitonly
     next-line                         "C-j" "j" "<down>"            :exitonly
     previous-line                     "C-k" "k" "<up>"            :exitonly
     crux-move-beginning-of-line       "C-a" "a" "<home>"            :exitonly
     end-of-visual-line                "C-e" "e" "<end>"             :exitonly
     smart-region                      "C-SPC"                       :exitonly
     cory/mark-line                    "C-M-SPC"                     :exitonly
     exchange-point-and-mark           "C-x C-x" "q"                 :exitonly)

    ("scroll-up"
     cory/scroll-up                    "M-n" "n"
     cory/end-of-workspace             "M->" ">"
     cory/beginning-of-workspace       "M-<" "<"                     :exitonly
     cory/scroll-down                  "C-n"                         :exitonly
     ;; same as navigation
     backward-char                     "C-h" "h" "<left>"            :exitonly
     forward-char                      "C-l" "l" "<right>"           :exitonly
     next-line                         "C-j" "j" "<down>"            :exitonly
     previous-line                     "C-k" "k" "<up>"            :exitonly
     crux-move-beginning-of-line       "C-a" "a" "<home>"            :exitonly
     end-of-visual-line                "C-e" "e" "<end>"             :exitonly
     smart-region                      "C-SPC"                       :exitonly
     cory/mark-line                    "C-M-SPC"                     :exitonly
     exchange-point-and-mark           "C-x C-x" "q"                 :exitonly)

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

    ("search-nav"
     isearch-repeat-forward            "C-f" "f" "C-M-f" "ESC M-f"
     isearch-repeat-backward           "C-r" "r" "C-M-r" "ESC M-r"
     isearch-exit                      "<enter>" "<return>" "RET"    :exitonly)

    ("undo"
     undo-tree-undo                    "C-_" "_" "C-z" "z"
     undo-tree-redo                    "C-S-z" "Z")

    ;; Repeat Maps for Org-Mode
    ("org-nav"
     org-backward-heading-same-level   "C-c C-h" "C-h" "h"
     org-forward-heading-same-level    "C-c C-l" "C-l" "l"
     org-previous-visible-heading      "C-c C-k" "C-k" "k"
     org-next-visible-heading          "C-c C-j" "C-j" "j"
     outline-up-heading                "C-c C-u" "C-u" "u"
     org-next-block                    "C-c M-l" "M-l" "L"
     org-previous-block                "C-c M-h" "M-h" "H"
     org-goto                          "C-c C-g" "g"
     org-refile                        "C-c C-w" "r")

    ("org-editing"
     org-metadown                      "M-<down>" "<down>" "j"
     org-metaup                        "M-<up>" "<up>" "k"
     org-metaleft                      "M-<left>" "<left>" "h"
     org-metaright                     "M-<right>" "<right>" "l"
     org-shiftmetaleft                 "M-S-<left>" "S-<left>" "H"
     org-shiftmetaright                "M-S-<right>" "S-<right>" "L"
     org-demote-subtree                "C->" ">"
     org-promote-subtree               "C-<" "<")

    ("org-task"
     org-todo                          "C-c C-t" "C-t" "t"
     org-priority                      "C-c ," ","
     org-time-stamp                    "C-c ." "."
     org-schedule                      "C-c C-s" "C-s" "s"
     org-deadline                      "C-c C-d" "C-d" "d")

    ("word-nav"
     backward-word                     "M-h" "h"
     forward-word                      "M-l" "l")

    ;; ("set-mark"
    ;;  smart-region                      "C-SPC" "SPC")

    ;; ("mark-line"
    ;;  cory/mark-line                    "C-M-SPC" "SPC")

    ("repeat"
     repeat                            "C-'" "'")

    ("macrursors"
     macrursors-mark-next-instance-of     "C->" ">" "."
     macrursors-mark-previous-instance-of "C-<" "<" ",")

    ("macrursors-select"
     macrursors-select                 "C-; SPC" "C-c SPC" "SPC")

    ("vertico"
     vertico-previous                  "C-j" "j" "<down>" "<backtab>"
     vertico-next                      "C-k" "k" "<up>" "TAB"
     vertico-grid-scroll-down          "C-n" "n" "<next>"
     vertico-grid-scroll-up            "M-n" "<prior>")

    ("smartparens-nav"
     sp-forward-sexp                   "C-M-l" "l"
     sp-backward-sexp                  "C-M-h" "h"
     sp-down-sexp                      "C-M-d" "d"
     sp-backward-down-sexp             "C-M-k" "k"
     sp-up-sexp                        "C-M-j" "j"
     sp-backward-up-sexp               "C-M-u" "u")

    ("smartparens-del-char"
     sp-delete-char                    "C-d" "d" "<deletechar>")

    ("smartparens-kill-word"
     sp-kill-word                      "M-d" "d" "C-<delete>")

    ("corfu"
     corfu-next                        "TAB" "<tab>" "j"
     corfu-previous                    "S-TAB" "<backtab>" "k"))

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
			       (set-face-attribute 'mode-line nil
						   :foreground "#141404"
						   :background "#c0daff")
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
