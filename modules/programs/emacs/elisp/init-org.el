(add-hook 'text-mode-hook
	  (lambda ()
	    (setq-local completion-at-point-functions
			(list (cape-super-capf
			       #'tags-completion-at-point-function
			       #'cape-dict)))))

(use-package org
  :defer
  :hook
  (org-mode . (lambda ()
		(org-indent-mode)
		(variable-pitch-mode) ; varible font
		;; (auto-fill-mode) ; line breaks
		(visual-line-mode)
		(setq-local completion-styles '(emacs21)
			    completion-at-point-functions
			    (list (cape-super-capf
				   #'pcomplete-completions-at-point
				   #'cape-dict)))
		(org-cdlatex-mode)
		(corfu-mode -1)))

  (org-mode . (lambda () (interactive)
                (setq prettify-symbols-alist '(("[#A]" . " ")
                                               ("[#B]" . " ")
                                               ("[#C]" . " ")
                                               ("[ ]" . "☐ ")
                                               ("[X]" . "☑ ")
                                               ("[-]" . "❍ ")
                                               ("#+begin_src" . " ")
                                               ("#+end_src" . "―")
                                               ("#+begin_collapsible" . " ")
                                               ("#+end_collapsible" . "―")
                                               ("#+begin_aside" . " ")
                                               ("#+end_aside" . "―")
                                               ("#+begin_quote" . " ")
                                               ("#+end_quote" . "―")
                                               ("#+begin_defn" .  " ")
                                               ("#+end_defn" . "―")
                                               ("#+begin_questionable" .  " ")
                                               ("#+end_questionable" . "―")
                                               ("#+begin_problem" .  " ")
                                               ("#+end_problem" . "―")
                                               ("#+EXCLUDE_TAGS:" . " ")
                                               (":PROPERTIES:" . "\n")
                                               (":END:" . "―")
                                               ("#+STARTUP:" . " ")
                                               ("#+TITLE: " . "")
                                               ("#+title: " . "")
                                               ("#+RESULTS:" . " ")
                                               ("#+NAME:" . " ")
                                               ("#+ROAM_TAGS:" . " ")
                                               ("#+FILETAGS:" . " ")
                                               ("#+HTML_HEAD:" . " ")
                                               ("#+SUBTITLE:" . " ")
                                               ("#+AUTHOR:" . " ")
                                               (":Effort:" . " ")
                                               ("SCHEDULED:" . " ")
                                               ("DEADLINE:" . " ")
                                               ("#+begin_defn" . " ")
                                               ("#+end_defn" . "―")
					       ;; ("|" . "│")
					       ))
                (prettify-symbols-mode)))

  :bind
  (;; ("C-c o a" . org-agenda-list)
   ;; ("C-c o A" . org-agenda)
   ;; ("C-c o g" . consult-org-agenda)
   ;; ("C-c o c" . org-capture)
   ;; ("C-c o r" . org-refile)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("C-c C-h" . consult-org-heading)
   ("C-x r N" . cory/rectangle-number-lines)
   ;; ([(control return)] . crux-smart-open-line)
   ;; ([(control shift return)] . crux-smart-open-line-above)
   ;; ([(meta return)] . org-insert-heading-respect-content)
   ("C-x C-e" . org-babel-execute-src-block)
   ("C-'" . nil)
   ;; Minimak binds
   ("C-j" . nil)
   ("C-c M-b" . nil)
   ("C-c M-j" . org-previous-block)
   ("C-c M-f" . nil)
   ("C-c M-l" . org-next-block)
   ("C-c M-i" . org-insert-last-stored-link)
   ("C-c C-b" . cory/org-insert-image)
   ("C-c C-j" . org-backward-heading-same-level)
   ("C-c C-f" . org-insert-link)
   ("C-c C-l" . org-forward-heading-same-level)
   ("C-c C-n" . org-export-dispatch)
   ("C-c C-e" . org-next-visible-heading)
   ("C-c C-p" . nil)
   ("C-c <C-i>" . org-previous-visible-heading)
   ("C-c C-g" . org-goto)
   ;; TODO more minimak binds
   ("C-M-i" . org-metaup)
   ("C-M-e" . org-metadown)
   ("C-M-l" . org-metaright)
   ("C-M-j" . org-metaleft)
   ("M-E" . org-shiftdown)
   ("M-I" . org-shiftup)
   ("M-L" . org-shiftright)
   ("M-J" . org-shiftleft))

  :init
  ;; Org-Emphasis-Regex settings. Set regex boundaries for emphasis.
  ;; Load this before org-mode is loaded.
  ;; See https://emacs.stackexchange.com/q/54673/11934
  ;; https://emacs.stackexchange.com/q/54632/11934
  (setq org-emphasis-regexp-components
        '("-—[:space:]('\"{["
          "\] - [:space:].,:!?;'\")}\\["
          "[:space:]"
          "."
          1))

  :custom
  ;; Aesthetics & UI
  (org-adapt-indentation 'headline-data) ;; adapt indentation only for data lines
  (org-catch-invisible-edits 'smart) ;; prevent editing invisible area
  (org-cycle-separator-lines 0) ;; no empty lines in collapsed view
  (org-ellipsis " ↷") ;; nicer elipses "…" "↴" "▼"
  (org-fontify-quote-and-verse-blocks t) ;; make quotes stand out
  (org-hide-emphasis-markers t)  ;; hide emph markers
  (org-hide-leading-stars t)  ;; hide leading stars
  (org-image-actual-width 500) ;; show all images at 500px using imagemagik
  (org-insert-heading-respect-content t) ;; insert new headings after subtree
  (org-list-allow-alphabetical t) ;; allow alphabetical list
  (org-pretty-entities t) ;; make latex look good, etc.
  (org-pretty-entities-include-sub-superscripts t) ;; prettify sub/superscripts
  (org-read-date-prefer-future 'time) ;; Incomplete dates refer to future dates & times
  ;; (org-startup-folded t) ;; start org in outline
  (org-startup-indented t) ;; start with indentation of headlines
  (org-auto-align-tags nil) ;; don't auto-align tags
  (org-tags-column 0) ;; place tags directly next to headline text
  (org-startup-with-inline-images t)
  (org-edit-src-content-indentation 0)

  ;; Footnotes
  (org-footnote-section nil) ;; place footnotes locally
  (org-footnote-auto-adjust t) ;; renumber footnotes

  ;; Insertion/Yanking
  (org-M-RET-may-split-line '((default . t)))  ;; don't split line when creating a new headline, list item, or table field
  (org-yank-adjusted-subtrees t)  ;; adjust subtrees to depth when yanked
  (org-yank-folded-subtrees t) ;; fold subtrees on yank

  ;; Lists
  ;; Demote sequence for list bullets
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-list-indent-offset 1) ;; increase sub-item indentation

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline nil) ;; don't log the time a task was rescheduled/redeadlined.
  (org-log-reschedule nil)

  ;; Movement
  (org-return-follows-link t) ;; make RET follow links
  (org-special-ctrl-a/e t)  ;; better movement in headers

  ;; Searching
  (org-imenu-depth 8) ;; scan to depth 8 w/imenu
  (imenu-auto-rescan t) ;; make sure imenu refreshes

  ;; Source block settings
  (org-src-fontify-natively t) ;; use lang-specific fontification
  (org-src-window-setup 'other-window) ;; edit source in other window
  (org-src-tab-acts-natively t) ;; use lang bindings
  (org-confirm-babel-evaluate t) ;; confirm evaluation

  ;; TODOS
  (org-use-fast-todo-selection 'expert) ;; don't use popup window for todos
  ;; don't set to DONE if children aren’t DONE
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(g)" "NEXT(n)" "WAITING(w@/!)" "MAYBE(m)" "SOMEDAY(s)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  ;; Export to PDF
  (org-startup-with-latex-preview t)
  (org-highlight-latex-and-related '(native))
  ;; (org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (org-latex-listings 'minted)
  (org-latex-packages-alist '(("" "minted")))
  (org-latex-tables-centered t)
  (org-latex-compiler "lualatex")
  (org-preview-latex-default-process 'dvisvgm)
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Export to docx
  ;; Don't use bad hyperref value
  ;; https://emacs.stackexchange.com/a/46226/11934
  (org-latex-hyperref-template nil)
  ;; Export settings
  (org-table-export-default-format "orgtbl-to-csv") ;; export for org-tables to csv
  (org-export-with-smart-quotes t)
  (org-export-with-broken-links t)
  (org-export-async-debug t)
  (org-html-postamble nil) ;; dont export postamble
  (org-export-async-init-file nil)
  (org-export-backends '(ascii beamer html icalendar latex odt pandoc hugo md))
  ;; org v8 bundled with Emacs 24.4
  (org-odt-preferred-output-format "docx")

  ;; Agenda
  (org-agenda-files '("~/Org/Tasks.org"
		      "~/Org/School.org"
		      "~/Org/Homework.org"))
  ;; (org-agenda-start-on-weekday nil) ;; Start on current day
  (org-agenda-start-on-weekday 0) ;; Start on Sunday
  (org-agenda-timegrid-use-ampm 1) ;; 12-hour clock

  ;; Agenda logging
  (org-agenda-start-with-log-mode t)

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ───── " "────────────────────────────────────────────────────────"))
  (org-agenda-current-time-string
   "⭠ now ──────────────────────────────────────────────────")

  ;; Display properties
  (org-agenda-tags-column org-tags-column)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)

  ;; from stack overflow https://stackoverflow.com/a/22900459/6277148
  ;; note that the formatting is nicer that just using '%b'
  (org-agenda-prefix-format
   '((agenda . " %-18c%?-10t ")
     (timeline . "  % s")
     (todo . " ")
     (tags . " ")
     (search . " %i %-12:c")))

  ;; Scheduling
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-sorting-strategy
   '((agenda time-up) (todo time-up) (tags time-up) (search time-up)))
  (calendar-week-start-day 0) ;; Start week on Sunday

  ;; Agenda Custom Commands
  ;; Configure custom agenda views
  ;; https://orgmode.org/manual/Storing-searches.html#Storing-searches
  ;; https://systemcrafters.cc/emacs-from-scratch/organize-your-life-with-org-mode/
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-agenda-span 'day)))
       (tags-todo "DEADLINE=\"<today>\""
                  ((org-agenda-overriding-header "Due Today!")))
       (tags-todo "+DEADLINE<\"<+5d>\"+DEADLINE>\"<today>\""
                  ((org-agenda-overriding-header "Due Soon")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "email" ((org-agenda-overriding-header "Email")))
       ))

     ("n" "Next Tasks"
      ((todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))))

     ("W" "Work Tasks" tags-todo "+work")

     ;; Low-effort next actions
     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files)))

     ("w" "Workflow Status"
      ((todo "WAIT"
             ((org-agenda-overriding-header "Waiting on External")
              (org-agenda-files org-agenda-files)))
       (todo "REVIEW"
             ((org-agenda-overriding-header "In Review")
              (org-agenda-files org-agenda-files)))
       (todo "PLAN"
             ((org-agenda-overriding-header "In Planning")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "BACKLOG"
             ((org-agenda-overriding-header "Project Backlog")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "READY"
             ((org-agenda-overriding-header "Ready for Work")
              (org-agenda-files org-agenda-files)))
       (todo "ACTIVE"
             ((org-agenda-overriding-header "Active Projects")
              (org-agenda-files org-agenda-files)))
       (todo "COMPLETED"
             ((org-agenda-overriding-header "Completed Projects")
              (org-agenda-files org-agenda-files)))
       (todo "CANCELLED"
             ((org-agenda-overriding-header "Cancelled Projects")
              (org-agenda-files org-agenda-files)))))))

  ;; Refile
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 8)))
  (org-refile-use-cache t)  ;; use cache for org refile
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Capture
  (org-capture-templates
   `(("t" "Tasks / Projects")
     ("tt" "Task" entry (file+olp "~/Org/Tasks.org" "Inbox")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

     ("j" "Journal Entries")
     ("jj" "Journal" entry
      (file+olp+datetree "~/Org/Journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
      :clock-in :clock-resume
      :empty-lines 1)
     ("jm" "Meeting" entry
      (file+olp+datetree "~/Org/Journal.org")
      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)

     ("w" "Workflows")
     ("we" "Checking Email" entry (file+olp+datetree "~/Org/Journal.org")
      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

     ("m" "Metrics Capture")
     ("mw" "Weight" table-line (file+headline "~/Org/Metrics.org" "Weight")
      "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  :config
  ;; (dolist (face '((org-level-1 . 1.75)
  ;;                 (org-level-2 . 1.5)
  ;;                 (org-level-3 . 1.25)
  ;;                 (org-level-4 . 1.1)
  ;;                 (org-level-5 . 1.0)
  ;;                 (org-level-6 . 1.0)
  ;;                 (org-level-7 . 1.0)
  ;;                 (org-level-8 . 1.0)))
  ;;   (set-face-attribute (car face) nil :height (cdr face) :weight 'light))
  ;; (set-face-attribute 'org-document-title nil :height 2.0 :underline nil)
  (set-face-attribute 'org-document-title nil :height 1.5 :underline t)
  (set-face-attribute 'org-level-1 nil :height 1.2 :underline nil :weight 'bold :foreground "blue4")
  (set-face-attribute 'org-level-2 nil :height 1.0 :weight 'bold :foreground "sienna4" :inherit nil)
  (set-face-attribute 'org-level-3 nil :height 1.0 :foreground "purple4" :inherit nil)
  (set-face-attribute 'org-level-4 nil :height 1.0 :foreground "red4" :inherit nil)
  (set-face-attribute 'org-level-5 nil :height 1.0 :foreground "dark green" :inherit nil)
  (set-face-attribute 'org-level-6 nil :height 1.0 :foreground "DeepSkyBlue4" :inherit nil)
  (set-face-attribute 'org-level-7 nil :height 1.0 :inherit nil)
  (set-face-attribute 'org-level-8 nil :height 1.0 :inherit nil)

  (set-face-attribute 'org-block nil :background "grey93" :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :background "grey93" :inherit '(fixed-pitch))
  (set-face-attribute 'org-table nil :foreground "black" :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-todo nil :weight 'unspecified)

  (set-face-attribute 'org-scheduled nil :foreground "black")
  (set-face-attribute 'org-scheduled-today nil :foreground "Blue1")
  (set-face-attribute 'org-scheduled-previously nil :foreground "Red1")
  (set-face-attribute 'org-upcoming-deadline nil :foreground "black")
  (set-face-attribute 'org-upcoming-distant-deadline nil :foreground "grey50")
  (set-face-attribute 'org-imminent-deadline nil :foreground "Red1" :weight 'normal)
  (set-face-attribute 'org-agenda-done nil :foreground "ForestGreen")

  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Display weekly agenda on startup
  (defun cory/display-weekly-agenda ()
    "Display the weekly agenda."
    (org-agenda-list 1)
    (delete-other-windows)
    (toggle-frame-fullscreen))

  (add-hook 'after-init-hook #'cory/display-weekly-agenda)

  ;; Org-babel languages
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((latex . t)
				 (emacs-lisp . t)
				 (scheme . t)
				 (lisp . t)
				 ;; (c++ . t)
				 (java . t)))

  (defun cory/rectangle-number-lines ()
    (interactive)
    (rectangle-number-lines (region-beginning) (region-end) 1 "%s. "))

  (defun cory/org-insert-heading-above-respect-content (&optional invisible-ok)
    "Insert heading with above current heading."
    (interactive)
    (move-beginning-of-line nil)
    (org-insert-heading nil invisible-ok))

  ;; Automatically tangle our Emacs.org config file when we save it
  ;; (defun cory/org-babel-tangle-config ()
  ;;   (when (string-equal (buffer-file-name)
  ;; 			(expand-file-name
  ;; 			 "~/.config/nix/modules/programs/emacs/config.org"))
  ;;     ;; Dynamic scoping to the rescue
  ;;     (let ((org-confirm-babel-evaluate nil))
  ;; 	(org-babel-tangle))))

  ;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook
  ;; 					   #'cory/org-babel-tangle-config)))

  ;; Minimak binds
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-keymap (kbd "C-z") #'org-agenda-undo)
    (define-key org-agenda-keymap (kbd "C-c C-n") nil)
    (define-key org-agenda-keymap (kbd "C-c C-p") nil)
    (define-key org-agenda-keymap (kbd "C-c C-e") #'org-agenda-next-date-line)
    (define-key org-agenda-keymap (kbd "C-c <C-i>") #'org-agenda-previous-date-line)
    (define-key org-agenda-keymap (kbd "C-s") #'org-save-all-org-buffers)
    (define-key org-agenda-keymap (kbd "L") #'org-agenda-clock-in)
    (define-key org-agenda-keymap (kbd "P") #'org-agenda-recenter)
    (define-key org-agenda-keymap (kbd "I") #'org-agenda-previous-item)
    (define-key org-agenda-keymap (kbd "N") #'org-agenda-entry-text-mode)
    (define-key org-agenda-keymap (kbd "E") #'org-agenda-next-item)
    (define-key org-agenda-keymap (kbd "n") #'org-agenda-set-effort)
    (define-key org-agenda-keymap (kbd "e") #'org-agenda-next-line)
    (define-key org-agenda-keymap (kbd "p") #'org-agenda-diary-entry)
    (define-key org-agenda-keymap (kbd "i") #'org-agenda-previous-line)
    (define-key org-agenda-keymap (kbd "b") #'org-agenda-goto-date)
    (define-key org-agenda-keymap (kbd "j") #'org-agenda-earlier)
    (define-key org-agenda-keymap (kbd "f") #'org-agenda-log-mode)
    (define-key org-agenda-keymap (kbd "l") #'org-agenda-later)
    (define-key org-agenda-mode-map (kbd "C-z") #'org-agenda-undo)
    (define-key org-agenda-mode-map (kbd "C-c C-n") nil)
    (define-key org-agenda-mode-map (kbd "C-c C-p") nil)
    (define-key org-agenda-mode-map (kbd "C-c C-e") #'org-agenda-next-date-line)
    (define-key org-agenda-mode-map (kbd "C-c <C-i>") #'org-agenda-previous-date-line)
    (define-key org-agenda-mode-map (kbd "C-s") #'org-save-all-org-buffers)
    (define-key org-agenda-mode-map (kbd "L") #'org-agenda-clock-in)
    (define-key org-agenda-mode-map (kbd "P") #'org-agenda-recenter)
    (define-key org-agenda-mode-map (kbd "I") #'org-agenda-previous-item)
    (define-key org-agenda-mode-map (kbd "N") #'org-agenda-entry-text-mode)
    (define-key org-agenda-mode-map (kbd "E") #'org-agenda-next-item)
    (define-key org-agenda-mode-map (kbd "n") #'org-agenda-set-effort)
    (define-key org-agenda-mode-map (kbd "e") #'org-agenda-next-line)
    (define-key org-agenda-mode-map (kbd "p") #'org-agenda-diary-entry)
    (define-key org-agenda-mode-map (kbd "i") #'org-agenda-previous-line)
    (define-key org-agenda-mode-map (kbd "b") #'org-agenda-goto-date)
    (define-key org-agenda-mode-map (kbd "j") #'org-agenda-earlier)
    (define-key org-agenda-mode-map (kbd "f") #'org-agenda-log-mode)
    (define-key org-agenda-mode-map (kbd "l") #'org-agenda-later)

    (set-face-attribute 'org-agenda-structure nil :height 1.75)
    (set-face-attribute 'org-agenda-date nil :height 1.2 :inherit 'unspecified)

    (add-hook 'org-agenda-mode-hook (lambda ()
				      (hl-line-mode 1)
				      (setq-local cursor-type nil))))

  ;; Org Src
  (with-eval-after-load 'org-src
    (define-key org-src-mode-map (kbd "C-s") #'org-edit-src-save))

  ;; Org Speed
  (defun cory/org-jump-to-heading-beginning ()
    "Jump to the beginning of the line of the closest Org heading."
    (interactive)
    (org-back-to-heading)
    (beginning-of-line))

  (define-key org-mode-map (kbd "&*") #'cory/org-jump-to-heading-beginning)
  ;; (define-key org-mode-map (kbd "^") #'org-sort)
  ;; (define-key org-mode-map (kbd "z") #'org-refile)
  ;; (define-key org-mode-map (kbd "@") #'org-mark-subtree)

  ;; Turn off org-speed
  ;; (setq org-use-speed-commands t)
  ;; (setq org-speed-commands
  ;; 	'(("w" . widen)
  ;; 	  ("Outline Navigation")
  ;; 	  ("e" org-speed-move-safe 'org-next-visible-heading)
  ;; 	  ("i" org-speed-move-safe 'org-previous-visible-heading)
  ;; 	  ("l" org-speed-move-safe 'org-forward-heading-same-level)
  ;; 	  ("j" org-speed-move-safe 'org-backward-heading-same-level)
  ;; 	  ("L" . org-next-block)
  ;; 	  ("J" . org-previous-block)
  ;; 	  ("u" org-speed-move-safe 'outline-up-heading)
  ;; 	  ("g" . org-goto)
  ;; 	  ("r" org-refile
  ;; 	   '(4))
  ;; 	  ("Outline Visibility")
  ;; 	  ("c" . org-cycle)
  ;; 	  ("C" . org-shifttab)
  ;; 	  (" " . org-display-outline-path)
  ;; 	  ("s" . org-toggle-narrow-to-subtree)
  ;; 	  ("k" . org-cut-subtree)
  ;; 	  ("=" . org-columns)
  ;; 	  ("Outline Structure Editing")
  ;; 	  ("U" . org-metaup)
  ;; 	  ("D" . org-metadown)
  ;; 	  ("y" . org-metaright)
  ;; 	  ("b" . org-metaleft)
  ;; 	  ("Y" . org-shiftmetaright)
  ;; 	  ("B" . org-shiftmetaleft)
  ;; 	  ("n" progn
  ;; 	   (forward-char 1)
  ;; 	   (call-interactively 'org-insert-heading-respect-content))
  ;; 	  ("^" . org-sort)
  ;; 	  ("w" . org-refile)
  ;; 	  ("a" . org-archive-subtree-default-with-confirmation)
  ;; 	  ("@" . org-mark-subtree)
  ;; 	  ("#" . org-toggle-comment)
  ;; 	  ("Clock Commands")
  ;; 	  ("I" . org-clock-in)
  ;; 	  ("O" . org-clock-out)
  ;; 	  ("Meta Data Editing")
  ;; 	  ("t" . org-todo)
  ;; 	  ("," org-priority)
  ;; 	  ("0" org-priority 32)
  ;; 	  ("1" org-priority 65)
  ;; 	  ("2" org-priority 66)
  ;; 	  ("3" org-priority 67)
  ;; 	  (":" . org-set-tags-command)
  ;; 	  ("f" . org-set-effort)
  ;; 	  ("F" . org-inc-effort)
  ;; 	  ("W" lambda
  ;; 	   (m)
  ;; 	   (interactive "Minutes before warning: ")
  ;; 	   (org-entry-put
  ;; 	    (point)
  ;; 	    "APPT_WARNTIME" m))
  ;; 	  ("Agenda Views etc")
  ;; 	  ("v" . org-agenda)
  ;; 	  ("/" . org-sparse-tree)
  ;; 	  ("Misc")
  ;; 	  ("o" . org-open-at-point)
  ;; 	  ("?" . org-speed-command-help)
  ;; 	  ("<" org-agenda-set-restriction-lock 'subtree)
  ;; 	  (">" org-agenda-remove-restriction-lock)))

  ;; For exporting
  (require 'ox)

  (defun cory/org-insert-image ()
    (interactive)
    (insert (concat "[[" (read-file-name "Image: ") "]]"))
    (org-display-inline-images)))

;;; Hydra for Agenda
;; Hydra for org agenda (graciously offered by Spacemacs)
(with-eval-after-load 'org-agenda
  (defhydra cory/hydra-org-agenda (:color pink :hint none)
    "
Org agenda (_q_uit)
^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
    ;; Entry
    ("hA" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("hr" org-agenda-refile)
    ("h:" org-agenda-set-tags)
    ("ht" org-agenda-todo)
    ;; Visit entry
    ("o"   link-hint-open-link :exit t)
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("SPC" org-agenda-show-and-scroll-up)
    ("RET" org-agenda-switch-to :exit t)
    ;; Date
    ("dt" org-agenda-date-prompt)
    ("dd" org-agenda-deadline)
    ("+" org-agenda-do-date-later)
    ("-" org-agenda-do-date-earlier)
    ("ds" org-agenda-schedule)
    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ;; Toggle mode
    ("ta" org-agenda-archives-mode)
    ("tA" (org-agenda-archives-mode 'files))
    ("tr" org-agenda-clockreport-mode)
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("td" org-agenda-toggle-diary)
    ;; Filter
    ("fc" org-agenda-filter-by-category)
    ("fx" org-agenda-filter-by-regexp)
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fh" org-agenda-filter-by-top-headline)
    ("fd" org-agenda-filter-remove-all)
    ;; Clock
    ("cq" org-agenda-clock-cancel)
    ("cj" org-agenda-clock-goto :exit t)
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ;; Other
    ("q" nil :exit t)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("gr" org-agenda-redo)))

(use-package org-bullets
  :disabled t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("
\u200b" "\u200b" "•" "-" "•" "–" "•" "–")))

;; show markup at point -- this should be part of org!
(use-package org-appear
  :commands (org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

;; A nicer set of default display options
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-hide-stars nil) ;; compatibility w/org-indent
  ;; don't use other faces
  (org-modern-priority nil)
  (org-modern-todo nil)
  (org-modern-tag t)
  ;; Customize this per your font
  (org-modern-label-border .25)
  ;; Note that these stars allow differentiation of levels
  ;; "①" "②" "③" "④" "⑤" "⑥" "⑦"
  ;; (org-modern-star ["\u200b"])
  (org-modern-star ["
\u200b" "\u200b" "•" "-" "•" "–" "•" "–"]))

;; Make org-modern work better with org-indent
;; (use-package org-modern-indent
;;   :hook (org-indent-mode . org-modern-indent-mode))

;; Drag and drop
(use-package org-download
  :commands (org-mode org-download-clipboard)
  :custom
  (org-download-screenshot-method "sleep 2 && flameshot gui -s --raw > %s")
  :bind
  (:map org-mode-map
   ("C-c C-p" . org-download-screenshot)))

;; Org roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Org/Roam")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head
	       "%<%Y%m%d%H%M%S>-${slug}.org"
	       ,(let ((options '("#+options: _:{}"
				 "#+options: ^:{}"
				 "#+startup: latexpreview"
				 "#+startup: entitiespretty"
				 "#+startup: inlineimages"
				 "#+title: ${title}"
				 "#+date: %U")))
		  (mapconcat 'identity options "\n")))
      :unnarrowed t)))
  :bind (("C-c o n l" . org-roam-buffer-toggle)
	 ("C-c o n f" . org-roam-node-find)
	 ("C-c o n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

;; Writing
(use-package writegood-mode
  :hook (flyspell-mode . writegood-mode)
  :bind (:map flyspell-mode-map
	 ("C-c C-g g" . writegood-grade-level)
	 ("C-c C-g e" . writegood-reading-ease)))

(use-package dictionary)

;; Spelling
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook sgml-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(with-eval-after-load 'flyspell
  (setq flyspell-mode-map
	(let ((map (make-sparse-keymap)))
	  ;; (if flyspell-use-meta-tab
	  ;;     (define-key map "\M-\t" 'flyspell-auto-correct-word))
	  ;; (define-key map flyspell-auto-correct-binding 'flyspell-auto-correct-previous-word)
	  ;; (define-key map [(control ?\,)] 'flyspell-goto-next-error)
	  ;; (define-key map [(control ?\.)] 'flyspell-auto-correct-word)
	  (define-key map [?\C-c ?$] 'flyspell-correct-word-before-point)
	  map))

  (define-key flyspell-mouse-map (kbd "<mouse-2>") nil)
  (define-key flyspell-mouse-map (kbd "<mouse-3>") #'flyspell-correct-word))

;; FIXME
(use-package flyspell-correct
  :after flyspell
  :init
  (add-to-list 'ispell-skip-region-alist '("+begin_src" . "+end_src"))
  (setq flyspell-use-meta-tab nil)
  :bind (:map flyspell-mode-map ("C-\"" . flyspell-correct-wrapper)))

(use-package frog-menu
  :custom
  ;; Need to redefine keys to account for custom keyboard layout
  (frog-menu-avy-keys (append (string-to-list "atenisubopyflmc")
			      (string-to-list (upcase "atenisubopyflmc"))
			      (number-sequence ?, ?@)))
  :config
  (defun frog-menu-flyspell-correct (candidates word)
    "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
    (let* ((corrects (if flyspell-sort-corrections
			 (sort candidates 'string<)
                       candidates))
           (actions `(("C-s" "Save word"         (save    . ,word))
                      ("C-a" "Accept (session)"  (session . ,word))
                      ("C-b" "Accept (buffer)"   (buffer  . ,word))
                      ("C-c" "Skip"              (skip    . ,word))))
           (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                     ispell-dictionary
                                                     "default")))
           (res      (frog-menu-read prompt corrects actions)))
      (unless res
	(error "Quit"))
      res))

  (setq flyspell-correct-interface #'frog-menu-flyspell-correct))

(use-package cdlatex
  :commands 'turn-on-cdlatex)

;; (use-package latex-preview-pane)
