;;
;; --- ORG MODE ---
;;

(use-package org
  :defer
  :hook
  (org-mode . (lambda ()
		(org-indent-mode)
		(variable-pitch-mode) ; varible font
		;; (auto-fill-mode) ; line breaks
		(visual-line-mode)
		(setq-local completion-styles '(emacs21 flex))
		(setq-local corfu-auto-prefix 2)
		(org-cdlatex-mode)))

  (org-mode . (lambda () (interactive)
                (setq prettify-symbols-alist '(("[#A]" . " ")
                                               ("[#B]" . " ")
                                               ("[#C]" . " ")
                                               ("[ ]" . " ")
                                               ("[X]" . " ")
                                               ("[-]" . " ")
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
                                               ("#+end_defn" . "―")))
                (prettify-symbols-mode)))

  :bind
  (("C-c o a" . org-agenda-list)
   ("C-c o A" . org-agenda)
   ("C-c o g" . consult-org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o r" . org-refile)
   :map org-mode-map
   ("C-o" . org-meta-return)
   ("C-c C-h" . consult-org-heading)
   ("C-x r N" . cory/rectangle-number-lines)
   ([(control return)] . crux-smart-open-line)
   ([(control shift return)] . crux-smart-open-line-above)
   ([(meta return)] . org-insert-heading-respect-content)
   ("C-x C-e" . org-babel-execute-src-block))

  :custom
  (org-ellipsis " ▼")
  (org-hide-emphasis-markers t)
  (org-agenda-files '("~/Code/Org/Tasks.org"
		      "~/Code/Org/School.org"
		      "~/Code/Org/Homework.org"))
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-on-weekday nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-pretty-entities t)
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-return-follows-link t)
  (org-insert-heading-respect-content t)
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
  (org-agenda-current-time-string "← now ----------")
  (org-agenda-timegrid-use-ampm 1) ;; 12-hour clock
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (org-refile-targets
   '(("Archive.org" :maxlevel . 1)
     ("Tasks.org" :maxlevel . 1)))
  (org-tag-alist
   '((:startgroup)
     ;; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)))

  ;; Configure custom agenda views
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
	     ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

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
       (todo "CANC"
	     ((org-agenda-overriding-header "Cancelled Projects")
	      (org-agenda-files org-agenda-files)))))))

  (org-capture-templates
   `(("t" "Tasks / Projects")
     ("tt" "Task" entry (file+olp "~/Code/Org/Tasks.org" "Inbox")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

     ("j" "Journal Entries")
     ("jj" "Journal" entry
      (file+olp+datetree "~/Code/Org/Journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
      :clock-in :clock-resume
      :empty-lines 1)
     ("jm" "Meeting" entry
      (file+olp+datetree "~/Code/Org/Journal.org")
      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)

     ("w" "Workflows")
     ("we" "Checking Email" entry (file+olp+datetree "~/Code/Org/Journal.org")
      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

     ("m" "Metrics Capture")
     ("mw" "Weight" table-line (file+headline "~/Code/Org/Metrics.org" "Weight")
      "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  :config
  (dolist (face '((org-level-1 . 1.75)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.25)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face)))
  (set-face-attribute 'org-document-title nil :height 2.0 :underline nil)

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Drag and drop
(use-package org-download
  :commands (org-mode org-download-clipboard)
  :custom
  (org-download-screenshot-method "sleep 2 && flameshot gui -s --raw > %s")
  :bind ("C-c C-o s" . org-download-screenshot))

;; Org roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Code/Org/Roam")
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
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(setq flyspell-mode-map
      (let ((map (make-sparse-keymap)))
	;; (if flyspell-use-meta-tab
	;;     (define-key map "\M-\t" 'flyspell-auto-correct-word))
	;; (define-key map flyspell-auto-correct-binding 'flyspell-auto-correct-previous-word)
	;; (define-key map [(control ?\,)] 'flyspell-goto-next-error)
	;; (define-key map [(control ?\.)] 'flyspell-auto-correct-word)
	(define-key map [?\C-c ?$] 'flyspell-correct-word-before-point)
	map))

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
