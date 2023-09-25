(add-hook 'text-mode-hook
	  (lambda ()
	    (unless (equal major-mode 'org-mode)
	      (setq-local completion-at-point-functions
			  (list (cape-super-capf
				 #'tags-completion-at-point-function
				 #'cape-dict))))))

(use-package org
  :hook
  (org-mode . (lambda ()
		(org-indent-mode)
		(variable-pitch-mode) ; varible font
		(visual-line-mode)
		(setq-local completion-styles '(emacs21)
			    completion-at-point-functions (list #'cape-dict)
			    completion-cycle-threshold t
			    completion-ignore-case nil)
		;; (org-cdlatex-mode)
		(corfu-mode -1)))

  :bind
  (:map org-mode-map
   ("C-c M-h" . consult-org-heading)
   ("C-x r N" . cory/rectangle-number-lines)
   ;; ([(control return)] . crux-smart-open-line)
   ;; ([(control shift return)] . crux-smart-open-line-above)
   ;; ([(meta return)] . org-insert-heading-respect-content)
   ("C-x C-e" . org-babel-execute-src-block)
   ;; TODO fix org dictionary completion
   ("C-M-s" . completion-at-point)
   ("TAB" . completion-at-point)
   ("C-<tab>" . org-cycle)
   ("C-'" . nil)
   ("C-j" . nil)
   ("M-h" . nil)
   ("M-i" . org-mark-element)
   ("C-c M-b" . nil)
   ("C-c M-d" . org-previous-block)
   ("C-c M-f" . nil)
   ("C-c M-n" . org-next-block)
   ("C-c M-i" . org-insert-last-stored-link)
   ("C-c C-h" . cory/org-insert-image)
   ("C-c C-d" . org-backward-heading-same-level)
   ("C-c C-l" . org-insert-link)
   ("C-c C-n" . org-forward-heading-same-level)
   ("C-c C-e" . org-export-dispatch)
   ("C-c <C-h>" . org-next-visible-heading)
   ("C-c C-p" . nil)
   ("C-c C-t" . org-previous-visible-heading)
   ("C-c C-g" . org-goto)
   ("C-M-t" . org-metaup)
   ("C-M-h" . org-metadown)
   ("C-M-n" . org-metaright)
   ("C-M-d" . org-metaleft)
   ("M-H" . org-shiftdown)
   ("M-T" . org-shiftup)
   ("M-N" . org-shiftright)
   ("M-D" . org-shiftleft))

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

  ;; Lists
  ;; Demote sequence for list bullets
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-list-indent-offset 1) ;; increase sub-item indentation

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
  (org-highlight-latex-and-related '(native))
  (org-latex-tables-centered t)
  (org-latex-src-block-backend 'minted)
  (org-latex-packages-alist '(("newfloat" "minted")))
  (org-latex-compiler "lualatex")
  ;; (org-startup-with-latex-preview t)
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
  ;; (org-export-backends '(ascii beamer html icalendar latex odt pandoc hugo md))
  (org-export-backends '(ascii html latex odt md))
  ;; org v8 bundled with Emacs 24.4
  (org-odt-preferred-output-format "docx")

  :config
  (set-face-attribute 'org-document-title nil :height 1.5 :underline t)
  (set-face-attribute 'org-level-1 nil :height 1.2 :underline nil :weight 'bold :foreground "blue4")
  (set-face-attribute 'org-level-2 nil :height 1.0 :weight 'bold :foreground "sienna4" :inherit nil)
  (set-face-attribute 'org-level-3 nil :height 1.0 :foreground "purple4" :inherit nil)
  (set-face-attribute 'org-level-4 nil :height 1.0 :foreground "red4" :inherit nil)
  (set-face-attribute 'org-level-5 nil :height 1.0 :foreground "dark green" :inherit nil)
  (set-face-attribute 'org-level-6 nil :height 1.0 :foreground "DeepSkyBlue4" :inherit nil)
  (set-face-attribute 'org-level-7 nil :height 1.0 :inherit nil)
  (set-face-attribute 'org-level-8 nil :height 1.0 :inherit nil)

  (set-face-attribute 'org-block nil :background 'unspecified :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :underline '(:line-width (1 . 1) :color "black") :extend t :weight 'demi-bold :height 0.9 :foreground "gray7" :inherit nil)
  (set-face-attribute 'org-block-end-line nil :inherit 'org-block-begin-line)
  (set-face-attribute 'org-code nil :background "grey95" :inherit '(fixed-pitch))
  (set-face-attribute 'org-table nil :foreground "black" :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-todo nil :weight 'unspecified)

  (set-face-attribute 'org-tag nil :width 'condensed :height 0.9 :weight 'regular :underline nil :box '(:color "black" :line-width (1 . -3)) :background "grey90" :foreground "grey10")

  (set-face-attribute 'org-scheduled nil :foreground "black")
  (set-face-attribute 'org-scheduled-today nil :foreground "Blue1")
  (set-face-attribute 'org-scheduled-previously nil :foreground "Red1")
  (set-face-attribute 'org-upcoming-deadline nil :foreground "black")
  (set-face-attribute 'org-upcoming-distant-deadline nil :foreground "grey50")
  (set-face-attribute 'org-imminent-deadline nil :foreground "Red1" :weight 'normal)
  (set-face-attribute 'org-agenda-done nil :foreground "ForestGreen")

  ;; Latex
  (require 'ox-latex)

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

  ;; Org Src
  (with-eval-after-load 'org-src
    (define-key org-src-mode-map (kbd "C-s") #'org-edit-src-save))

  ;; Org Speed
  (defun cory/org-jump-to-heading-beginning ()
    "Jump to the beginning of the line of the closest Org heading."
    (interactive)
    (org-back-to-heading)
    (beginning-of-line))

  ;; For exporting
  (require 'ox)

  (defun cory/org-insert-image ()
    (interactive)
    (insert (concat "[[" (read-file-name "Image: ") "]]"))
    (org-display-inline-images)))

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
  ;; (org-modern-priority nil)
  ;; (org-modern-todo nil)
  (org-modern-tag t)
  (org-modern-table nil)
  ;; Customize this per your font
  (org-modern-label-border .25)
  (org-modern-star ["
\u200b" "\u200b" "•" "-" "•" "–" "•" "–"])
  :config
  (set-face-attribute 'org-modern-tag nil
		      :inherit '(org-modern-label))
  (set-face-attribute 'org-modern-done nil
		      :inherit '(org-done org-modern-label))
  (set-face-attribute 'org-modern-todo nil
		      :weight 'semibold
		      :inverse-video t
		      :inherit '(org-todo org-modern-label))
  (set-face-attribute 'org-modern-label nil
		      :width 'condensed
		      :height 0.9
		      :weight 'regular
		      :underline nil
		      :box '(:color "black"
			     :line-width (1 . -3))
		      :background "grey90"
		      :foreground "grey10")
  (set-face-attribute 'org-modern-symbol nil
		      :inherit 'bold)
  (set-face-attribute 'org-modern-priority nil
		      :weight 'semibold
		      :inverse-video t
		      :inherit   '(org-priority org-modern-label))
  (set-face-attribute 'org-modern-block-name nil
		      :inherit '(org-block-begin-line))
  (set-face-attribute 'org-modern-statistics nil
		      :inherit 'org-modern-done)
  (set-face-attribute 'org-modern-horizontal-rule nil
		      :strike-through "grey10"
		      :inherit 'org-hide)
  (set-face-attribute 'org-modern-date-active nil
		      :inherit 'org-modern-done)
  (set-face-attribute 'org-modern-date-inactive nil
		      :foreground "grey90"
		      :background "dark red"
		      :inherit 'org-modern-label))

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
	 ("C-c o n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

;; Writing
(use-package writegood-mode
  :hook (org-mode . writegood-mode)
  ;; :bind (:map org-mode-map
  ;; 	 ("C-c C-g g" . writegood-grade-level)
  ;; 	 ("C-c C-g e" . writegood-reading-ease))
  :config
  (set-face-attribute 'writegood-weasels-face nil
		      :foreground "DarkOrange"
		      :strike-through t
		      :underline 'unspecified)
  (set-face-attribute 'writegood-duplicates-face nil
		      :foreground "DeepPink"
		      :underline 'unspecified)
  (set-face-attribute 'writegood-passive-voice-face nil
		      :underline '(:style wave :color "deep sky blue"))
  (add-to-list 'writegood-weasel-words "essentially")
  (add-to-list 'writegood-weasel-words "just")
  (add-to-list 'writegood-weasel-words "so"))

;; Spelling
(use-package jinx
  :bind
  (:map text-mode-map
   ("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)
   ([remap next-error] . jinx-next)
   ([remap previous-error] . jinx-previous)
   :map jinx-overlay-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("M-h" . jinx-next)
   ("C-<down>" . jinx-next)
   ("M-t" . jinx-previous)
   ("C-<up>" . jinx-previous)
   :map jinx-correct-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("M-h" . jinx-next)
   ("C-<down>" . jinx-next)
   ("M-t" . jinx-previous)
   ("C-<up>" . jinx-previous)
   :map jinx-repeat-map
   ("M-n"        . nil)
   ("M-p"        . nil)
   ("M-g M-h"    . jinx-next)
   ("M-g C-<down>" . jinx-next)
   ("M-h"        . jinx-next)
   ("C-<down>"   . jinx-next)
   ("h"          . jinx-next)
   ("C-x `"      . jinx-next)
   ("`"          . jinx-next)
   ("M-g M-t"    . jinx-previous)
   ("M-g C-<up>" . jinx-previous)
   ("M-t"        . jinx-previous)
   ("C-<up>"     . jinx-previous)
   ("t"          . jinx-previous))
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook #'jinx-mode))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook sgml-mode-hook nxml-mode-hook))
    (add-hook hook (lambda () (jinx-mode -1))))
  :config
  (set-face-attribute 'jinx-misspelled nil
		      :underline '(:style wave :color "red")))

;;; Latex

;; (use-package cdlatex
;;   :commands 'turn-on-cdlatex)

;; (use-package latex-preview-pane)
