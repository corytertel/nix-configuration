;; Buffer management
;; FIXME
(with-eval-after-load 'ibuf-ext
  ;; Minimak binds
  (define-key ibuffer-mode-map (kbd "M-n") nil)
  (define-key ibuffer-mode-map (kbd "M-p") nil)
  (define-key ibuffer-mode-map (kbd "M-i") #'ibuffer-backward-filter-group)
  (define-key ibuffer-mode-map (kbd "M-e") #'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "l") nil)
  (define-key ibuffer-mode-map (kbd "n") nil)
  (define-key ibuffer-mode-map (kbd "p") #'ibuffer-redisplay)
  (define-key ibuffer-mode-map (kbd "i") #'ibuffer-backward-line)
  (define-key ibuffer-mode-map (kbd "e") #'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "M-f a C-M-f") #'ibuffer-do-isearch-regexp)
  (define-key ibuffer-mode-map (kbd "M-f a C-o") #'ibuffer-do-occur)
  (define-key ibuffer-mode-map (kbd "M-f a C-f") #'ibuffer-do-isearch))

(use-package ibuffer-project
  :hook (ibuffer-mode . cory/ibuffer-setup)
  :custom
  (ibuffer-truncate-lines nil)
  (ibuffer-project-use-cache t)
  (ibuffer-expert t) ; stop yes no prompt on delete
  :config
  (defun cory/ibuffer-setup ()
    (setq ibuffer-filter-groups
	  (append (cdr (ibuffer-project-generate-filter-groups))
		  '(("Programming"
		     (mode . prog-mode))
		    ("Org"
		     (mode . org-mode))
		    ("Magit"
		     (name . "^magit"))
		    ("Planner"
		     (or
		      (name . "^\\*Calendar\\*$")
		      (name . "^\\*Org Agenda\\*")))
		    ("Sunrise"
		     (mode . sunrise-mode))
		    ("Emacs"
		     (or
		      (name . "^\\*scratch\\*$")
		      (name . "^\\*Messages\\*$"))))))))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
