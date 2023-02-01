;; Buffer management
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
