;; Project Management
(use-package project)

;; Git Management
(use-package magit
  :bind (("C-c g s" . magit-status)
	 :map magit-stash-mode-map
	 ("W" . magit-toggle-whitespace))
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-options)
	(magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh)))

;; (use-package forge)
