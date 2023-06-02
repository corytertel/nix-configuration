;; Snippets
(use-package yasnippet
  :hook
  (emacs-lisp-mode . cory/elisp-super-capf)
  :config
  ;; Don't touch TAB!!!
  ;; The active keymap while a snippet expansion is in progress.
  (setq yas-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-e")    (yas-filtered-definition 'yas-next-field-or-maybe-expand))
	  (define-key map (kbd "<C-i>")  (yas-filtered-definition 'yas-prev-field))
	  (define-key map (kbd "<down>") (yas-filtered-definition 'yas-next-field-or-maybe-expand))
	  (define-key map (kbd "<up>")   (yas-filtered-definition 'yas-prev-field))
	  (define-key map (kbd "C-g")    (yas-filtered-definition 'yas-abort-snippet))
	  (define-key map (kbd "C-d")    (yas-filtered-definition yas-maybe-skip-and-clear-field))
	  (define-key map (kbd "DEL")    (yas-filtered-definition yas-maybe-clear-field))
	  map))

  ;; The keymap used when `yas-minor-mode' is active.
  (setq yas-minor-mode-map
	(let ((map (make-sparse-keymap)))
	  ;; (define-key map (kbd "C-<tab>") yas-maybe-expand)
	  (define-key map (kbd "C-e") yas-maybe-expand)
	  (define-key map (kbd "<down>") yas-maybe-expand)
	  (define-key map "\C-c&\C-s" 'yas-insert-snippet)
	  (define-key map "\C-c&\C-n" 'yas-new-snippet)
	  (define-key map "\C-c&\C-v" 'yas-visit-snippet-file)
	  map))

  (yas-global-mode 1)

  (defun cory/elisp-super-capf ()
    (setq-local completion-at-point-functions
		(list (cape-super-capf
		       ;; #'tempel-complete
		       (cape-company-to-capf #'company-yasnippet)
		       #'elisp-completion-at-point)
		      #'cape-dabbrev
		      #'cape-file))))

(use-package common-lisp-snippets)

(use-package clojure-snippets)

(use-package gitignore-snippets
  :config (gitignore-snippets-init))

(use-package cape-yasnippet)
