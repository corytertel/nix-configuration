;; Templates
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "")

  :hook
  ;; (prog-mode . cory/tempel-setup-capf)
  (emacs-lisp-mode . cory/elisp-super-capf)

  :bind (:map tempel-map
	 ("C-<tab>" . tempel-next)
	 ("C-S-<tab>" . tempel-previous)
	 ([remap keyboard-quit] . tempel-done))

  :init
  ;; Setup completion at point
  (defun cory/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons (cape-super-capf
		       #'tempel-complete
		       (car completion-at-point-functions))
		      (cdr completion-at-point-functions))))

  (defun cory/elisp-super-capf ()
    (setq-local completion-at-point-functions
		(list (cape-super-capf
		       #'tempel-complete
		       #'elisp-completion-at-point)
		      #'cape-dabbrev
		      #'cape-file)))

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )
