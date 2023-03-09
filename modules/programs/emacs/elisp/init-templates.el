;; ;; Templates
;; (use-package tempel
;;   ;; Require trigger prefix before template name when completing.
;;   ;; :custom
;;   ;; (tempel-trigger-prefix "")

;;   :hook
;;   ;; (prog-mode . cory/tempel-setup-capf)
;;   (emacs-lisp-mode . cory/elisp-super-capf)

;;   :bind (:map tempel-map
;; 	 ("C-<tab>" . tempel-next)
;; 	 ("C-S-<tab>" . tempel-previous)
;; 	 ([remap keyboard-quit] . tempel-done))

;;   :init
;;   ;; Setup completion at point
;;   (defun cory/tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons (cape-super-capf
;; 		       #'tempel-complete
;; 		       (car completion-at-point-functions))
;; 		      (cdr completion-at-point-functions))))

;;   (defun cory/elisp-super-capf ()
;;     (setq-local completion-at-point-functions
;; 		(list (cape-super-capf
;; 		       #'tempel-complete
;; 		       #'elisp-completion-at-point)
;; 		      #'cape-dabbrev
;; 		      #'cape-file)))

;;   ;; Optionally make the Tempel templates available to Abbrev,
;;   ;; either locally or globally. `expand-abbrev' is bound to C-x '.
;;   ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;;   ;; (global-tempel-abbrev-mode)
;;   )

;; Snippets
(use-package yasnippet
  :hook
  (emacs-lisp-mode . cory/elisp-super-capf)
  :config
  ;; Don't touch TAB!!!

  ;; The active keymap while a snippet expansion is in progress.
  (setq yas-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-<tab>")   (yas-filtered-definition 'yas-next-field-or-maybe-expand))
	  (define-key map (kbd "C-M-<tab>") (yas-filtered-definition 'yas-prev-field))
	  (define-key map (kbd "C-g")   (yas-filtered-definition 'yas-abort-snippet))
	  (define-key map (kbd "C-d")   (yas-filtered-definition yas-maybe-skip-and-clear-field))
	  (define-key map (kbd "DEL")   (yas-filtered-definition yas-maybe-clear-field))
	  map))

  ;; The keymap used when `yas-minor-mode' is active.
  (setq yas-minor-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-<tab>") yas-maybe-expand)
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

(use-package company
  ;; :hook (company-mode . (lambda () (company-mode -1)))
  :config
  (require 'company-yasnippet))
