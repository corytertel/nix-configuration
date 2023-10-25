;; Syntax checking
(use-package flymake
  :ensure t
  :hook
  (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
   ([remap next-error] . flymake-goto-next-error)
   ([remap previous-error] . flymake-goto-prev-error)
   :map goto-map
   ("d"   . flymake-show-buffer-diagnostics)
   ("M-d" . flymake-show-project-diagnostics))

  :init
  ;; Disable legacy diagnostic functions as some have bugs (mainly haskell)
  (setq flymake-proc-ignored-file-name-regexps '("\\.l?hs\\'"))
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  :config
  (define-fringe-bitmap 'cory-info-mark
    (vector #b0000001111000000
	    #b0000111111110000
	    #b0001110000111000
	    #b0011000000001100
	    #b0110000000000110
	    #b0110000000000110
	    #b0110000000000110
	    #b0110000000000110
	    #b0011000000001100
	    #b0001100000011000
	    #b0000110000110000
	    #b0000110000110000
	    #b0000011111100000
	    #b0000000000000000
	    #b0000011111100000
	    #b0000001111000000
	    #b0000001111000000)
    16
    16
    'center)

  (define-fringe-bitmap 'cory-warning-mark
    (vector #b0000000110000000
	    #b0000000110000000
	    #b0000001111000000
	    #b0000001111000000
	    #b0000011001100000
	    #b0000011001100000
	    #b0000110000110000
	    #b0000110110110000
	    #b0001100110011000
	    #b0001100110011000
	    #b0011000110001100
	    #b0011000000001100
	    #b0110000110000110
	    #b0110000110000110
	    #b1100000000000011
	    #b1111111111111111)
    16
    16
    'center)

  (define-fringe-bitmap 'cory-error-mark
    (vector #b0011000000001100
	    #b0111100000011110
	    #b1100110000110011
	    #b1100011001100011
	    #b0110001111000110
	    #b0011000110001100
	    #b0001100000011000
	    #b0000110000110000
	    #b0000110000110000
	    #b0001100000011000
	    #b0011000110001100
	    #b0110001111000110
	    #b1100011001100011
	    #b1100110000110011
	    #b0111100000011110
	    #b0011000000001100)
    16
    16
    'center)

  (setq ;; flymake-fringe-indicator-position 'right-fringe
   flymake-note-bitmap '(cory-info-mark compilation-info)
   flymake-warning-bitmap '(cory-warning-mark compilation-warning)
   flymake-error-bitmap '(cory-error-mark compilation-error))

  (set-face-attribute 'flymake-warning nil
		      :underline '(:style wave :color "DarkOrange"))

  ;; Show diagnostics
  ;; (setq flymake-show-diagnostics-at-end-of-line t)
  )

(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :custom
  (flymake-diagnostic-at-point-error-prefix nil)
  (flymake-diagnostic-at-point-timer-delay 0.1)
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  ;; Redefinition
  ;; (defun flymake-diagnostic-at-point-maybe-display ()
  ;;     "Display the flymake diagnostic text for the thing at point.

  ;; The diagnostic text will be rendered using the function defined
  ;; in `flymake-diagnostic-at-point-display-diagnostic-function.'"
  ;;     (when-let* ((m flymake-mode)
  ;; 		(prop (get-char-property (point) 'flymake-diagnostic))
  ;; 		(text (flymake--diag-text prop))
  ;; 		(type (flymake--diag-type prop)))
  ;;       (save-excursion
  ;; 	(move-beginning-of-line nil)
  ;; 	(funcall flymake-diagnostic-at-point-display-diagnostic-function
  ;; 		 (cond ((eq type ':warning) (propertize text 'face 'warning))
  ;; 		       ((eq type ':error)   (propertize text 'face 'error))
  ;; 		       (t text))))))
  )

(use-package flymake-kondor
  :ensure t
  :hook
  (clojure-mode . flymake-kondor-setup)
  (clojurescript-mode . flymake-kondor-setup)
  (clojurec-mode . flymake-kondor-setup))

;; (use-package flymake-joker
;;   :config
;;   (add-hook 'clojure-mode-hook #'flymake-joker-clj-enable)
;;   (add-hook 'clojurescript-mode-hook #'flymake-joker-cljs-enable)
;;   (add-hook 'clojure-mode-hook #'flymake-mode))

;; Display help messages automatically in echo area
(setq help-at-pt-timer-delay 0.1)
(setq help-at-pt-display-when-idle '(flymake-diagnostic))

;; (use-package flycheck
;;   :hook
;;   (prog-mode . flycheck-mode)
;;   :bind
;;   (:map flycheck-mode-map
;;    ([remap next-error] . flycheck-next-error)
;;    ([remap previous-error] . flycheck-previous-error)
;;    ;; :map goto-map
;;    ;; ("d"   . flymake-show-buffer-diagnostics)
;;    ;; ("M-d" . flymake-show-project-diagnostics)
;;    )
;;   )

;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :config
;;   (flycheck-pos-tip-mode 1))

;; (use-package consult-flycheck
;;   :after (consult flycheck)
;;   :bind (:map flycheck-mode-map
;; 	 ("M-f f" . consult-flycheck)
;; 	 ("M-f M-f" . consult-flycheck)))

;; (use-package flycheck-eglot
;;   ;; :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))
