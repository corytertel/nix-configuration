;; Syntax checking
(use-package flymake
  :ensure t
  :hook
  (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
   ;; ([remap forward-paragraph]  . flymake-goto-next-error)
   ;; ([remap backward-paragraph] . flymake-goto-prev-error)
   ([remap next-error] . flymake-goto-next-error)
   ([remap prev-error] . flymake-goto-prev-error)
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
   flymake-error-bitmap '(cory-error-mark compilation-error)))

(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config
  (setq flymake-diagnostic-at-point-error-prefix nil)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package flymake-racket
  :ensure t
  :commands (flymake-racket-add-hook)
  :init
  (add-hook 'racket-mode-hook #'flymake-racket-add-hook))

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
