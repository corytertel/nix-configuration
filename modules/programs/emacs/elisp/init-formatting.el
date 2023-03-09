;; Automatically remove trailing whitespace if user put it there
(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :config (setq ws-butler-keep-whitespace-before-point nil))

;; Indenting
(use-package aggressive-indent
  :custom
  (aggressive-indent-excluded-modes
   '(elm-mode
     haskell-mode
     inf-ruby-mode
     makefile-mode
     makefile-gmake-mode
     python-mode
     sql-interactive-mode))
  :hook
  (prog-mode . aggressive-indent-mode)
  (html-mode . aggressive-indent-mode)
  (css-mode  . aggressive-indent-mode)
  (java-mode . (lambda () (aggressive-indent-mode -1)))
  :config
  (electric-indent-mode 0))

;; Word wrapping
(global-visual-line-mode 1)
(setq-default fill-column 80)
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Turn ^L into pretty lines
(use-package page-break-lines
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#6597ef"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "#ffa805"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "#0b8007"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "#de7397"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "#6597ef"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "#ffa805"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "#0b8007"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "#de7397"))))
  (rainbow-delimiters-base-error-face ((t (:foreground "#de7397"))))
  (rainbow-delimiters-mismatched-face ((t (:foreground "#de7397"))))
  (rainbow-delimiters-unmatched-face ((t (:foreground "#de7397")))))

;; Show empty whitespace
;; (setq whitespace-style '(face trailing tabs lines empty))
(setq whitespace-style '(trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Highlight and navigate TODO keywords
(use-package hl-todo
  :config (global-hl-todo-mode))

