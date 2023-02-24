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
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show empty whitespace
;; (setq whitespace-style '(face trailing tabs lines empty))
(setq whitespace-style '(trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Highlight and navigate TODO keywords
(use-package hl-todo
  :config (global-hl-todo-mode))

