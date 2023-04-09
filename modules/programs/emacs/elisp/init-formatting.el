;; Automatically remove trailing whitespace if user put it there
(use-package ws-butler
  :hook
  ((text-mode prog-mode) . ws-butler-mode)
  ;; (java-mode . (lambda () (ws-butler-mode -1)))
  ;; :custom
  ;; (ws-butler-keep-whitespace-before-point nil)
  )

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
     sql-interactive-mode
     java-mode
     snippet-mode))
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
(require 'whitespace)
(with-eval-after-load 'whitespace
  (setq whitespace-style '(face trailing lines)
	whitespace-line-column 100)
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (set-face-attribute 'whitespace-line nil
		      :background 'unspecified))

;; Highlight and navigate TODO keywords
(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   '(("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("TEMP" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXXX*" . "#cc9393")
     ("DOING" . "#cc9393")
     ("WAITING" . "#d0bf8f")
     ("MAYBE" . "#dca3a3")
     ("SOMEDAY" . "#d0bf8f")
     ("CANCELLED" . "#8c5353")))
  :config (global-hl-todo-mode))
