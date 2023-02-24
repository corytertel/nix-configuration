;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode))

;; F#
(use-package fsharp-mode)

(use-package eglot-fsharp
  :hook (fsharp-mode . cory/eglot-ensure))

;; prettify symbols
(defun fsharp-enable-prettify-symbols ()
  (let ((alist '(("->" . #x2192)
                 ("<-" . #x2190)
                 ("|>" . #x22b3)
                 ("<|" . #x22b2))))
    (setq-local prettify-symbols-alist alist)))

(add-hook 'fsharp-mode-hook
          (lambda ()
            (fsharp-enable-prettify-symbols)))
