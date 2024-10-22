;; This mode should be turned on only in the modes which it is desired (lisp-like modes).

;; Other packages like cleverparens and lispy do an alright job, but they do not replace the fundamental unit of editing for lisp, which I believe makes for a simpler and more elegant solution. Attempts to keep the line-based editing in lisp require numerous workarounds to make it alright. And it still isn't great.

(require 'evil)
(require 'evil-commands)
(require 'evil-common)
(require 'evil-core)
(require 'evil-macros)
(require 'smartparens)

(evil-define-text-object evil-a-form (count &optional beg end _type)
  "Select a form."
  (let ((bounds (bounds-of-thing-at-point 'list)))
    (if bounds
        (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t)
      (error "No sexp at point."))))

(evil-define-text-object evil-inner-form (count &optional beg end _type)
  "Select inner form."
  (let ((bounds (bounds-of-thing-at-point 'list)))
    (if bounds
        (evil-range (1+ (car bounds)) (1- (cdr bounds)) 'exclusive :expanded t)
      (error "No sexp at point."))))

(define-key evil-operator-state-map "a f" #'evil-a-form)
(define-key evil-operator-state-map "i f" #'evil-inner-form)
