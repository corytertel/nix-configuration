;; This mode should be turned on only in the modes which it is desired (lisp-like modes).

;; Other packages like cleverparens and lispy do an alright job, but they do not replace the fundamental unit of editing for lisp, which I believe makes for a simpler and more elegant solution. Attempts to keep the line-based editing in lisp require numerous workarounds to make it alright. And it still isn't great.

;; Many operations are inspired by cleverparens

(require 'evil)
(require 'evil-commands)
(require 'evil-common)
(require 'evil-core)
(require 'evil-macros)
(require 'smartparens)

;; Need to check whether the current object is a sexp or a comment

;; This is a HACK
;; Please try to do this correctly by defining commands/operations instead of
;; hacking around existing line functionality
;; Also define line operations in the future

;; This hijacks line commands
;; It is not a true solution
;; Evil still "thinks" it's operating on a line, which may have unintended consequences

;; You can still do line commands through D,Y,S,C
;; TODO think about replacing those binds instead (will be less hacky)

(evil-define-operator evil-delete-sexp (beg end type register yank-handler)
  :move-point nil
  (interactive "<R><x><y>")
  (message "hi")
  (if (or (eq 'line type) (eq 'screen-line type))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (sp--kill-or-copy-region
                     :beg-prf :end nil)))
    (evil-delete beg end type register yank-handler)))

(evil-define-operator evil-change-sexp (beg end type register yank-handler delete-func)
  :move-point nil
  (interactive "<R><x><y>")
  (if (or (eq 'line type) (eq 'screen-line type))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (sp--kill-or-copy-region
                     :beg-prf :end nil))
        (insert "()")
        (forward-char -1)
        (evil-insert 1))
    (evil-change beg end type register yank-handler delete-func)))

(evil-define-operator evil-yank-sexp (beg end type register yank-handler)
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if (or (eq 'line type) (eq 'screen-line type))
      (save-excursion
        (let ((lst (sp-backward-up-sexp)))
          (sp-get lst (sp--kill-or-copy-region
                       :beg-prf :end t))))
    (evil-yank beg end type register yank-handler)))


(defun evil-sexp-setup ()
  (evil-local-set-key 'normal "d" #'evil-delete-sexp)
  (evil-local-set-key 'normal "c" #'evil-change-sexp)
  (evil-local-set-key 'normal "y" #'evil-yank-sexp))


(add-hook 'emacs-lisp-mode-hook #'evil-sexp-setup)
(add-hook 'lisp-mode-hook #'evil-sexp-setup)
(add-hook 'scheme-mode-hook #'evil-sexp-setup)



;; Trying out binding sexp operations to "s"

(evil-define-text-object evil-sexp (count &optional beg end _type)
  "Select sexp."
  (message (prin1-to-string _type))
  (let ((bounds (bounds-of-thing-at-point 'list)))
    (if bounds
        (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t)
      (error "No sexp at point."))))

(evil-define-motion evil-forward-sexp (count)
  :type exclusive
  (let ((count (or count 1)))
    (sp-forward-sexp count)))

(evil-define-motion evil-backward-sexp (count)
  :type exclusive
  (let ((count (or count 1)))
    (sp-backward-sexp count)))

(define-key evil-operator-state-map "s" #'evil-sexp)
(define-key evil-motion-state-map "s" #'evil-forward-sexp)
(define-key evil-motion-state-map "S" #'evil-backward-sexp)

;; Remove substitute binds
(define-key evil-normal-state-map "s" nil)
(define-key evil-normal-state-map "S" nil)




;; ;;; Alternate binds

;; (define-key evil-motion-state-map "U" #'sp-backward-up-sexp)
;; (define-key evil-motion-state-map "D" #'sp-down-sexp)
;; (define-key evil-motion-state-map "H" #'sp-backward-sexp)
;; (define-key evil-motion-state-map "L" #'sp-forward-sexp)

;; ;; Remove conflicting binds
;; (define-key evil-normal-state-map "D" nil)



(define-key evil-motion-state-map "U" #'sp-backward-up-sexp)
(define-key evil-motion-state-map "D" #'sp-down-sexp)
(define-key evil-motion-state-map "L" #'sp-up-sexp)
(define-key evil-motion-state-map "H" #'sp-backward-down-sexp)
(define-key evil-motion-state-map "B" #'sp-backward-sexp)
(define-key evil-motion-state-map "E" #'sp-forward-sexp)

;; Remove conflicting binds
(define-key evil-normal-state-map "D" nil)
