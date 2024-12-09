;; This mode should be turned on only in the modes which it is desired (lisp-like modes).

;; Other packages like cleverparens and lispy do an alright job, but they do not replace the fundamental unit of editing for lisp, which I believe makes for a simpler and more elegant solution. Attempts to keep the line-based editing in lisp require numerous workarounds to make it alright. And it still isn't great.

(require 'evil)
(require 'evil-commands)
(require 'evil-common)
(require 'evil-core)
(require 'evil-macros)
(require 'smartparens)

;;; Text objects

(evil-define-text-object evil-a-form (count &optional beg end _type)
  "Select a form."
  (let ((bounds (bounds-of-thing-at-point 'list)))
    (if bounds
        (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t)
      (error "No form at point."))))

(evil-define-text-object evil-inner-form (count &optional beg end _type)
  "Select inner form."
  (let ((bounds (bounds-of-thing-at-point 'list)))
    (if bounds
        (evil-range (1+ (car bounds)) (1- (cdr bounds)) 'exclusive :expanded t)
      (error "No form at point."))))

;; TODO does not selecet the space after the element
(evil-define-text-object evil-a-element (count &optional beg end _type)
  "Select a element."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t)
      (error "No element at point."))))

(evil-define-text-object evil-inner-element (count &optional beg end _type)
  "Select inner form."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t)
      (error "No element at point."))))

(evil-define-text-object evil-a-string (count &optional beg end _type)
  "Select a string."
  (let ((bounds (bounds-of-evil-string-at-point)))
    (if bounds
        (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t)
      (error "No string at point."))))

(evil-define-text-object evil-inner-string (count &optional beg end _type)
  "Select inner string."
  (let ((bounds (bounds-of-evil-string-at-point)))
    (if bounds
        (evil-range (1+ (car bounds)) (1- (cdr bounds)) 'exclusive :expanded t)
      (error "No string at point."))))

;;; Functions

(defun evil-sexp-insert-beginning-of-sexp ()
  (interactive)
  (sp-beginning-of-sexp)
  (insert " ")
  (backward-char)
  (evil-insert-state 1))

(defun evil-sexp-insert-end-of-sexp ()
  (interactive)
  (sp-end-of-sexp)
  (evil-insert-state 1))

(defun evil-sexp-raise-list ()
  (interactive)
  (sp-backward-up-sexp)
  (sp-raise-sexp))

(defun evil-sexp-beginning-of-next-element ()
  (interactive)
  (sp-forward-sexp 2)
  (sp-backward-sexp))

(defun evil-sexp-beginning-of-previous-element ()
  (interactive)
  (sp-backward-sexp))

(defun evil-sexp-end-of-next-element ()
  (interactive)
  (sp-forward-sexp))

(defun evil-sexp-end-of-previous-element ()
  (interactive)
  (sp-backward-sexp 2)
  (sp-forward-sexp))

(defun evil-sexp--wrap-list-and-insert-beginning (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-backward-up-sexp)
     (sp-wrap-with-pair ,s)
     (insert " ")
     (backward-char)
     (evil-insert-state 1)))

(defun evil-sexp--wrap-element-and-insert-beginning (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)
     (insert " ")
     (backward-char)
     (evil-insert-state 1)))

(defun evil-sexp--wrap-list-and-insert-end (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-backward-up-sexp)
     (sp-wrap-with-pair ,s)
     (sp-forward-sexp)
     (evil-insert-state 1)))

(defun evil-sexp--wrap-element-and-insert-end (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)
     (sp-forward-sexp)
     (evil-insert-state 1)))

(defun evil-sexp-backward-sexp-begin ()
  (interactive)
  (beginning-of-defun))

(defun evil-sexp-forward-sexp-begin ()
  (interactive)
  (beginning-of-defun)
  (sp-forward-sexp 2)
  (sp-backward-sexp))

;; ;; TODO make fn accept number arg
;; (defun evil-sexp-select-prev-element ()
;;   (interactive))

;; ;; TODO make fn accept number arg
;; (defun evil-sexp-select-next-element ()
;;   (interactive))

(defun evil-eval-last-sexp (eval-last-sexp-arg-internal)
  (interactive "P")
  (save-excursion
    (sp-up-sexp)
    (funcall eval-last-sexp-function eval-last-sexp-arg-internal)))

;;; Binds

;; Leader key implementation
(defvar-keymap evil-leader-map
  :doc "Keymap for the evil leader key.")
(define-key evil-motion-state-map "," evil-leader-map)

(define-key evil-outer-text-objects-map "f" #'evil-a-form)
(define-key evil-outer-text-objects-map "e" #'evil-a-element)
(define-key evil-outer-text-objects-map "s" #'evil-a-string)
(define-key evil-inner-text-objects-map "f" #'evil-inner-form)
(define-key evil-inner-text-objects-map "e" #'evil-inner-element)
(define-key evil-inner-text-objects-map "s" #'evil-inner-string)

;; (define-key evil-operator-state-map "i f" #'evil-inner-form)

;; TODO
;; - transpose commands
;; - [e and ]e commands
;; ]] does not work properly
(defun evil-sexp-setup ()
  (evil-local-set-key 'motion "[[" #'evil-sexp-backward-sexp-begin)
  (evil-local-set-key 'motion "]]" #'evil-sexp-forward-sexp-begin)

  (evil-local-set-key 'motion "(" #'sp-backward-up-sexp)
  (evil-local-set-key 'motion ")" #'sp-up-sexp)
  (evil-local-set-key 'motion "\M-(" #'sp-down-sexp)
  (evil-local-set-key 'motion "\M-)" #'sp-backward-down-sexp)

  (local-set-key "\M-b" nil)
  (local-set-key "\M-w" nil)
  (local-set-key "\M-e" nil)
  (evil-local-set-key 'motion "\M-b" #'evil-sexp-beginning-of-previous-element)
  (evil-local-set-key 'motion "\M-w" #'evil-sexp-beginning-of-next-element)
  (evil-local-set-key 'motion "g\M-e" #'evil-sexp-end-of-previous-element)
  (evil-local-set-key 'motion "\M-e" #'evil-sexp-end-of-next-element)

  (evil-local-set-key 'motion ",h" #'evil-sexp-insert-beginning-of-sexp)
  (evil-local-set-key 'motion ",l" #'evil-sexp-insert-end-of-sexp)

  (evil-local-set-key 'normal ",o" #'evil-sexp-raise-list)
  (evil-local-set-key 'normal ",O" #'sp-raise-sexp)
  (evil-local-set-key 'normal ",s" #'sp-split-sexp)
  (evil-local-set-key 'normal ",@" #'sp-splice-sexp)
  (evil-local-set-key 'normal ",?" #'sp-convolute-sexp)

  (evil-local-set-key 'normal "\M-\S-j" #'sp-backward-barf-sexp)
  (evil-local-set-key 'normal "\M-\S-k" #'sp-forward-barf-sexp)
  (evil-local-set-key 'normal "\M-\S-h" #'sp-backward-slurp-sexp)
  (evil-local-set-key 'normal "\M-\S-l" #'sp-forward-slurp-sexp)

  (evil-local-set-key 'normal ",i" (evil-sexp--wrap-list-and-insert-beginning "("))
  (evil-local-set-key 'normal ",I" (evil-sexp--wrap-list-and-insert-end "("))
  (evil-local-set-key 'normal ",[" (evil-sexp--wrap-list-and-insert-beginning "["))
  (evil-local-set-key 'normal ",]" (evil-sexp--wrap-list-and-insert-end "["))
  (evil-local-set-key 'normal ",{" (evil-sexp--wrap-list-and-insert-beginning "{"))
  (evil-local-set-key 'normal ",}" (evil-sexp--wrap-list-and-insert-end "{"))

  (evil-local-set-key 'normal ",w" (evil-sexp--wrap-element-and-insert-beginning "("))
  (evil-local-set-key 'normal ",W" (evil-sexp--wrap-element-and-insert-end "("))
  (evil-local-set-key 'normal ",e[" (evil-sexp--wrap-element-and-insert-beginning "["))
  (evil-local-set-key 'normal ",e]" (evil-sexp--wrap-element-and-insert-end "["))
  (evil-local-set-key 'normal ",e{" (evil-sexp--wrap-element-and-insert-beginning "{"))
  (evil-local-set-key 'normal ",e}" (evil-sexp--wrap-element-and-insert-end "{"))

  (evil-local-set-key 'normal ",x" #'evil-eval-last-sexp))


(add-hook 'emacs-lisp-mode-hook #'evil-sexp-setup)
(add-hook 'lisp-mode-hook #'evil-sexp-setup)
(add-hook 'scheme-mode-hook #'evil-sexp-setup)

;; Vim already comes with predefined xml tags as objects

;;; Functions

(defun evil-sexp--wrap-xml-tag (beg end tag)
  (goto-char end)
  (insert (concat "</" tag ">"))
  (goto-char beg)
  (insert (concat "<" tag ">")))

(defun evil-sexp-wrap-tag-and-insert-beginning ()
  (interactive)
  ;; This is a hacky way to get the bounds
  ;; TODO find better way
  (let ((beg (save-excursion (sp-backward-up-sexp) (point)))
        (end (save-excursion (sp-up-sexp) (point)))
        (tag (read-string "Tag name: ")))
    (evil-sexp--wrap-xml-tag beg end tag)
    (evil-insert-state 1)))

(defun evil-sexp-wrap-tag-and-insert-end ()
  (interactive)
  ;; This is a hacky way to get the bounds
  ;; TODO find better way
  (let ((beg (save-excursion (sp-backward-up-sexp) (point)))
        (end (save-excursion (sp-up-sexp) (point)))
        (tag (read-string "Tag name: ")))
    (evil-sexp--wrap-xml-tag beg end tag)
    (goto-char (+ end 2 (length tag)))
    (evil-insert-state 1)))

(defun evil-sexp-wrap-element-and-insert-beginning ()
  (interactive)
  ;; This is a hacky way to get the bounds
  ;; TODO find better way
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (beg (car bounds))
         (end (cdr bounds))
         (tag (read-string "Tag name: ")))
    (evil-sexp--wrap-xml-tag beg end tag)
    (evil-insert-state 1)))

(defun evil-sexp-wrap-element-and-insert-end ()
  (interactive)
  ;; This is a hacky way to get the bounds
  ;; TODO find better way
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (beg (car bounds))
         (end (cdr bounds))
         (tag (read-string "Tag name: ")))
    (evil-sexp--wrap-xml-tag beg end tag)
    (goto-char (+ end 2 (length tag)))
    (evil-insert-state 1)))

(defun evil-sexp-break-tag ()
  (interactive)
  (let (beg)
    (save-excursion
      (sp-backward-up-sexp)
      (setq beg (point))
      (newline)
      (backward-char)
      (sp-forward-sexp)
      (newline)
      (forward-char)
      (indent-region beg (point)))))

;;; Binds

(defun evil-xml-setup ()
  (evil-local-set-key 'normal ",<" #'evil-sexp-wrap-tag-and-insert-beginning)
  (evil-local-set-key 'normal ",>" #'evil-sexp-wrap-tag-and-insert-end)
  (evil-local-set-key 'normal ",e<" #'evil-sexp-wrap-element-and-insert-beginning)
  (evil-local-set-key 'normal ",e>" #'evil-sexp-wrap-element-and-insert-end)
  (evil-local-set-key 'normal ",b" #'evil-sexp-break-tag))

(add-hook 'web-mode-hook
          (lambda () (and buffer-file-name
		   (string-match ".*\\.html" buffer-file-name)
                   (progn (evil-sexp-setup)
                          (evil-xml-setup)))))

(add-hook 'js-jsx-mode-hook (lambda () (evil-sexp-setup) (evil-xml-setup)))
(add-hook 'js-ts-mode-hook (lambda () (evil-sexp-setup) (evil-xml-setup)))
(add-hook 'tsx-ts-mode-hook (lambda () (evil-sexp-setup) (evil-xml-setup)))
