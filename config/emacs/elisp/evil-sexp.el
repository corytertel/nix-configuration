turned on only in the modes which it is desired (lisp-like modes).

;; Other packages like cleverparens and lispy do an alright job, but they do not replace the fundamental unit of editing for lisp, which I believe makes for a simpler and more elegant solution. Attempts to keep the line-based editing in lisp require numerous workarounds to make it alright. And it still isn't great.

;; Many operations are inspired by cleverparens

(require 'evil)
(require 'smartparens)

;; Need to check whether the current object is a sexp or a comment

;; TODO make the command more robust
;; rewrite it how the other evil commands are written

(defun evil-delete-list ()
  (interactive)
  (let ((lst (sp-backward-up-sexp)))
    (sp-get lst (sp--kill-or-copy-region
                 :beg-prf :end nil))))

(defun evil-yank-list ()
  (interactive)
  (let ((lst (sp-backward-up-sexp)))
    (sp-get lst (sp--kill-or-copy-region
                 :beg-prf :end t))))

(define-key evil-normal-state-map (kbd "'") #'evil-delete-list)


(evil-define-operator evil-delete-list (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (when (and (memq type '(inclusive exclusive))
           (not (evil-visual-state-p))
           (eq 'evil-delete-list evil-this-operator)
           (save-excursion (goto-char beg) (bolp))
           (save-excursion (goto-char end) (eolp))
           (<= 1 (evil-count-lines beg end)))
    ;; Delete list-wise. Not for change operator or visual state.
    (let ((new-range (evil-line-expand beg end)))
      (setq beg (car new-range)
            end (cadr new-range)
            type 'line)))
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
       (= end (point-max))
       (or (= beg end)
          (/= (char-before end) ?\n))
       (/= beg (point-min))
       (= (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t (delete-region beg end)))
  (when (and (eq type 'line)
           (called-interactively-p 'any))
    (evil-first-non-blank)
    (when (and (not evil-start-of-line)
             evil-operator-start-col
             ;; Special exceptions to ever saving column:
             (not (memq evil-this-motion '(evil-forward-word-begin
                                         evil-forward-WORD-begin))))
      (move-to-column evil-operator-start-col))))


;; :map evil-motion-state-map
;; ("H" . sp-backward-sexp)
;; ("L" . sp-forward-sexp)
;; ("(" . sp-backward-up-sexp)
;; (")" . sp-up-sexp)
;; ("{" . sp-down-sexp)
;; ("}" . sp-backward-down-sexp)
