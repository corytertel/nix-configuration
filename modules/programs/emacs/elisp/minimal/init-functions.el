;;; Scroll functions

;; (defun cory/scroll-down-half-page ()
;;   "Scroll down half a page while keeping the cursor centered."
;;   (interactive)
;;   (let ((ln (line-number-at-pos (point)))
;; 	(lmax (line-number-at-pos (point-max))))
;;     (cond ((= ln 1) (move-to-window-line nil))
;; 	  ((= ln lmax) (recenter (window-end)))
;; 	  (t (progn
;;                (move-to-window-line -1)
;;                (recenter))))))

;; (defun cory/scroll-up-half-page ()
;;   "Scroll up half a page while keeping the cursor centered."
;;   (interactive)
;;   (previous-line)
;;   (let ((ln (line-number-at-pos (point)))
;; 	(lmax (line-number-at-pos (point-max))))
;;     (cond ((= ln 1) nil)
;; 	  ((= ln lmax) (move-to-window-line nil))
;; 	  (t (progn
;;                (move-to-window-line 0)
;;                (recenter))))))

;; (put 'cory/scroll-down-half-page 'scroll-command t)
;; (put 'cory/scroll-up-half-page 'scroll-command t)

(defun cory/scroll-down (arg)
  "Move cursor down half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (next-line dist)))

(defun cory/scroll-up (arg)
  "Move cursor up half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (previous-line dist)))

(put 'cory/scroll-down 'scroll-command t)
(put 'cory/scroll-up 'scroll-command t)

;;; Selection functions

(defmacro cory/def-bounds-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (car bounds))
	 (set-mark-command nil)
	 (goto-char (cdr bounds))))))

(defmacro cory/def-beginning-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (car bounds))))))

(defmacro cory/def-end-of-thing (name thing)
  `(defun ,name ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point ,thing)))
       (when bounds
	 (goto-char (cdr bounds))))))

(cory/def-bounds-of-thing cory/mark-word 'word)
(cory/def-bounds-of-thing cory/mark-list 'list)
(cory/def-bounds-of-thing cory/mark-symbol 'symbol)
(cory/def-bounds-of-thing cory/mark-sexp 'sexp)
(cory/def-bounds-of-thing cory/mark-number 'number)
(cory/def-bounds-of-thing cory/mark-sentence 'sentence)
(cory/def-bounds-of-thing cory/mark-url 'url)
(cory/def-bounds-of-thing cory/mark-email 'email)
(cory/def-bounds-of-thing cory/mark-line 'line)

(cory/def-beginning-of-thing cory/beginning-of-word 'word)
(cory/def-beginning-of-thing cory/beginning-of-list 'list)
(cory/def-beginning-of-thing cory/beginning-of-symbol 'symbol)
(cory/def-beginning-of-thing cory/beginning-of-sexp 'sexp)
(cory/def-beginning-of-thing cory/beginning-of-number 'number)
(cory/def-beginning-of-thing cory/beginning-of-sentence 'sentence)
(cory/def-beginning-of-thing cory/beginning-of-url 'url)
(cory/def-beginning-of-thing cory/beginning-of-email 'email)
(cory/def-beginning-of-thing cory/beginning-of-line 'line)

(cory/def-end-of-thing cory/end-of-word 'word)
(cory/def-end-of-thing cory/end-of-list 'list)
(cory/def-end-of-thing cory/end-of-symbol 'symbol)
(cory/def-end-of-thing cory/end-of-sexp 'sexp)
(cory/def-end-of-thing cory/end-of-number 'number)
(cory/def-end-of-thing cory/end-of-sentence 'sentence)
(cory/def-end-of-thing cory/end-of-url 'url)
(cory/def-end-of-thing cory/end-of-email 'email)
(cory/def-end-of-thing cory/end-of-line 'line)

(defun cory/beginning-of-workspace ()
  "If a secondary selection is active, goto the beginning of it.
Else, goto the beginning of the buffer."
  (interactive)
  (if (and
      (secondary-selection-exist-p)
      (< (overlay-start mouse-secondary-overlay)
	 (overlay-end mouse-secondary-overlay))
      (<= (overlay-start mouse-secondary-overlay)
	 (point)
	 (overlay-end mouse-secondary-overlay)))
      (goto-char (overlay-start mouse-secondary-overlay))
    (goto-char (point-min))))

(defun cory/end-of-workspace ()
  "If a secondary selection is active, goto the end of it.
Else, goto the end of the buffer."
  (interactive)
  (if (and
      (secondary-selection-exist-p)
      (< (overlay-start mouse-secondary-overlay)
	 (overlay-end mouse-secondary-overlay))
      (<= (overlay-start mouse-secondary-overlay)
	 (point)
	 (overlay-end mouse-secondary-overlay)))
      (goto-char (- (overlay-end mouse-secondary-overlay) 1))
    (goto-char (point-max))))

;;; Mouse functions

(defun cory/mouse-goto-bol (click)
  "Move to beginning of line for mouse-1 click in left fringe."
  (interactive "e")
  (cory/mouse-goto-line click 'left))

(defun cory/mouse-goto-eol (click)
  "Move to beginning of line for mouse-1 click in left fringe."
  (interactive "e")
  (mouse-goto-line click 'right))

(defun cory/mouse-goto-line (click left/right)
  "Helper for `mouse-goto-(bol|eol)'."
  (let* ((posn      (event-start click))
         (click-pt  (posn-point posn))
         (window    (posn-window posn))
         (buf       (window-buffer window))
         (clicks    (if (eq mouse-selection-click-count-buffer buf)
                        (event-click-count click)
                      0)))
    (when (= clicks 1)                  ; No-op if not single-click.
      (with-current-buffer buf
        (goto-char click-pt)
        (if (eq 'left left/right)
            (line-beginning-position)
          (line-end-position))))))

;;; Misc functions

(defun cory/create-tmp-file ()
  (interactive)
  (find-file (concat temporary-file-directory (read-string "New tmp file:"))))

(defun cory/insert-space ()
  "Insert a space."
  (interactive)
  (self-insert-command 1 ? )
  (backward-char))

(defun cory/newline-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                            (and (looking-back ">" 1) (looking-at "<"))
                            (and (looking-back "(" 1) (looking-at ")"))
                            (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))
