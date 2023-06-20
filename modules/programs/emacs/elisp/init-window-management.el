;;
;; --- WINDOW MANAGEMENT
;;

(setq focus-follows-mouse t
      mouse-autoselect-window t
      pop-up-frames t)

(pixel-scroll-precision-mode t)

(with-eval-after-load 'pixel-scroll
  (define-key pixel-scroll-precision-mode-map (kbd "<next>") nil)
  (define-key pixel-scroll-precision-mode-map (kbd "<prior>") nil))

(defun split-and-follow-below ()
  "Open a new window vertically."
  (interactive)
  (split-window-below)
  (other-window 1)
  (consult-buffer))

(defun split-and-follow-right ()
  "Open a new window horizontally."
  (interactive)
  (split-window-right)
  (other-window 1)
  (consult-buffer))

(defun kill-all-buffers-and-windows ()
  "Kill all buffers and windows."
  (interactive)
  (when (yes-or-no-p "Really kill all buffers and windows? ")
    (save-some-buffers)
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)))

(defun cory/other-window ()
  (interactive)
  (other-window 1)
  (when (equal (buffer-name) "*Dyalog Symbols*")
    (cory/other-window)))

(defun previous-window ()
  "Reverse direction of `other-window'."
  (other-window -1))

;; (defun cory/toggle-last-buffer ()
;;   (interactive)
;;   (switch-to-buffer
;;    (car (cl-remove-if-not
;; 	 (lambda (s) (equal s (string-trim s "[ \*]+" "\*")))
;; 	 (mapcar #'buffer-name (cdr (buffer-list)))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                    (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                    (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun cory/quit-window (&optional arg window)
  (interactive "P")
  (quit-window t window))

;; (defalias 'quit-window #'cory/quit-window
;;   "Use `cory/quit-window' instead of `quit-window'.")

;; Select only "real buffers" when toggling between buffers
(set-frame-parameter (selected-frame) 'buffer-predicate
		     (lambda (buf)
		       (let ((name (buffer-name buf)))
			 (not (or (string-prefix-p "*" name)
			       (eq 'dired-mode (buffer-local-value 'major-mode buf)))))))

(global-set-key (kbd "C-x 0") #'delete-window)
(global-set-key (kbd "C-x 1") #'delete-other-windows)
;; (global-set-key (kbd "C-x 2") #'split-and-follow-below)
;; (global-set-key (kbd "C-x 3") #'split-and-follow-right)
(global-set-key (kbd "C-x 2") #'make-frame-command)
(global-set-key (kbd "C-x 3") #'make-frame-command)
(global-set-key (kbd "C-x 4 q") #'kill-all-buffers-and-windows)
(global-set-key (kbd "<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "M-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-M-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "M-S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-M-S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "<f2>") #'cory/other-window)
(global-set-key (kbd "C-<f2>") #'cory/other-window)
(global-set-key (kbd "M-<f2>") #'cory/other-window)
(global-set-key (kbd "C-M-<f2>") #'cory/other-window)
(global-set-key (kbd "S-<f2>") #'cory/other-window)
(global-set-key (kbd "C-S-<f2>") #'cory/other-window)
(global-set-key (kbd "M-S-<f2>") #'cory/other-window)
(global-set-key (kbd "C-M-S-<f2>") #'cory/other-window)

;; Suppress async-shell-command popup
(add-to-list 'display-buffer-alist
	     '("\\*Async Shell Command\\*.*" display-buffer-no-window))

;; Emacs Everywhere
(use-package emacs-everywhere
  :bind
  (:map emacs-everywhere-mode-map
   ("C-c q" . emacs-everywhere-finish)))
