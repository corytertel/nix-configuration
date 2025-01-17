;;
;; --- WINDOW MANAGEMENT
;;

;; The goal of this is to get as close to tmux window management as possible.

(setq focus-follows-mouse t
      mouse-autoselect-window t)

;; Smooth scrolling
;; (pixel-scroll-precision-mode t)
;; (with-eval-after-load 'pixel-scroll
;;   (define-key pixel-scroll-precision-mode-map (kbd "<next>") nil)
;;   (define-key pixel-scroll-precision-mode-map (kbd "<prior>") nil))

;; Mouse scrolling in terminal emacs
;; (mouse-wheel-mode 1)
;; (xterm-mouse-mode 1)

(defun split-and-follow-below ()
  "Open a new window vertically."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-and-follow-right ()
  "Open a new window horizontally."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun kill-all-buffers-and-windows ()
  "Kill all buffers and windows."
  (interactive)
  (when (yes-or-no-p "Really kill all buffers and windows? ")
    (save-some-buffers)
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)))

(defun cory/delete-window ()
  "Prompt the user to delete currently focused window."
  (interactive)
  (when (yes-or-no-p "Delete window?")
    (delete-window nil)))

(defun cory/delete-other-windows ()
  "Prompt the user to make the currently focused window fill its frame."
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "No other windows exist on the current frame."))
        (t
         (when (yes-or-no-p "Delete other windows?")
           (delete-other-windows nil nil)))))

(defun cory/other-window ()
  (interactive)
  (other-window 1)
  (when (equal (buffer-name) "*Dyalog Symbols*")
    (cory/other-window)))

;; (defun previous-window ()
;;   "Reverse direction of `other-window'."
;;   (other-window -1))

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

;; FIXME
(defun rotate-windows-reverse ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (- (% i numWindows) 1)))

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

(global-set-key (kbd "<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "M-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-M-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "M-S-<f1>") #'mode-line-other-buffer)
(global-set-key (kbd "C-M-S-<f1>") #'mode-line-other-buffer)

;; Suppress async-shell-command popup
(add-to-list 'display-buffer-alist
	     '("\\*Async Shell Command\\*.*" display-buffer-no-window))

;;; Tmux-like Navigation
;;; Make Emacs window-management identical to Tmux window-management for consistency

(require 'window)
(require 'windmove)
(setq windmove-wrap-around t)

(use-package zoom-window)

(require 'tab-bar)

(defun next-workspace ()
  (interactive)
  (cond ((= 1 (length (funcall tab-bar-tabs-function)))
         (message "No next workspace."))
        (t
         (tab-bar-switch-to-next-tab 1))))

(defun previous-workspace ()
  (interactive)
  (cond ((= 1 (length (funcall tab-bar-tabs-function)))
         (message "No previous workspace."))
        (t
         (tab-bar-switch-to-prev-tab 1))))

(defun switch-to-workspace-0 () (interactive) (switch-to-workspace-n "0"))
(defun switch-to-workspace-1 () (interactive) (switch-to-workspace-n "1"))
(defun switch-to-workspace-2 () (interactive) (switch-to-workspace-n "2"))
(defun switch-to-workspace-3 () (interactive) (switch-to-workspace-n "3"))
(defun switch-to-workspace-4 () (interactive) (switch-to-workspace-n "4"))
(defun switch-to-workspace-5 () (interactive) (switch-to-workspace-n "5"))
(defun switch-to-workspace-6 () (interactive) (switch-to-workspace-n "6"))
(defun switch-to-workspace-7 () (interactive) (switch-to-workspace-n "7"))
(defun switch-to-workspace-8 () (interactive) (switch-to-workspace-n "8"))
(defun switch-to-workspace-9 () (interactive) (switch-to-workspace-n "9"))

(defun switch-to-workspace-n (&optional name)
  (interactive)
  (let* ((recent-tabs (mapcar (lambda (tab)
                                (alist-get 'name tab))
                              (tab-bar--tabs-recent)))
         (name (or name (completing-read (format-prompt "Switch to tab by name"
                                                       (car recent-tabs))
                                        recent-tabs nil t nil nil recent-tabs)))
         (tab-index (tab-bar--tab-index-by-name name)))
    (tab-bar-select-tab (1+ tab-index))))

(defun rename-workspace ()
  (interactive)
  (tab-bar-rename-tab
   (concat (substring (cdr (assq 'name (tab-bar--current-tab))) 0 1)
           ": "
           (read-string "Rename workspace: "))))

(defun xor (b1 b2)
  "Exclusive or of its two arguments."
  (or (and b1 b2)
     (and (not b1) (not b2))))

(defun move-border-left-or-right (arg dir)
  "General function covering move-border-left and move-border-right. If DIR is
     t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 1))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun move-border-up-or-down (arg dir)
  "If DIR is t, then move up, otherwise move down."
  (interactive)
  (if (null arg) (setq arg 1))
  (let ((bottom-edge (nth 3 (window-edges))))
    (if (xor dir (= bottom-edge (1+ (frame-height))))
        (enlarge-window arg nil)
      (shrink-window arg nil))))

(defun move-border-left (arg)
  (interactive "P")
  (move-border-left-or-right arg t))

(defun move-border-right (arg)
  (interactive "P")
  (move-border-left-or-right arg nil))

(defun move-border-up (arg)
  (interactive "P")
  (move-border-up-or-down arg t))

(defun move-border-down (arg)
  (interactive "P")
  (move-border-up-or-down arg nil))

(use-package tabspaces
  :hook
  (after-init . tab-bar-mode)
  (after-init . tabspaces-mode)
  :config
  (global-set-key (kbd "C-x b") #'tabspaces-switch-to-buffer)

  (defun delete-current-workspace ()
    (interactive)
    (cond ((= 1 (length (funcall tab-bar-tabs-function)))
           (message "Cannot delete sole workspace."))
          ((yes-or-no-p (concat "Delete workspace " (cdr (assq 'name (tab-bar--current-tab))) "?"))
           (tabspaces-kill-buffers-close-workspace))))

  (defun tabspaces-mouse-1 (event)
    "Close the tab whose \"x\" close button you click.
See also `tabspaces-mouse-close-tab', which closes the tab
regardless of where you click on it.  Also add a new tab."
    (interactive "e")
    (let* ((item (tab-bar--event-to-item (event-start event)))
           (tab-number (tab-bar--key-to-number (nth 0 item))))
      (cond
       ((and (memq (car item) '(add-tab history-back history-forward))
           (functionp (nth 1 item)))
        (call-interactively (nth 1 item)))
       ((and (nth 2 item) (not (eq tab-number t)))
        ;; Kill all buffers of workspace
        (let ((buf (tabspaces--buffer-list nil tab-number)))
          (unwind-protect
              (cl-loop for b in buf
                       do (kill-buffer b))
            (tab-bar-close-tab tab-number)))))))

  (defun tabspaces-mouse-close-tab (event)
    "Close the tab you click on.
This is in contrast with `tabspaces-mouse-1' that closes a tab
only when you click on its \"x\" close button."
    (interactive "e")
    (let* ((item (tab-bar--event-to-item (event-start event)))
           (tab-number (tab-bar--key-to-number (nth 0 item))))
      (unless (eq tab-number t)
        ;; Kill all buffers of workspace
        (let ((buf (tabspaces--buffer-list nil tab-number)))
          (unwind-protect
              (cl-loop for b in buf
                       do (kill-buffer b))
            (tab-bar-close-tab tab-number))))))

  (define-key tab-bar-map (kbd "<mouse-1>") #'tabspaces-mouse-1)
  (define-key tab-bar-map (kbd "<down-mouse-2>") #'tabspaces-mouse-close-tab)

  (setq tab-bar-new-tab-to 'rightmost)
  (setq tabspaces-include-buffers '("*scratch*" "*Messages*"))

  (defvar workspace-number 0)
  (tab-rename "0" 0)

  ;; not exactly how tmux makes new workspaces
  ;; tmux will reuse an old number if it was deleted, this won't
  (defun create-new-workspace ()
    (interactive)
    (setq workspace-number (1+ workspace-number))
    (tab-new)
    (tab-rename (int-to-string workspace-number))
    (tabspaces-remove-buffer)
    (switch-to-buffer "*scratch*"))

  (defun break-window-to-new-workspace ()
    (interactive)
    (let ((buf (current-buffer)))
      (tabspaces-remove-buffer buf)
      (setq workspace-number (1+ workspace-number))
      (tab-new)
      (tab-rename (int-to-string workspace-number))
      (tabspaces-remove-buffer)
      (switch-to-buffer buf)))
  )

;; Selecting a window by number
(use-package winum)

;;; Keymap

(require 'keymap)

(defvar-keymap window-map
  :doc "Keymap for window-management related commands."
  ;; TODO next layout
  "%" #'split-and-follow-right
  "\"" #'split-and-follow-below
  "&" #'delete-current-workspace
  "!" #'break-window-to-new-workspace
  "'" #'switch-to-workspace-n
  "," #'rename-workspace
  "C-o" #'rotate-windows
  "M-o" #'rotate-windows-reverse ; for no reason, this doesn't bind
  "C-x" #'cory/delete-other-windows ; not in tmux, but it's too useful to me
  "x" #'cory/delete-window
  "o" #'cory/other-window
  "z" #'zoom-window-zoom
  "c" #'create-new-workspace
  "n" #'next-workspace
  "p" #'previous-workspace
  "t" #'world-clock
  "q" #'winum-select-window-by-number
  "0" #'switch-to-workspace-0
  "1" #'switch-to-workspace-1
  "2" #'switch-to-workspace-2
  "3" #'switch-to-workspace-3
  "4" #'switch-to-workspace-4
  "5" #'switch-to-workspace-5
  "6" #'switch-to-workspace-6
  "7" #'switch-to-workspace-7
  "8" #'switch-to-workspace-8
  "9" #'switch-to-workspace-9
  "E" #'balance-windows
  "<left>" #'windmove-left
  "<right>" #'windmove-right
  "<up>" #'windmove-up
  "<down>" #'windmove-down
  "C-<left>" #'move-border-left
  "C-<right>" #'move-border-right
  "C-<up>" #'move-border-up
  "C-<down>" #'move-border-down
  "M-<left>" (lambda () (interactive) (move-border-left 5))
  "M-<right>" (lambda () (interactive) (move-border-right 5))
  "M-<up>" (lambda () (interactive) (move-border-up 5))
  "M-<down>" (lambda () (interactive) (move-border-down 5))
  "{" #'windmove-swap-states-up
  "}" #'windmove-swap-states-down
  "?" (lambda () (interactive) (helpful-variable #'window-map))
  ;; ESC and C-g should get you out of everything
  "ESC" #'ignore
  "C-g" #'ignore)

;; Keep C-b to backward-char in both insert and emacs modes
;; Only in normal mode should C-b be used for window management

(define-key evil-normal-state-map (kbd "C-b") window-map)

;;; Unbind default emacs window management binds

(global-set-key (kbd "C-x 0") nil)
(global-set-key (kbd "C-x 1") nil)
(global-set-key (kbd "C-x 2") nil)
(global-set-key (kbd "C-x 3") nil)
(global-set-key (kbd "C-x 4") nil)
(global-set-key (kbd "C-x 5") nil)
