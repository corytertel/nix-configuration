;;
;; --- WINDOW MANAGEMENT
;;

(setq focus-follows-mouse t
      mouse-autoselect-window t)

(pixel-scroll-precision-mode t)

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

(defun previous-window ()
  "Reverse direction of `other-window'."
  (other-window -1))

(defun cory/last-real-buffer (buffers)
  (if buffers
      (let ((name (buffer-name (car buffers))))
	(if (and (equal name (string-trim name "[ \*]+" "\*"))
	      (not (equal "Sunrise" (string-trim name ".*(" ").*"))))
	    (car buffers)
	  (cory/last-real-buffer (cdr buffers))))
    nil))

(defun cory/toggle-last-buffer ()
  (interactive)
  (switch-to-buffer (cory/last-real-buffer (cdr (buffer-list)))))

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

(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-and-follow-below)
(global-set-key (kbd "C-x 3") 'split-and-follow-right)
(global-set-key (kbd "C-x 4 q") 'kill-all-buffers-and-windows)
(global-set-key (kbd "C-c b") 'balance-windows)
(global-set-key (kbd "<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "C-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "M-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "C-M-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "<f2>") 'other-window)
(global-set-key (kbd "C-<f2>") 'other-window)
(global-set-key (kbd "M-<f2>") 'other-window)
(global-set-key (kbd "C-M-<f2>") 'other-window)

;; Workspaces
(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

