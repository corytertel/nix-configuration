;;
;; --- WINDOW MANAGEMENT
;;

(setq focus-follows-mouse t
      mouse-autoselect-window t)

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
;; (global-set-key (kbd "C-c b") 'balance-windows)
(global-set-key (kbd "<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "C-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "M-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "C-M-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "<f2>") 'other-window)
(global-set-key (kbd "C-<f2>") 'other-window)
(global-set-key (kbd "M-<f2>") 'other-window)
(global-set-key (kbd "C-M-<f2>") 'other-window)

(use-package yequake
  :custom
  (yequake-frames
   '(("eshell"
      (width . 0.5)
      (height . 0.5)
      (left . 0.5)
      (top . 0.5)
      (alpha . 0.7)
      (buffer-fns . ("*eshell*<1>"))
      (frame-parameters . ((undecorated . t))))
     ("org-capture"
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.7)
      (buffer-fns . (yequake-org-capture))
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)))))))

;; Popups
;; (use-package popper
;;   :ensure t
;;   :custom
;;   (popper-reference-buffers
;;    '("\\*Messages\\*"
;;      "Output\\*$"
;;      "\\*Async Shell Command\\*"
;;      "\\*eldoc\\*"
;;      "\\*Ibuffer\\*"
;;      "\\*vc-git"
;;      "\\*Help\\*"
;;      "\\*Compile-Log\\*"
;;      "\\*Warnings\\*"
;;      "\\*Chicken Documentation\\*"
;;      geiser-repl-mode
;;      flymake-diagnostics-buffer-mode
;;      calendar-mode
;;      help-mode
;;      compilation-mode
;;      eshell-mode
;;      vterm-mode))
;;   ;; (popper-group-function #'popper-group-by-project) ; project.el projects
;;   (popper-window-height (lambda (win)
;; 			  (fit-window-to-buffer
;; 			   win
;; 			   (frame-height)
;; 			   30)))
;;   :config
;;   (popper-mode)
;;   ;; (popper-echo-mode)

;;   ;; (defvar popper-dedicated-term nil)

;;   ;; (defun popper-open-dedicated-term ()
;;   ;;   (unless popper-mode (user-error "Popper-mode not active!"))
;;   ;;   (unless (and popper-dedicated-term (buffer-live-p popper-dedicated-term))
;;   ;;     (let ((current (buffer-name (current-buffer))))
;;   ;; 	(eshell t)
;;   ;; 	(rename-buffer "*popper-dedicated-term*")
;;   ;; 	(setq popper-dedicated-term (current-buffer))
;;   ;; 	(switch-to-buffer current)))
;;   ;;   (display-buffer "*popper-dedicated-term*")
;;   ;;   (with-current-buffer "*popper-dedicated-term*"
;;   ;;     (run-hooks 'popper-open-popup-hook)))

;;   ;; (defun popper-toggle-dedicated-term ()
;;   ;;   (interactive)
;;   ;;   (if popper-open-popup-alist
;;   ;; 	(popper-close-latest)
;;   ;;     (popper-open-dedicated-term)))

;;   ;; (defun cory/remove-buried-popup-buffer ()
;;   ;;   ""
;;   ;;   (dolist (p1 popper-buried-popup-alist)
;;   ;;     (dolist (p2 (cdr p1))
;;   ;; 	(when (string=
;;   ;; 	       "*popper-dedicated-term*"
;;   ;; 	       (buffer-name (cdr p2)))
;;   ;; 	  (delete p2 (cdr p1))))))

;;   ;; (defun cory/popper-display-in-posframe (buf _)
;;   ;;   (when (posframe-workable-p)
;;   ;;     (posframe-show buf
;;   ;;                    :position t
;;   ;;                    :poshandler #'posframe-poshandler-frame-center
;;   ;;                    :width 150
;;   ;;                    :height 40
;;   ;;                    :border-width 1
;;   ;;                    :border-color "#141404")))

;;   ;; (setq popper-display-function #'cory/popper-display-in-posframe)

;;   :bind
;;   (("C-`" . popper-toggle-latest)
;;    ("C-~" . popper-toggle-type)
;;    ("M-`" . popper-cycle)))

;; (use-package popper
;;   :bind
;;   (("C-`" . popper-toggle-latest)
;;    ("C-~" . popper-toggle-type)
;;    ("M-`" . popper-cycle))
;;   :custom
;;   (popper-reference-buffers '(eshell-mode))
;;   (popper-window-height 40)
;;   :config
;;   ;; Override `popper-open-latest'
;;   (defun popper-open-latest (&optional group)
;;     "Open the last closed popup.

;; Optional argument GROUP is called with no arguments to select
;; a popup buffer to open."
;;     (unless popper-mode (user-error "Popper-mode not active!"))
;;     (let* ((identifier (when popper-group-function group))
;;            (no-popup-msg (format "No buried popups for group %s"
;;                                  (if (symbolp identifier)
;;                                      (symbol-name identifier)
;;                                    identifier))))
;;       (if (null (alist-get identifier popper-buried-popup-alist
;;                           nil 'remove 'equal))
;;           (progn
;; 	    (eshell)
;; 	    (setq aweshell-dedicated-buffer (current-buffer))
;; 	    (previous-buffer)
;; 	    (display-buffer aweshell-dedicated-buffer)
;; 	    (with-current-buffer aweshell-dedicated-buffer
;; 	      (run-hooks 'popper-open-popup-hook))
;; 	    (push (cons (selected-window) aweshell-dedicated-buffer) popper-open-popup-alist))
;; 	(if-let* ((new-popup (pop (alist-get identifier popper-buried-popup-alist
;;                                              nil 'remove 'equal)))
;;                   (buf (cdr new-popup)))
;;             (if (not (buffer-live-p buf))
;; 		(popper-open-latest)
;; 	      (display-buffer buf)
;; 	      (with-current-buffer buf
;; 		(run-hooks 'popper-open-popup-hook)))
;;           (message no-popup-msg)))))

;;   (defun cory/popper-display-in-posframe (buf _)
;;     (when (posframe-workable-p)
;;       (posframe-show buf
;;                      :position t
;;                      :poshandler #'posframe-poshandler-frame-center
;;                      :width 150
;;                      :height 40
;;                      :border-width 1
;;                      :border-color "#141404")))

;;   (setq popper-display-function #'cory/popper-display-in-posframe)
;;   (popper-mode))

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

;; ;; Sidebar (Project Explorer)
;; (use-package dired-sidebar :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;   :custom
;;   (dired-sidebar-subtree-line-prefix "   ")
;;   (dired-sidebar-theme 'nerd)
;;   (dired-sidebar-use-term-integration t)
;;   (dired-sidebar-use-custom-font t)
;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
;;   (set-face-attribute 'dired-sidebar-face nil :inherit 'variable-pitch))
