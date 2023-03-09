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
(global-set-key (kbd "S-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "C-S-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "M-S-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "C-M-S-<f1>") 'cory/toggle-last-buffer)
(global-set-key (kbd "<f2>") 'other-window)
(global-set-key (kbd "C-<f2>") 'other-window)
(global-set-key (kbd "M-<f2>") 'other-window)
(global-set-key (kbd "C-M-<f2>") 'other-window)
(global-set-key (kbd "S-<f2>") 'other-window)
(global-set-key (kbd "C-S-<f2>") 'other-window)
(global-set-key (kbd "M-S-<f2>") 'other-window)
(global-set-key (kbd "C-M-S-<f2>") 'other-window)

;; ;; Workspaces
;; (use-package eyebrowse
;;   :diminish eyebrowse-mode
;;   :custom
;;   (eyebrowse-new-workspace t)
;;   :bind
;;   (:map eyebrowse-mode-map
;;    ("M-1" . eyebrowse-switch-to-window-config-1)
;;    ("M-2" . eyebrowse-switch-to-window-config-2)
;;    ("M-3" . eyebrowse-switch-to-window-config-3)
;;    ("M-4" . eyebrowse-switch-to-window-config-4)
;;    ("M-5" . eyebrowse-switch-to-window-config-5)
;;    ("M-6" . eyebrowse-switch-to-window-config-6)
;;    ("M-7" . eyebrowse-switch-to-window-config-7)
;;    ("M-8" . eyebrowse-switch-to-window-config-8)
;;    ("M-9" . eyebrowse-switch-to-window-config-9)
;;    ("M-0" . eyebrowse-switch-to-window-config-0))
;;   :config
;;   (eyebrowse-mode t))

;;; Tab Bar
;; Use tab-bar for window grouping and configuration within a project (replaces eyebrowse)
(with-eval-after-load 'tab-bar
  (setq tab-bar-show 1
	tab-bar-tab-hints t ;; show numbers in tabs
	;; Unless another file/buffer is designated, start from workspace scratch buffer
	tab-bar-new-tab-choice "*scratch*"
	tab-bar-select-tab-modifiers '(super)
	tab-bar-close-tab-select 'recent
	tab-bar-new-tab-to 'rightmost
	tab-bar-close-last-tab-choice 'tab-bar-mode-disable
	tab-bar-tab-name-format-function #'cory/tab-bar-tab-name-format
	tab-bar-new-button nil
	tab-bar-close-button nil
	tab-bar-format '(tab-bar-format-history
			 tab-bar-format-tabs
			 cory/tab-bar-suffix
			 tab-bar-format-add-tab))

  ;; Tab bar numbers
  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defvar cory/tab-bar--circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨")
      (10 . "⑩")
      (11 . "⑪")
      (12 . "⑫")
      (13 . "⑬")
      (14 . "⑭")
      (15 . "⑮"))

    "Alist of integers to strings of circled unicode numbers.")

  (defun cory/tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
          (tab-num (if (and tab-bar-tab-hints (< i 16))
                       (alist-get i cory/tab-bar--circle-numbers-alist) "")))
      (propertize
       (concat
	" "
	tab-num
	(propertize " " 'display '(space :width (4)))
	(alist-get 'name tab)
	(or (and tab-bar-close-button-show
              (not (eq tab-bar-close-button-show
                     (if current-p 'non-selected 'selected)))
              tab-bar-close-button)
           "")
	(propertize " " 'display '(space :width (4))))
       'face (funcall tab-bar-tab-face-function tab))))

  ;; See https://github.com/rougier/nano-modeline/issues/33
  (defun cory/tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " ")

  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun cory/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Otherwise use completion to select the tab."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
			(tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))

  ;; Binds
  (global-set-key (kbd "s-1") nil)
  (global-set-key (kbd "s-2") nil)
  (global-set-key (kbd "s-3") nil)
  (global-set-key (kbd "s-4") nil)
  (global-set-key (kbd "s-5") nil)
  (global-set-key (kbd "s-6") nil)
  (global-set-key (kbd "s-7") nil)
  (global-set-key (kbd "s-8") nil)
  (global-set-key (kbd "s-9") nil)
  (global-set-key (kbd "M-1") #'tab-bar-select-tab)
  (global-set-key (kbd "M-2") #'tab-bar-select-tab)
  (global-set-key (kbd "M-3") #'tab-bar-select-tab)
  (global-set-key (kbd "M-4") #'tab-bar-select-tab)
  (global-set-key (kbd "M-5") #'tab-bar-select-tab)
  (global-set-key (kbd "M-6") #'tab-bar-select-tab)
  (global-set-key (kbd "M-7") #'tab-bar-select-tab)
  (global-set-key (kbd "M-8") #'tab-bar-select-tab)
  (global-set-key (kbd "M-9") #'tab-bar-select-tab)
  (global-set-key (kbd "C-t") #'cory/tab-bar-select-tab-dwim)

  ;; Faces
  (set-face-attribute 'tab-bar nil
		      :foreground "#3647d9"
		      :background "white"
		      :family "Liberation Serif"
		      :height 130
		      :box '(:line-width (1 . 1)
			     :color nil
			     :style flat-button))

  (set-face-attribute 'tab-bar-tab nil
		      :background "#c0daff"
		      :foreground "black"
		      :weight 'bold)

  (set-face-attribute 'tab-bar-tab-inactive nil
                      :background "white"
		      :foreground "#3647d9")

  (set-face-attribute 'tab-line nil
                      :background "white"
		      :family "Liberation Serif"
		      :height 110)

  (set-face-attribute 'tab-bar-tab-ungrouped nil
                      :background "#ff00ff"
		      :foreground "#ff0000")

  (set-face-attribute 'tab-bar-tab-group-current nil
                      :background "#ff00ff"
		      :foreground "black"
		      :underline t)

  (set-face-attribute 'tab-bar-tab-group-inactive nil
		      :background "#ff00ff"
		      :foreground "#ff0000"))

(require 'tab-bar)
