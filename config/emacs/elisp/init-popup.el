
(setq pop-up-frames t)

(global-set-key (kbd "C-x 2") #'make-frame-command)
(global-set-key (kbd "C-x 3") #'make-frame-command)
(global-set-key (kbd "C-x C-n") #'make-frame-command)

;; vertico-frame for wm integration
(setq vertico-frame-frame-alist
      '((name . "vertico-frame")
	(minibuffer . nil)
	(width . 150)
	(height . 12)))
(vertico-frame-mode t)

;; Center text in window
;; perfect-margin is the fastest to recenter when refocusing/resizing the emacs frame
(use-package perfect-margin
  :custom
  (perfect-margin-visible-width 100)
  ;; (perfect-margin-ignore-regexps '())
  (perfect-margin-ignore-regexps '(" \\*Minibuf-[0-9]+\\*"))
  (perfect-margin-hide-fringes nil)
  :init
  ;; Set fringes before turning on perfect-margin
  ;; Set the fringe to an big enough widthh
  (setq-default fringe-mode 20)
  (set-fringe-mode 20)
  :config
  ;; enable perfect-mode
  (perfect-margin-mode t)
  ;; auto-center minibuffer windows
  ;; (setq perfect-margin-ignore-filters nil)
  ;; auto-center special windows
  ;; (setq perfect-margin-ignore-regexps nil)
  ;; add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (global-set-key (kbd (concat margin "<wheel-up>")) 'mwheel-scroll)
    (global-set-key (kbd (concat margin "<wheel-down>")) 'mwheel-scroll))

  (add-hook 'eww-mode-hook (lambda () (setq-local perfect-margin-visible-width 128))))

;; Modeline
(set-face-attribute 'mode-line nil
    		    :foreground "#141404"
		    :background "#ffffff"
		    :box '(:line-width 1 :color "#3647d9" :style nil)
		    :underline nil
		    :overline nil
		    :family variable-font-name
		    :height variable-font-height)

(setq repeat-echo-function (lambda (keymap)
			     ;; (repeat-echo-message keymap)
			     (cond
			      (keymap
			       (set-face-attribute 'mode-line nil
					           :foreground "#141404"
					           :background "#ffdac0")
			       (set-face-attribute 'mode-line-inactive nil
					           :foreground "#141404"
					           :background "#ffffff"))
			      (t
			       (set-face-attribute 'mode-line nil
					           :foreground "#141404"
					           :background "#ffffff")
			       (set-face-attribute 'mode-line-inactive nil
					           :foreground "#141404"
					           :background "#ffffff")))))

;; Find-file launches external os apps
(defun cory/find-file ()
  (interactive)
  (let* ((file (car (find-file-read-args
		     "Find file: "
		     (confirm-nonexistent-file-or-buffer))))
	 (run (lambda (command)
		(async-shell-command
		 (concat command " " (shell-quote-argument (expand-file-name file)))))))
    (cond
     ((string-match-p ".*\.mp4$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mpeg$" file) (funcall run "vlc"))
     ((string-match-p ".*\.ogg$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mkv$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mov$" file) (funcall run "vlc"))
     ((string-match-p ".*\.webm$" file) (funcall run "vlc"))
     ((string-match-p ".*\.mp3$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.opus$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.wav$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.weba$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.aac$" file) (funcall run "strawberry"))
     ((string-match-p ".*\.doc$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.docx$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.odt$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.ppt$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.pptx$" file) (funcall run "libreoffice"))
     ((string-match-p ".*\.xcf$" file) (funcall run "gimp"))
     ((string-match-p ".*\.pdf$" file) (funcall run "zathura"))
     ((string-match-p ".*\.jpg$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.jpeg$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.png$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.xpm$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.xbm$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.svg$" file) (funcall run "sxiv"))
     ((string-match-p ".*\.webp$" file) (funcall run "sxiv"))
     (t (find-file file)))))

(global-set-key [remap find-file] #'cory/find-file)

;; Dired opes external os apps
(use-package dired-open
  :custom
  (dired-open-extensions '(("mp4" . "vlc")
			   ("mpeg" . "vlc")
			   ("ogg" . "vlc")
			   ("mkv" . "vlc")
			   ("mov" . "vlc")
			   ("webm" . "vlc")
			   ("mp3" . "strawberry")
			   ("opus" . "strawberry")
			   ("wav" . "strawberry")
			   ("weba" . "strawberry")
			   ("aac" . "strawberry")
			   ("doc" . "libreoffice")
			   ("docx" . "libreoffice")
			   ("odt" . "libreoffice")
			   ("ppt" . "libreoffice")
			   ("pptx" . "libreoffice")
			   ("xcf" . "gimp")
			   ("pdf" . "zathura")
			   ("jpg" . "sxiv")
			   ("jpeg" . "sxiv")
			   ("png" . "sxiv")
			   ("xpm" . "sxiv")
			   ("xbm" . "sxiv")
			   ("svg" . "sxiv")
			   ("webp" . "sxiv"))))
