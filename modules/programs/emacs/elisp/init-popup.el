
(setq pop-up-frames t)

(global-set-key (kbd "C-x 2") #'make-frame-command)
(global-set-key (kbd "C-x 3") #'make-frame-command)

;; vertico-frame for wm integration
(setq vertico-frame-frame-alist
      '((name . "vertico-frame")
	(minibuffer . nil)
	(width . 150)
	(height . 12)))
(vertico-frame-mode t)
