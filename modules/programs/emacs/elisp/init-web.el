
(defun cory/open-current-file-in-firefox ()
  "Opens the current file in Firefox."
  (interactive)
  (let ((file
	 (if (not (file-remote-p buffer-file-name))
	     buffer-file-name
	   (let ((buf (current-buffer)))
	     (write-file (concat "/tmp/" (replace-regexp-in-string "/" "!" buffer-file-name)))
	     (async-shell-command (concat "firefox file://" buffer-file-name) nil nil)
	     (switch-to-buffer buf))))))
  (async-shell-command (concat "firefox file://" buffer-file-name) nil nil))

(with-eval-after-load 'mhtml-mode
  (define-key mhtml-mode-map (kbd "C-c C-o") #'cory/open-current-file-in-firefox))

;; Automatically close html tags when you type '</'
(setq sgml-quick-keys 'close)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map (kbd "<") nil)
  (define-key sgml-mode-map (kbd "C-c C-o") nil))
