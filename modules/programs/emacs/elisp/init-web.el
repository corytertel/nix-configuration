
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

(defun cory/sgml-mark-tag ()
  (interactive)
  (call-interactively #'sgml-skip-tag-backward)
  (call-interactively #'set-mark-command)
  (call-interactively #'sgml-skip-tag-forward))

;; Automatically close html tags when you type '</'
(setq sgml-quick-keys 'close)
(with-eval-after-load 'sgml-mode
  ;; remove annoying binds
  (define-key sgml-mode-map (kbd "<") nil)
  (define-key sgml-mode-map (kbd ">") nil)
  (define-key sgml-mode-map (kbd "\"") nil)
  (define-key sgml-mode-map (kbd "&") nil)
  (define-key sgml-mode-map (kbd "'") nil)
  (define-key sgml-mode-map (kbd "SPC") nil)
  (define-key sgml-mode-map (kbd "C-c C-o") nil)
  ;; Minimak binds
  (define-key sgml-mode-map  (kbd "C-M-a") nil)
  (define-key sgml-mode-map  (kbd "C-M-e") nil)
  (define-key sgml-mode-map  (kbd "C-M-i") nil)
  (define-key sgml-mode-map  (kbd "C-M-b") #'sgml-skip-tag-backward)
  (define-key sgml-mode-map  (kbd "C-M-y") #'sgml-skip-tag-forward)
  (define-key sgml-mode-map  (kbd "C-M-h") #'cory/sgml-mark-tag)
  (define-key sgml-mode-map  (kbd "C-M-s") #'ispell-complete-word)
  (define-key sgml-mode-map  (kbd "C-c C-b") nil)
  (define-key sgml-mode-map  (kbd "C-c C-f") nil)
  (define-key sgml-mode-map  (kbd "C-c C-j") #'sgml-skip-tag-backward)
  (define-key html-mode-map (kbd "C-c C-j") #'sgml-skip-tag-backward)
  (define-key sgml-mode-map  (kbd "C-c C-l") #'sgml-skip-tag-forward))
