;;
;; --- DIRED ---
;;

(setq dired-listing-switches "-ghoaF --group-directories-first"
      dired-dwim-target t
      delete-by-moving-to-trash t)

;; Make M-> and M-< work in dired
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(defun cory/dired ()
  (interactive)
  (dired default-directory))

(defun cory/last-buffer-not-dired (buffers)
  (if buffers
      (let ((name (buffer-name (car buffers))))
	(if (and (equal name (string-trim name "[ \*]+" "\*"))
	       (not (equal 'dired-mode (with-current-buffer (car buffers)
				       major-mode))))
	    (car buffers)
	  (cory/last-real-buffer (cdr buffers))))
    nil))

(defun cory/goto-last-buffer-not-dired ()
  (interactive)
  (switch-to-buffer (cory/last-buffer-not-dired (cdr (buffer-list)))))

(add-hook 'dired-mode-hook (lambda ()
			     (hl-line-mode 1)
			     (setq-local cursor-type nil)))

(defun cory/dired-make-directory ()
  (interactive)
  (call-interactively #'make-directory)
  (call-interactively #'revert-buffer))

(defun cory/dired-make-file ()
  (interactive)
  (call-interactively #'make-empty-file)
  (call-interactively #'revert-buffer))

(defun dired-isearch-filenames-backward ()
  "Search backwards for a string using Isearch only in file names in the Dired buffer."
  (interactive)
  (setq-local dired-isearch-filenames t)
  (isearch-backward nil t))

(defun dired-isearch-filenames-backward-regexp ()
  "Search backwards for a regexp using Isearch only in file names in the Dired buffer."
  (interactive)
  (setq-local dired-isearch-filenames t)
  (isearch-backward-regexp nil t))

;; Binds
(global-set-key (kbd "C-/") #'cory/dired)
(define-key dired-mode-map (kbd "C-/") #'cory/goto-last-buffer-not-dired)

;; Minimak binds
(define-key dired-mode-map (kbd "C-M-n") nil)
(define-key dired-mode-map (kbd "C-M-h") #'dired-next-subdir)
(define-key dired-mode-map (kbd "C-M-p") nil)
(define-key dired-mode-map (kbd "C-M-t") #'dired-prev-subdir)
(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "C-d") #'dired-display-file)
(define-key dired-mode-map (kbd "* C-n") nil)
(define-key dired-mode-map (kbd "* <C-h>") #'dired-next-marked-file)
(define-key dired-mode-map (kbd "* C-p") nil)
(define-key dired-mode-map (kbd "* C-t") #'dired-prev-marked-file)
(define-key dired-mode-map (kbd "M-s a C-M-s") nil)
(define-key dired-mode-map (kbd "M-f a C-M-f") #'dired-do-isearch-regexp)
(define-key dired-mode-map (kbd "M-s a C-s") nil)
(define-key dired-mode-map (kbd "M-f a C-f") #'dired-do-isearch)
(define-key dired-mode-map (kbd "M-s f C-M-s") nil)
(define-key dired-mode-map (kbd "M-f f C-f") #'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "M-s f C-s") nil)
(define-key dired-mode-map (kbd "M-f f C-M-f") #'dired-isearch-filenames)
(define-key dired-mode-map (kbd "f") #'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "r") #'dired-isearch-filenames-backward-regexp)
(define-key dired-mode-map (kbd "n") nil)
(define-key dired-mode-map (kbd "h") #'dired-next-line)
(define-key dired-mode-map (kbd "t") #'dired-previous-line)
(define-key dired-mode-map (kbd "p") #'dired-maybe-insert-subdir)
(define-key dired-mode-map (kbd "C-M-f") #'dired-isearch-filenames)
(define-key dired-mode-map (kbd "C-M-r") #'dired-isearch-filenames-backward)
(define-key dired-mode-map (kbd "C-f") #'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "C-r") #'dired-isearch-filenames-backward-regexp)
(define-key dired-mode-map (kbd "<find>") #'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "M-<find>") #'dired-isearch-filenames)
(define-key dired-mode-map (kbd "C-c C-d") #'cory/dired-make-directory)
(define-key dired-mode-map (kbd "C-c C-f") #'cory/dired-make-file)
(define-key dired-mode-map (kbd "C-t") nil)
(define-key dired-mode-map (kbd "C-t .") nil)
(define-key dired-mode-map (kbd "C-t C-t") nil)
(define-key dired-mode-map (kbd "C-t a") nil)
(define-key dired-mode-map (kbd "C-t c") nil)
(define-key dired-mode-map (kbd "C-t d") nil)
(define-key dired-mode-map (kbd "C-t e") nil)
(define-key dired-mode-map (kbd "C-t f") nil)
(define-key dired-mode-map (kbd "C-t i") nil)
(define-key dired-mode-map (kbd "C-t j") nil)
(define-key dired-mode-map (kbd "C-t r") nil)
(define-key dired-mode-map (kbd "C-t t") nil)
(define-key dired-mode-map (kbd "C-t x") nil)
(define-key dired-mode-map (kbd "M-t .") #'image-dired-display-thumb)
(define-key dired-mode-map (kbd "M-t M-t") #'image-dired-dired-toggle-marked-thumbs)
(define-key dired-mode-map (kbd "M-t a") #'image-dired-display-thumbs-append)
(define-key dired-mode-map (kbd "M-t c") #'image-dired-dired-comment-files)
(define-key dired-mode-map (kbd "M-t d") #'image-dired-display-thumbs)
(define-key dired-mode-map (kbd "M-t e") #'image-dired-dired-edit-comment-and-tags)
(define-key dired-mode-map (kbd "M-t f") #'image-dired-mark-tagged-files)
(define-key dired-mode-map (kbd "M-t i") #'image-dired-dired-display-image)
(define-key dired-mode-map (kbd "M-t j") #'image-dired-jump-thumbnail-buffer)
(define-key dired-mode-map (kbd "M-t r") #'image-dired-delete-tag)
(define-key dired-mode-map (kbd "M-t t") #'image-dired-tag-files)
(define-key dired-mode-map (kbd "M-t x") #'image-dired-dired-display-external)

;; NOTE May slow down Emacs
;; Auto refresh buffers
;; (global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
;; (setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose nil)
