;; Snippets
(use-package yasnippet
  :hook
  (emacs-lisp-mode . cory/elisp-super-capf)
  :custom-face
  (yas-field-highlight-face ((t (:inherit nil :background "light goldenrod yellow"))))
  :config
  ;; Don't touch TAB!!!
  ;; The active keymap while a snippet expansion is in progress.
  (setq yas-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-e")    (yas-filtered-definition 'yas-next-field-or-maybe-expand))
	  (define-key map (kbd "<C-i>")  (yas-filtered-definition 'yas-prev-field))
	  (define-key map (kbd "<down>") (yas-filtered-definition 'yas-next-field-or-maybe-expand))
	  (define-key map (kbd "<up>")   (yas-filtered-definition 'yas-prev-field))
	  (define-key map (kbd "C-g")    (yas-filtered-definition 'yas-abort-snippet))
	  (define-key map (kbd "C-d")    (yas-filtered-definition yas-maybe-skip-and-clear-field))
	  (define-key map (kbd "DEL")    (yas-filtered-definition yas-maybe-clear-field))
	  map))

  ;; The keymap used when `yas-minor-mode' is active.
  (setq yas-minor-mode-map
	(let ((map (make-sparse-keymap)))
	  ;; (define-key map (kbd "C-<tab>") yas-maybe-expand)
	  (define-key map (kbd "C-e") yas-maybe-expand)
	  (define-key map (kbd "<down>") yas-maybe-expand)
	  (define-key map "\C-c&\C-s" 'yas-insert-snippet)
	  (define-key map "\C-c&\C-n" 'yas-new-snippet)
	  (define-key map "\C-c&\C-v" 'yas-visit-snippet-file)
	  map))

  (yas-global-mode 1)

  (defun cory/elisp-super-capf ()
    (setq-local completion-at-point-functions
		(list (cape-super-capf
		       ;; #'tempel-complete
		       (cape-company-to-capf #'company-yasnippet)
		       ;; #'cape-yasnippet
		       #'elisp-completion-at-point)
		      #'cape-dabbrev
		      #'cape-file))))

(use-package yasnippet-snippets)

(use-package common-lisp-snippets)

(use-package clojure-snippets)

(use-package gitignore-snippets
  :config (gitignore-snippets-init))

(use-package cape-yasnippet)

;; Boilerplate code files
(auto-insert-mode t)

(define-auto-insert "\\.org\\'"
  (lambda ()
    (let ((title (read-string "Enter title: ")))
      (insert "#+title: " title "\n"
              "#+OPTIONS: \\n:t\n"
	      "#+OPTIONS: _:{}\n"
	      "#+OPTIONS: ^:{}\n"
	      "#+STARTUP: latexpreview\n"
	      "#+STARTUP: entitiespretty\n"
	      "#+STARTUP: inlineimages\n"
              "#+DATE: Created on " (format-time-string "%-d %B %Y @%H:%M") "\n\n")
      (newline)
      (goto-char (point-max)))))

(define-auto-insert "\\.sh\\'"
  (lambda ()
    (insert "#!/bin/sh\n\n"
            "# Description: \n"
            "# Author: \n"
            "# Date: " (format-time-string "%Y-%m-%d") "\n\n"
            "# Add your script code here\n")
    (search-forward "Description: ")
    (forward-word)
    (forward-char)))

(define-auto-insert "\\.html\\'"
  (lambda ()
    (insert "<!DOCTYPE html>\n"
	    "<html>\n"
	    "  <head>\n"
	    "    <meta charset='UTF-8'>\n"
	    "    <title></title>\n"
	    "  </head>\n"
	    "  <body>\n"
	    "  </body>\n"
	    "</html>\n")
    (goto-char (point-min))
    (search-forward "<body>")
    (newline)))

(define-auto-insert "\\.java\\'"
  (lambda ()
    (insert "public class " (file-name-sans-extension (buffer-name)) " {\n"
	    "  "
	    "}")
    (previous-line)
    (move-end-of-line nil)))
