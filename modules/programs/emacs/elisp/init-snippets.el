;; Snippets
(use-package yasnippet
  :custom-face
  (yas-field-highlight-face ((t (:inherit nil :background "light goldenrod yellow"))))
  :config
  ;; Don't touch TAB!!!
  ;; The active keymap while a snippet expansion is in progress.
  (setq yas-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "<C-h>")  (yas-filtered-definition 'yas-next-field-or-maybe-expand))
	  (define-key map (kbd "C-t")    (yas-filtered-definition 'yas-prev-field))
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
	  (define-key map (kbd "<C-h>") yas-maybe-expand)
	  (define-key map (kbd "<down>") yas-maybe-expand)
	  (define-key map "\C-c&\C-s" 'yas-insert-snippet)
	  (define-key map "\C-c&\C-n" 'yas-new-snippet)
	  (define-key map "\C-c&\C-v" 'yas-visit-snippet-file)
	  map))

  (yas-global-mode 1))

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

(define-auto-insert "\\.c\\'"
  (lambda ()
    (insert "#include <stdio.h>\n\n"
	    "int main(int argc, char** argv) {\n\n"
	    "return 0;\n"
            "}\n")))

(define-auto-insert "\\.cpp\\'"
  (lambda ()
    (insert "#include <iostream>\n\n"
	    "auto main(int argc, char** argv) -> int {\n\n"
	    "return 0;\n"
	    "}\n")))

(define-auto-insert "\\.apl\\'"
  (lambda ()
    (insert ")load buildse\n"
	    "BUILD_SESSION 'US'\n"
	    "]box on -style=max\n"
	    "⎕IO ← 0\n\n")))
