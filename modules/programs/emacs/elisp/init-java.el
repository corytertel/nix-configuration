;;; Java

;; (use-package eglot-java
;;   :hook
;;   (java-mode . eglot-java-mode)
;;   :bind
;;   (:map eglot-java-mode-map
;;    ("C-c C-l n" . eglot-java-file-new)
;;    ("C-c C-l x" . eglot-java-run-main)
;;    ("C-c C-l t" . eglot-java-run-test)
;;    ("C-c C-l N" . eglot-java-project-new)
;;    ("C-c C-l T" . eglot-java-project-build-task)
;;    ("C-c C-l R" . eglot-java-project-build-refresh)))

(use-package lsp-java
  :after lsp
  :config
  (require 'dap-java))

;; For groovy and gradle support
(use-package groovy-mode :defer t)

;; Viewing Java Class files
(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'javap-handler
               (and (eq inhibit-file-name-operation operation)
                  inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (java-mode)
        (current-buffer))))
   ((javap-handler-real op args))))

(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

;;; Snippets

(defun cory/java-snippet-constructor-assignments (arg-string)
  (let ((indentation (make-string (save-excursion
				    (goto-char start-point)
				    (current-indentation))
				  ?\s)))
    (string-trim (mapconcat (lambda (arg)
			      (if (string-match "^\\*" arg)
				  ""
				(format "this.%s = %s;\n%s"
					arg arg indentation)))
			    (cory/java-snippet-split-args arg-string)
			    ""))))

(defun cory/java-snippet-split-args (arg-string)
  (mapcar (lambda (x)
	    (string-trim x ".* "))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

;;; Minimak binds
(with-eval-after-load 'cc-mode
  (define-key java-mode-map (kbd "C-M-a") nil)
  (define-key java-mode-map (kbd "C-M-b") #'c-beginning-of-defun)
  (define-key java-mode-map (kbd "C-M-e") nil)
  (define-key java-mode-map (kbd "C-M-y") #'c-end-of-defun)
  (define-key java-mode-map (kbd "C-c C-n") nil)
  (define-key java-mode-map (kbd "C-c C-e") #'c-forward-conditional)
  (define-key java-mode-map (kbd "C-c C-p") nil)
  (define-key java-mode-map (kbd "C-c C-i") #'c-backward-conditional))
