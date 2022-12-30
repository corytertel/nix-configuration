;;; Java

(use-package eglot-java
  :hook
  (java-mode . eglot-java-mode)
  :bind
  (:map eglot-java-mode-map
   ("C-c C-l n" . eglot-java-file-new)
   ("C-c C-l x" . eglot-java-run-main)
   ("C-c C-l t" . eglot-java-run-test)
   ("C-c C-l N" . eglot-java-project-new)
   ("C-c C-l T" . eglot-java-project-build-task)
   ("C-c C-l R" . eglot-java-project-build-refresh)))

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
