;;; Java

(add-hook 'java-mode-hook #'subword-mode)

(use-package eglot-java
  :hook
  (java-mode . (unless (file-remote-p buffer-file-name)
		 (eglot-java-mode)))
  :bind
  (:map eglot-java-mode-map
   ("C-c C-l n" . eglot-java-file-new)
   ("C-c C-l x" . eglot-java-run-main)
   ("C-c C-l t" . eglot-java-run-test)
   ("C-c C-l N" . eglot-java-project-new)
   ("C-c C-l T" . eglot-java-project-build-task)
   ("C-c C-l R" . eglot-java-project-build-refresh)))

;; (use-package lsp-java
;;   :after lsp
;;   :config
;;   (require 'dap-java))

;; (with-eval-after-load 'lsp-java
;;   ;; TODO make the version of jdtls match automatically with the version nix uses
;;   ;; TODO make lsp-java use the jdtls that nix installs, don't install it's own
;;   (let ((version "1.19.0")
;; 	(timestamp "202301171536"))
;;     (setq lsp-java-jdt-download-url
;; 	  (concat "https://download.eclipse.org/jdtls/milestones/" version
;; 		  "/jdt-language-server-" version "-" timestamp ".tar.gz"))))

;; jdtls has a problem with auto save
(add-hook 'java-mode-hook
	  (lambda () (auto-save-mode -1)))
(setq lsp-enable-indentation nil
      lsp-enable-relative-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-completion-enable-additional-text-edit nil)

;; For groovy and gradle support
(use-package groovy-mode :defer t)

;; Gradle support
(use-package gradle-mode
  :config
  ;; Java project creation
  (defun cory/gradle-create-new-java-app ()
    (interactive)
    (let ((project-name (read-string "Project Name: ")))
      (shell-command
       (concat
	"gradle init --type java-application --dsl groovy --test-framework junit --project-name "
	project-name " --package " (downcase project-name) " --console plain"))))
  ;; Run project
  (defun cory/gradle-run ()
    "Execute gradle run command."
    (interactive)
    (gradle-run "run")))

;; TODO Gradle project run

;; TODO Gradle project create new class/interface/enum

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
