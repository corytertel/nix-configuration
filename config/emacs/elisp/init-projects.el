;; Project Management
(use-package project
  :custom
  (project-vc-extra-root-markers
   '(".project" "Cargo.toml" "compile_commands.json" "compile_flags.txt"
     "project.clj" ".git" "gradlew" "pom.xml"))
  :config
  (defcustom cory/project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" ".git" "gradlew" "pom.xml")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)

  (defun cory/project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker cory/project-root-markers)
	(when (file-exists-p (concat path marker))
          (throw 'found marker)))))

  (defun cory/project-find-root (path)
    "Search up the PATH for `cory/project-root-markers'."
    (when-let ((root (locate-dominating-file path #'cory/project-root-p)))
      (cons 'transient (expand-file-name root))))
  (add-to-list 'project-find-functions #'cory/project-find-root))

;; Git Management
(use-package magit
  :bind (("C-c g s" . magit-status)
	 :map magit-stash-mode-map
	 ("W" . magit-toggle-whitespace))
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-options)
	(magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh)))

;; (use-package forge)
