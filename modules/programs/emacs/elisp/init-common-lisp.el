;;; Common Lisp

(use-package sly
  :commands (sly sly-connect)
  :init
  (setq-default sly-symbol-completion-mode nil)
  (setq inferior-lisp-program "sbcl")
  ;; (setq sly-default-lisp 'roswell)
  ;; (setq ros-config (concat user-emacs-directory
  ;;                          "ros-conf.lisp"))
  ;; (setq sly-lisp-implementations
  ;;       `((sbcl ("sbcl") :coding-system utf-8-unix)
  ;;         (ccl ("ccl") :coding-system utf-8-unix)
  ;;         (ecl ("ecl") :coding-system utf-8-unix)
  ;;         (roswell ("ros" "-Q" "-l" ,ros-config "run"))
  ;;         (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
  ;;               :coding-system utf-8-unix)))
  (setq sly-lisp-implementations
        `((sbcl ("sbcl") :coding-system utf-8-unix)))

  ;; (defun qlot-sly ()
  ;;   "Start a sly repl using qlot at the projects root"
  ;;   (interactive)
  ;;   (let ((dir (cdr (project-current))))
  ;;     (if (cd dir)
  ;;         (sly 'qlot)
  ;;       (error (format "Failed to cd to %s" dir)))))

  ;; (defun sly-critique-defun ()
  ;;   "Lint this file with lisp-critic"
  ;;   (interactive)
  ;;   ;; (sly-eval-async '(ql:quickload :lisp-critic))
  ;;   (let ((form (apply #'buffer-substring-no-properties
  ;;                      (sly-region-for-defun-at-point))))
  ;;     (sly-eval-async
  ;;      `(cl:format  "~a" (list ,(read form)))
  ;;      nil (sly-current-package))))

  ;; (defun sly-critique-file ()
  ;;   "Lint this file with lisp-critic"
  ;;   (interactive)
  ;;   (sly-eval-async '(ql:quickload :lisp-critic))
  ;;   (sly-eval-async `(lisp-critic:critique ,(buffer-file-name))))

  (add-hook 'sly-mode-hook (lambda ()
			     (setq-local completion-at-point-functions
					 (list (cape-super-capf
						;; #'tempel-complete
						(cape-company-to-capf #'company-yasnippet)
						#'sly-complete-filename-maybe
						#'sly-complete-symbol))))))

(use-package sly-asdf
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sly-repl-ansi-color
  :config
  (push 'sly-repl-ansi-color sly-contribs))

(use-package lisp-extra-font-lock)
