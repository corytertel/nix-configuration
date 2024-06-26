;;; C

;; Tags Setup
(add-hook 'c-mode-hook
	  (lambda ()
	    (setq-local completion-at-point-functions
			(list (cape-capf-super
			       ;; (cape-company-to-capf #'company-yasnippet)
			       #'yasnippet-capf
			       #'cape-dabbrev
			       #'tags-completion-at-point-function)
			      #'cape-file))
	    (let ((tags-dir (locate-dominating-file default-directory "TAGS")))
	      (cond ((not tags-dir)
		     (message "Unable to locate tags file"))
		    ((yes-or-no-p "Found tags file. Load?")
		     (message "Loading tags file: %sTAGS" tags-dir)
		     (visit-tags-table (concat tags-dir "TAGS")))))))

;;; C++
;; (use-package modern-cpp-font-lock
;;   :ensure t)
;; (modern-c++-font-lock-global-mode t)

(use-package cpp-auto-include)

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

;; (use-package irony
;;   :hook (((c++-mode c-mode objc-mode) . irony-mode-on-maybe)
;;          (irony-mode . irony-cdb-autosetup-compile-options))
;;   :config
;;   (defun irony-mode-on-maybe ()
;;     ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: solidity-mode
;;     (when (member major-mode irony-supported-major-modes)
;;       (irony-mode 1))))

;; (use-package irony-eldoc
;;   :hook (irony-mode . irony-eldoc))

;; (use-package srefactor
;;   :bind
;;   (:map c-mode-map
;;    ("C-c C-r" . srefactor-refactor-at-point)
;;    :map c++-mode-map
;;    ("C-c C-r" . srefactor-refactor-at-point))
;;   :config
;;   (semantic-mode 1))

;; (defun code-compile ()
;;   (interactive)
;;   (unless (file-exists-p "Makefile")
;;     (set (make-local-variable 'compile-command)
;; 	 (let ((file (file-name-nondirectory buffer-file-name)))
;; 	   (format "%s -o %s %s"
;; 		   (if  (equal (file-name-extension file) "cpp") "clang++" "clang" )
;; 		   (file-name-sans-extension file)
;; 		   file)))
;;     (compile compile-command)))

;;(global-set-key [f9] 'code-compile)
