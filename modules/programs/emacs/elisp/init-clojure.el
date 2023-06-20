;;; TODO Clean up clojure config
;;; Clojure
(use-package clojure-mode
  :defer t
  :interpreter ("bb" . clojure-mode)
  :config
  ;; Eval top level forms inside comment forms instead of the comment form itself
  (setq clojure-toplevel-inside-comment-form t)
  ;; Indent fn-traced and defn-traced the same as a regular defn.
  ;; The macros are for re-frame-10x tracing.
  (put-clojure-indent 'fn-traced :defn)
  (put-clojure-indent 'defn-traced :defn))

(use-package cider
  :bind (:map cider-mode-map
         ("M-?" . cider-maybe-clojuredocs)
	 ("C-x <down>" . cider-eval-last-sexp)
         :map cider-repl-mode-map
         ("M-?" . cider-doc))
  :hook ( ;; ((cider-mode cider-repl-mode) . cider-company-enable-fuzzy-completion)
         (cider-mode . eldoc-mode))
  :config
  (set-face-attribute 'cider-result-overlay-face nil
		      :background 'unspecified
		      :box 'unspecified
		      :inherit eval-result-overlay-face)

  (defun cider-maybe-clojuredocs (&optional arg)
    "Like `cider-doc' but call `cider-clojuredocs' when invoked with prefix arg in `clojure-mode'."
    (interactive "P")
    (if (and arg (eq major-mode 'clojure-mode))
        (cider-clojuredocs arg)
      (cider-doc)))

  ;; Location of the jdk sources for NixOS
  (setq cider-jdk-src-paths "/etc/profiles/per-user/cory/lib/openjdk/lib/src.zip")

  (require 's)

  ;; Inject reveal middleware in cider-jack-in when the `:reveal' alias is set
  (defun cider-cli-global-options-contains-reveal? (&rest _)
    (and cider-clojure-cli-global-options
       (s-contains? ":reveal" cider-clojure-cli-global-options)))
  (add-to-list 'cider-jack-in-nrepl-middlewares
               '("vlaaad.reveal.nrepl/middleware" :predicate cider-cli-global-options-contains-reveal?))

  ;; Inject shadowcljs nrepl middleware in cider-jack-in when the `:cljs' alias is set
  (defun cider-cli-global-options-contains-cljs? (&rest _)
    (and cider-clojure-cli-global-options
       (s-contains? ":cljs" cider-clojure-cli-global-options)))
  (add-to-list 'cider-jack-in-nrepl-middlewares
               '("shadow.cljs.devtools.server.nrepl/middleware" :predicate cider-cli-global-options-contains-cljs?))


  ;; jack-in for babashka
  (defun cider-jack-in-babashka ()
    "Start an babashka nREPL server for the current project and connect to it."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (process-filter (lambda (proc string)
                             "Run cider-connect once babashka nrepl server is ready."
                             (when (string-match "Started nREPL server at .+:\\([0-9]+\\)" string)
                               (cider-connect-clj (list :host "localhost"
                                                        :port (match-string 1 string)
                                                        :project-dir default-directory)))
                             ;; Default behavior: write to process buffer
                             (internal-default-process-filter proc string))))
      (set-process-filter
       (start-file-process "babashka" "*babashka*" "bb" "--nrepl-server" "0")
       process-filter)))

  ;; Store more items in repl history (default 500)
  (setq cider-repl-history-size 2000)
  ;; When loading the buffer (C-c C-k) save first without asking
  (setq cider-save-file-on-load t)
  ;; Don't show cider help text in repl after jack-in
  (setq cider-repl-display-help-banner nil)
  ;; Don't focus repl after sending somehint to there from another buffer
  (setq cider-switch-to-repl-on-insert nil)
  ;; Eval automatically when insreting in the repl (e..g. C-c C-j d/e) (unless called with prefix)
  (setq cider-invert-insert-eval-p t)
  ;; Don't focus error buffer when error is thrown
  (setq cider-auto-select-error-buffer nil)
  ;; Don't focus inspector after evaluating something
  (setq cider-inspector-auto-select-buffer nil)
  ;; Display context dependent info in the eldoc where possible.
  (setq cider-eldoc-display-context-dependent-info t)
  ;; Don't pop to the REPL buffer on connect
  ;; Create and display the buffer, but don't focus it.
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  ;; Just use symbol under point and don't prompt for symbol in e.g. cider-doc.
  (setq cider-prompt-for-symbol nil))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (dolist (magit-require '(("csv" . "clojure.data.csv")
                           ("edn" . "clojure.edn")
                           ;; ("http" . "clj-http.client")
                           ("reagent" . "reagent.core")
                           ("re-frame" . "re-frame.core")))
    (add-to-list 'cljr-magic-require-namespaces magit-require)))

;; (use-package ob-clojure
;;   :after ob
;;   :config
;;   (setq org-babel-clojure-backend 'cider))

(use-package clojure-mode-extra-font-locking)
(use-package paredit)

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
	       ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))
            (rainbow-delimiters-mode)))

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; Define keybindings just for clojure-mode
;; (define-key clojure-mode-map (kbd "SPC l c") 'cider-jack-in)

;; Clojure-mode specific keybindings
(add-hook 'clojure-mode-hook
	  '(cory/leader-keys
	    ","  '(:ignore t :which-key "clojure")
	    ",c" '(cider-jack-in-clj :which-key "cider jack in")
	    ",k" '(cider-load-buffer :which-key "load buffer")))

(setq read-process-output-max (* 1024 1024))
