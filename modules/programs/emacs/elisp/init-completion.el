;;
;; --- COMPLETION ---
;;

;; Keep track of recent files
(recentf-mode t)

;; Recursive minibuffers
(require 'mb-depth)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; Suggest commands used before in the minibuffer first (history)
(savehist-mode t)

;; Fuzzy matching
;; (setq completion-styles '(emacs21 flex))

;; Case insensitve completion
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      bookmark-completion-ignore-case t)

;; Custom vertical completion
(setq read-extended-command-predicate #'command-completion-default-include-p
      ;; completions-format 'one-column
      completion-auto-select nil
      ;; completion-auto-select t
      ;; completion-auto-select 'second-tab
      completions-detailed nil
      ;; completion-styles '(orderless partial-completion basic)
      completion-show-help nil
      completions-header-format (propertize "%s candidates:\n"
					    'face 'shadow)
      completion-auto-help 'visual
      completions-max-height 10
      completion-auto-wrap t)

;; Position and format of completions window
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-parameters . ((mode-line-format . none)))))

;; Disabled for Vertico
;; Automatic popup when minibuffer starts
;; (add-hook 'minibuffer-setup-hook #'minibuffer-completion-help)

;; Disable newline with C-n in minibuffer
(add-hook 'minibuffer-setup-hook
	  (lambda () (setq-local next-line-add-newlines nil)))

(defun cory/kill-dir-or-char ()
  "Kill backward by word for directories else by char"
  (interactive)
  (if (looking-back "/")
      (backward-kill-sexp 1)
    (backward-delete-char 1)))

(defun cory/minibuffer-complete ()
  (interactive)
  (call-interactively #'minibuffer-complete)
  (call-interactively #'minibuffer-completion-help))

(defun cory/switch-to-completions-beginning ()
  (interactive)
  (switch-to-completions))

(defun cory/switch-to-completions-end ()
  (interactive)
  (switch-to-completions)
  (previous-completion 1))

;; TODO make minibuffer completion more intuitive

;; Disabled for Vertico
;; (define-key minibuffer-local-completion-map
;;   (kbd "TAB") #'cory/minibuffer-complete)

(define-key minibuffer-local-completion-map
  (kbd "DEL") #'cory/kill-dir-or-char)

;; (define-key minibuffer-local-completion-map
;;   (kbd "C-e") #'cory/switch-to-completions-beginning)

;; (define-key minibuffer-local-completion-map
;;   (kbd "<C-i>") #'cory/switch-to-completions-end)

;; (define-key minibuffer-local-completion-map
;;   (kbd "<down>") #'cory/switch-to-completions-beginning)

;; (define-key minibuffer-local-completion-map
;;   (kbd "<up>") #'cory/switch-to-completions-end)

(define-key completion-list-mode-map
  [remap scroll-up-command] #'switch-to-minibuffer)

(use-package vertico
  :custom
  (vertico-count 10)
  (vertico-cycle t)
  ;; (vertico-preselect 'prompt)
  (vertico-grid-separator "       ")
  (vertico-grid-min-columns 3)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  ;; makes completion-in-region use consult
  ;; unnatural behavior when using default completion
  (completion-in-region-function #'consult-completion-in-region)
  ;; :init
  ;; (advice-add #'vertico--format-candidate :around
  ;;             (lambda (orig cand prefix suffix index _start)
  ;; 		(setq cand (funcall orig cand prefix suffix index _start))
  ;; 		(concat
  ;; 		 (if (= vertico--index index)
  ;;                    (propertize "» " 'face 'vertico-current)
  ;;                  "  ")
  ;; 		 cand)))
  :config
  (recentf-mode t)
  (savehist-mode t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Vertico keybinds
  ;; (define-key vertico-map "?" #'minibuffer-completion-help)
  ;; (define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
  ;; (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)
  ;; (define-key vertico-map (kbd "RET") #'vertico-insert)

  (defun cory/vertico-slash ()
    (interactive)
    (if (string-match-p "^.*/$" (nth (if (< vertico--index 0) 0 vertico--index) vertico--candidates))
	(vertico-insert)
      (insert "/")))

  (with-eval-after-load 'vertico-reverse
    ;; TODO work on tab behavior
    (define-key vertico-map (kbd "TAB") #'vertico-next)
    (define-key vertico-map (kbd "<backtab>") #'vertico-previous)
    (define-key vertico-map (kbd "/") #'cory/vertico-slash)
    (define-key vertico-map (kbd "M-m") #'vertico-quick-jump)
    (define-key vertico-map (kbd "RET") #'vertico-exit)
    (define-key vertico-reverse-map (kbd "M-n") #'vertico-grid-scroll-up)
    (define-key vertico-reverse-map (kbd "<prior>") #'vertico-grid-scroll-up)
    (define-key vertico-reverse-map (kbd "C-n") #'vertico-grid-scroll-down)
    (define-key vertico-reverse-map (kbd "<next>") #'vertico-grid-scroll-down))

  ;; (with-eval-after-load 'vertico-buffer
  ;;   (setq vertico-buffer-display-action '(display-buffer-reuse-window display-buffer-at-bottom))
  ;;   (add-hook 'vertico-buffer-mode-hook (lambda () (setq-local mode-line-format nil))))

  (vertico-mode t)
  (vertico-grid-mode t)
  (vertico-reverse-mode t)
  ;; (vertico-buffer-mode t)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
         ;; ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :disabled t
  :after vertico
  :ensure t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode))

;; Icons in minibuffer
(use-package all-the-icons-completion
  :disabled t
  :config
  (all-the-icons-completion-mode)
  ;; (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  )

;; Fuzzy matching
(use-package hotfuzz
  :custom
  (completion-styles '(hotfuzz))
  :config
  (require 'hotfuzz-module))

;; Minibuffer visual menu
(use-package consult
  :init
  (setq consult-preview-key nil)
  :bind
  (("C-c f"       . consult-recent-file)
   ;; ([remap switch-to-buffer] . consult-buffer)
   ([remap kmacro-end-or-call-macro-repeat] . consult-kmacro)
   ([remap yank-pop] . consult-yank-pop)
   ([remap occur] . consult-focus-lines)
   ("C-c SPC" . consult-global-mark)
   ([remap imenu] . consult-imenu)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap goto-line] . consult-goto-line)
   :map goto-map
   ("f"       . consult-flymake)
   :map search-map
   ("g"       . consult-grep)
   ("M-g"     . consult-grep)
   ("p"       . consult-ripgrep) ; project search
   :map comint-mode-map
   ("C-c h" . consult-history)
   :map dired-mode-map
   ("O" . consult-file-externally)
   :map minibuffer-local-map
   ("M-r" . consult-history))
  :config
  ;; consult to show xref results
  (with-eval-after-load 'xref
    (setq xref-show-definitions-function #'consult-xref
          xref-show-xrefs-function       #'consult-xref
          xref-search-program             'ripgrep)))

;; (use-package consult-eglot
;;   :after consult eglot
;;   ;; :bind (:map eglot-mode-map
;;   ;; 	 ("C-M-." . consult-eglot-symbols))
;;   ;; FIXME bind isn't binding
;;   :config
;;   (define-key eglot-mode-map [remap xref-find-apropos] 'consult-eglot-symbols))

(use-package affe
  :disabled t ; package is currently broken
  :bind
  (("C-x M-f" . (lambda () (affe-find "~/"))))
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  ;; Orderless as regex transformer
  (defun cory/affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'cory/affe-orderless-regexp-compiler))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-prefix 1)            ; Enable auto completion
  (corfu-auto-delay 0.0)           ; Enable auto completion
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-quit-at-boundary nil)
  (corfu-echo-documentation t)     ; Enable auto documentation in the minibuffer
  ;; (corfu-preview-current 'insert)  ; Insert preview of candidate when selected
  ;; (corfu-preselect-first nil)
  (corfu-preview-current nil)
  (corfu-preselect-first t)
  (corfu-popupinfo-max-width 60)
  (corfu-popupinfo-max-height 30)
  (corfu-popupinfo-delay nil)

  ;; :custom-face
  ;; (corfu-popupinfo ((t (:height 'unspecified))))

  :init
  ;; Need to recreate the map in order to preserve movement keys
  ;; Don't touch my movement keys!!
  ;; TAB cycles through completion options
  (setq corfu-map
	(let ((map (make-sparse-keymap)))
	  (define-key map [remap completion-at-point] #'corfu-complete)
	  (define-key map [remap keyboard-escape-quit] #'corfu-quit)
	  (define-key map [remap keyboard-quit] #'corfu-quit)
	  (define-key map [tab] #'corfu-next)
	  (define-key map [backtab] #'corfu-previous)
	  (define-key map (kbd "TAB") #'corfu-next)
	  (define-key map (kbd "S-TAB") #'corfu-previous)
	  (define-key map [return] #'corfu-insert)
	  (define-key map (kbd "<return>") #'corfu-insert)
	  (define-key map (kbd "M-.") #'corfu-info-location)
	  (define-key map (kbd "M-SPC") #'corfu-insert-separator)
	  map))

  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  :config
  ;; Fix corfu popup height
  (set-face-attribute 'corfu-popupinfo nil :height 'unspecified)
  ;; Deprecated face
  (set-face-attribute 'corfu-deprecated nil :foreground "magenta" :inherit nil)
  ;; Enable Corfu completion for commands like M-: (eval-expression) or M-!
  ;; (shell-command)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Completion at point extensions
(use-package cape
  :ensure t
  :custom
  (cape-dict-file "~/.local/share/dict/words")
  ;; :bind
  ;; (("C-c p i" . cape-ispell)
  ;;  ("C-c p w" . cape-dict)
  ;;  ("C-c p d" . cape-dabbrev)
  ;;  ("C-c p l" . cape-line))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)

  :config
  ;; (setq-local completion-at-point-functions
  ;;             (list (cape-super-capf #'cape-dabbrev #'cape-dict #'cape-keyword #'cape-symbol)))

  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;; Icons for corfu

(defvar kind-all-the-icons--cache nil
  "The cache of styled and padded label (text or icon).
An alist.")

(defun kind-all-the-icons-reset-cache ()
  "Remove all cached icons from `kind-all-the-icons-mapping'."
  (interactive)
  (setq kind-all-the-icons--cache nil))

(defun kind-all-the-icons--set-default-clear-cache (&rest args)
  (kind-all-the-icons-reset-cache)
  (apply #'set-default args))

(defvar kind-all-the-icons--icons
  `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
    (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
    (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
    (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
    (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
    (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
    (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
    (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
    (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
    (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
    (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
    (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
    (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
    (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
    (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
    (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
    (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
    (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
    (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
    (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
    (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
    (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
    (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
    (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))


(defsubst kind-all-the-icons--metadata-get (metadata type-name)
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defun kind-all-the-icons-formatted (kind)
  "Format icon kind with all-the-icons"
  (or (alist-get kind kind-all-the-icons--cache)
      (let ((map (assq kind kind-all-the-icons--icons)))
	(let*  ((icon (if map
                          (cdr map)
			(cdr (assq t kind-all-the-icons--icons))))
		(half (/ (default-font-width) 2))
		(pad (propertize " " 'display `(space :width (,half))))
		(disp (concat pad icon pad)))
          (setf (alist-get kind kind-all-the-icons--cache) disp)
          disp))))

(defun kind-all-the-icons-margin-formatter (metadata)
  "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
  (if-let ((kind-func (kind-all-the-icons--metadata-get metadata "company-kind")))
      (lambda (cand)
	(if-let ((kind (funcall kind-func cand)))
	    (kind-all-the-icons-formatted kind)
	  (kind-all-the-icons-formatted t))))) ;; as a backup

(add-to-list 'corfu-margin-formatters
	     #'kind-all-the-icons-margin-formatter)
