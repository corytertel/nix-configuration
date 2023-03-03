;;
;; --- DEFAULT COMPLETION ---
;;

;; Icons in minibuffer
(use-package all-the-icons-completion
  :disabled t
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; Completion matching
(use-package orderless
  :ensure t
  :custom
  ;; (completion-styles '(orderless flex))
  (completion-styles '(emacs21 orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator "[ \\]"))

;; Minibuffer visual menu
(use-package consult
  :init
  (setq consult-preview-key nil)
  :bind
  (("C-c f"       . consult-recent-file)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap kmacro-end-or-call-macro-repeat] . consult-kmacro)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ([remap occur] . consult-focus-lines)
   ([remap pop-global-mark] . consult-global-mark)
   ([remap imenu] . consult-imenu)
   ([remap repeat-complex-command] . consult-complex-command)
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

;;
;; --- EXPANSION ---
;;

;; (use-package hippie-exp
;;   :ensure nil
;;   :bind
;;   ([remap dabbrev-expand] . hippie-expand)
;;   :commands (hippie-expand)
;;   :config
;;   (setq hippie-expand-try-functions-list
;;         '(try-expand-dabbrev
;;           try-expand-dabbrev-all-buffers
;;           try-expand-dabbrev-from-kill
;;           try-complete-lisp-symbol-partially
;;           try-complete-lisp-symbol
;;           try-complete-file-name-partially
;;           try-complete-file-name
;;           try-expand-all-abbrevs
;;           try-expand-list
;;           try-expand-line)))

;;
;; --- CODE COMPLETION ---
;;

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-prefix 1)            ; Enable auto completion
  (corfu-auto-delay 0.0)           ; Enable auto completion
  (corfu-quit-at-boundary t)
  (corfu-echo-documentation t)     ; Enable auto documentation in the minibuffer
  ;; (corfu-preview-current 'insert)  ; Insert preview of candidate when selected
  ;; (corfu-preselect-first nil)
  (corfu-preview-current nil)
  (corfu-preselect-first t)
  (corfu-popupinfo-max-width 60)
  (corfu-popupinfo-max-height 30)
  (corfu-popupinfo-delay nil)

  :init
  ;; Need to recreate the map in order to preserve movement keys
  ;; Don't touch my movement keys!!
  ;; TAB cycles through completion options
  (setq corfu-map
	(let ((map (make-sparse-keymap)))
	  (define-key map [remap completion-at-point] #'corfu-complete)
	  (define-key map [remap keyboard-escape-quit] #'corfu-quit)
	  (define-key map (kbd "C-g") #'corfu-quit)
	  (define-key map [down] #'corfu-next)
	  (define-key map [up] #'corfu-previous)
	  (define-key map [tab] #'corfu-next)
	  (define-key map [backtab] #'corfu-previous)
	  (define-key map (kbd "TAB") #'corfu-next)
	  (define-key map (kbd "S-TAB") #'corfu-previous)
	  ;; (define-key map [(shift return)] #'corfu-insert)
	  ;; (define-key map (kbd "S-<return>") #'corfu-insert)
	  (define-key map [return] #'corfu-insert)
	  (define-key map (kbd "<return>") #'corfu-insert)
	  (define-key map (kbd "M-l") #'corfu-info-location)
	  (define-key map (kbd "M-SPC") #'corfu-insert-separator)
	  ;; (define-key map [space] #'cory/corfu-insert-with-space)
	  ;; (define-key map (kbd "SPC") #'cory/corfu-insert-with-space)
	  map))

  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

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

;; (use-package company
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (setq company-idle-delay 0.0)
;;   (setq company-tooltip-minimum-width 60)
;;   (setq company-tooltip-maximum-width 60)
;;   (setq company-tooltip-limit 7)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
;;                             company-echo-metadata-frontend))
;;   (unless (display-graphic-p)
;;     (define-key company-active-map (kbd "C-h") #'backward-kill-word)
;;     (define-key company-active-map (kbd "C-w") #'backward-kill-word))
;;   (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous)
;;   (if (display-graphic-p)
;;       (define-key company-active-map (kbd "<tab>") 'company-select-next)
;;     (define-key company-active-map (kbd "TAB") 'company-select-next))
;;   (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

;; (use-package company-box
;;   :if (display-graphic-p)
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-doc-enable nil)
;;   (setq company-box-scrollbar nil)
;;   (setq company-box-frame-behavior 'default))

;; ;; Autocompletion
;; (use-package company
;;   :diminish company-mode
;;   :bind
;;   (:map company-active-map
;;    ([return] . nil)
;;    ("RET" . nil)
;;    ("TAB" . company-complete-selection)
;;    ([tab] . company-complete-selection)
;;    ("C-f" . company-complete-selection)
;;    ("S-TAB" . company-select-previous)
;;    ([backtab] . company-select-previous)
;;    ("C-n" . company-select-next)
;;    ("C-p" . company-select-previous))
;;   ;; (:map lsp-mode-map
;;   ;;  ("<tab>" . company-indent-or-complete-common))
;;   :config
;;   (setq company-idle-delay 0.0
;;         company-minimum-prefix-length 1
;;         company-show-numbers t
;;         company-tooltip-maximum-width 100
;;         company-tooltip-minimum-width 20
;; 	;; Allow me to keep typing even if company disapproves.
;;         company-require-match nil)
;;   (global-company-mode))

;; TODO figure out import order
;; (use-package company-emoji
;;   :config (add-to-list 'company-backends 'company-emoji))

;; (use-package company-quickhelp
;;   :config (company-quickhelp-mode))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-icons-alist 'company-box-icons-all-the-icons))
