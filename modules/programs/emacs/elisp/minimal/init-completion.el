;; Keep track of recent files
(recentf-mode t)

;; Recursive minibuffers
(require 'mb-depth)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; Suggest commands used before in the minibuffer first (history)
(savehist-mode t)

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
      ;; completions-max-height 16
      completion-auto-wrap t)

;; Position and format of completions window
(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-parameters . ((mode-line-format . none)))))

;; Automatic popup when minibuffer starts
(add-hook 'minibuffer-setup-hook #'minibuffer-completion-help)

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

(define-key minibuffer-local-completion-map
  (kbd "TAB") #'cory/minibuffer-complete)

(define-key minibuffer-local-completion-map
  (kbd "DEL") #'cory/kill-dir-or-char)

(define-key completion-list-mode-map
  [remap scroll-up-command] #'switch-to-minibuffer)
