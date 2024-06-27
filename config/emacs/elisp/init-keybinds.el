;;; Keybinding Fix

(define-key function-key-map
  [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map
  [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map
  [(meta control shift iso-lefttab)] [(meta control shift tab)])

;;; VIPER

;; (setq viper-mode t
;;       viper-inhibit-startup-message t
;;       viper-expert-level 5
;;       viper-want-ctl-h-help t
;;       ;; viper-ex-style-motion nil ; allow l,h to cross line boundaries
;;       )
;; (require 'viper)

;; ;; (setq-default inhibit-startup-screen t) does not work
;; (defun display-startup-screen (&optional concise)
;;   (ignore))

;; ;; Relative line numbers
;; (display-line-numbers-mode)
;; (setq display-line-numbers 'relative)

;;; Evil

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-tree)
  (evil-move-beyond-eol t)
  :config
  (evil-mode 1)

  ;; Relative line numbers
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers 'relative)

  ;; Cursor
  (setq evil-default-cursor 'hollow)

  ;; Enable C-[
  (define-key evil-insert-state-map (kbd "C-[") #'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-[") #'evil-normal-state)

  ;; Make tab work the way it does in emacs
  (define-key evil-motion-state-map (kbd "TAB") #'indent-for-tab-command)

  ;; Make :q not quit emacs
  (global-set-key [remap evil-quit] 'kill-buffer-and-window)

  ;; Make : trigger M-x instead
  (define-key evil-motion-state-map (kbd ":") #'execute-extended-command)

  ;; Define aliases so M-x works like :
  (defalias 'q #'kill-buffer-and-window)
  (defalias 'wq #'cory/kill-buffer-and-window-and-save)
  (defalias 'w #'save-buffer)

  ;; Make M-x look similar to :
  (setq extended-command-versions
        (list (list ":" (lambda () read-extended-command-predicate))
              (list "M-X " #'command-completion--command-for-this-buffer-function)))

  ;; Make M-x have the line navigation of :
  ;; (read-extended-command ":")

  )

;;; Use devil as "leader key"

(use-package devil
  :after evil
  :bind
  (:map evil-motion-state-map
   ("SPC" . devil)
   ("C-h k" . devil-helpful-key))
  :config
  (defun devil-helpful-key ()
    "Describe a Devil key sequence with helpful."
    (interactive)
    (devil--log "Activated with %s" (key-description (this-command-keys)))
    (let* ((result (devil--read-key devil-describe-prompt (vector)))
           (key (devil--aget 'key result))
           (translated-key (devil--aget 'translated-key result))
           (binding (devil--aget 'binding result)))
      (devil--log "Read key: %s => %s => %s => %s"
                  key (key-description key) translated-key binding)
      (if translated-key
          (helpful-key (kbd translated-key))
  	;; Create a transient keymap to describe special key sequence.
  	(let* ((virtual-keymap (make-sparse-keymap))
               (exit-function (set-transient-map virtual-keymap)))
          (define-key virtual-keymap key binding)
          (helpful-key key)
          (funcall exit-function)))))
  :custom
  (devil-key " ")
  (devil-repeatable-keys nil)
  (devil-global-sets-buffer-default t)
  (devil-translations
   '(("%k %k %k" . "C-M-")
     ("%k %k" . "M-")
     ("%k" . "C-")))
  (devil-special-keys
   '(("%k %k %k %k" . ignore)
     ("%k <escape>" . ignore)
     ("%k %k <escape>" . ignore)
     ("%k %k %k <escape>" . ignore))))

;;; Vim training

;; (use-package evil-motion-trainer
;;   :after evil
;;   :config
;;   (global-evil-motion-trainer-mode 1)
;;   (setq evil-motion-trainer-threshold 5))

;;; Nice to have Keybinds

(cory/define-keys
 global-map
 ("C-x k" . kill-this-buffer)
 ("C-x K" . kill-buffer)
 ("C-x C-b" . ibuffer)
 ("C-c w" . woman)
 ("C-'"   . repeat)
 ("C-<f4>" . kill-this-buffer))

(cory/define-keys
 evil-motion-state-map
 ("C-b" . switch-to-buffer))

(global-set-key (kbd "M-f") search-map)

(cory/define-keys
 minibuffer-mode-map
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 minibuffer-local-completion-map
 ("M-s" . nil)
 ("M-f" . next-matching-history-element))

(cory/define-keys
 isearch-mode-map
 ("TAB" . isearch-complete)
 ("C-s" . nil)
 ("C-f" . isearch-repeat-forward)
 ("M-s '" . nil)
 ("M-s C-e" . nil)
 ("M-s M-<" . nil)
 ("M-s M->" . nil)
 ("M-s SPC" . nil)
 ("M-s _" . nil)
 ("M-s c" . nil)
 ("M-s e" . nil)
 ("M-s h l" . nil)
 ("M-s h r" . nil)
 ("M-s i" . nil)
 ("M-s o" . nil)
 ("M-s r" . nil)
 ("M-s w" . nil)
 ("M-f '" . isearch-toggle-char-fold)
 ("M-f C-e" . isearch-yank-line)
 ("M-f M-<" . isearch-beginning-of-buffer)
 ("M-f M->" . isearch-end-of-buffer)
 ("M-f SPC" . isearch-toggle-lax-whitespace)
 ("M-f _" . isearch-toggle-symbol)
 ("M-f c" . isearch-toggle-case-fold)
 ("M-f e" . isearch-edit-string)
 ("M-f h l" . isearch-highlight-lines-matching-regexp)
 ("M-f h r" . isearch-highlight-regexp)
 ("M-f i" . isearch-toggle-invisible)
 ("M-f o" . isearch-occur)
 ("M-f r" . isearch-toggle-regexp)
 ("M-f w" . isearch-toggle-word))

;;; CUA Compatibility

(cua-mode 1)

(cory/define-keys
 global-map
 ("C-o" . find-file)
 ("C-s" . save-buffer)
 ("C-S-s" . write-file))

(cory/define-keys
 evil-motion-state-map
 ("C-o" . find-file)
 ("C-f" . isearch-forward))

;;; Scroll Keybinds

(global-set-key (kbd "C-<mouse-4>") #'ignore)
(global-set-key (kbd "C-<mouse-5>") #'ignore)
(global-set-key (kbd "C-<wheel-down>") #'ignore)
(global-set-key (kbd "C-<wheel-up>") #'ignore)
(global-set-key (kbd "C-M-<mouse-4>") #'ignore)
(global-set-key (kbd "C-M-<mouse-5>") #'ignore)
(global-set-key (kbd "C-M-<wheel-down>") #'ignore)
(global-set-key (kbd "C-M-<wheel-up>") #'ignore)

;;; Escape as keyboard-quit

(define-key global-map [escape] #'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
