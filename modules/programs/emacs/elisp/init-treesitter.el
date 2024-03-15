
(use-package tree-sitter
  :after tree-sitter-langs
  :custom-face
  (tree-sitter-hl-face:property         ((t (:slant normal))))
  (tree-sitter-hl-face:method.call      ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.call    ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:operator         ((t (:inherit default))))
  (tree-sitter-hl-face:type.builtin     ((t (:inherit font-lock-type-face))))
  (tree-sitter-hl-face:number           ((t (:inherit highlight-numbers-number))))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs)

(use-package combobulate
  :hook
  ;; These are the only modes for which combobulate exists for for now
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  :init
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c j")
  :config
  (defalias 'combobulate-backward-up-node #'combobulate-navigate-up)
  (defalias 'combobulate-down-node #'combobulate-navigate-down)
  (defalias 'combobulate-forward-node #'combobulate-navigate-next)
  (defalias 'combobulate-backward-node #'combobulate-navigate-previous)

  (defun combobulate-up-node (&optional arg)
    (interactive "^p")
    (with-argument-repetition arg
      (combobulate-visual-move-to-node (combobulate--navigate-up) t)))

  ;; TODO does not work as expected
  (defun combobulate-backward-down-node (&optional arg)
    (interactive "^p")
    (with-argument-repetition arg
      (combobulate-visual-move-to-node (combobulate--navigate-down) t)))

  (with-eval-after-load 'combobulate-navigation
    (define-key combobulate-key-map (kbd "C-M-h") #'combobulate-up-node)
    (define-key combobulate-key-map (kbd "C-M-u") #'combobulate-backward-up-node)
    (define-key combobulate-key-map (kbd "C-M-y") #'combobulate-down-node)
    (define-key combobulate-key-map (kbd "C-M-t") #'combobulate-backward-down-node)
    (define-key combobulate-key-map (kbd "C-M-i") #'combobulate-mark-defun)
    (define-key combobulate-key-map (kbd "C-M-n") #'combobulate-forward-node)
    (define-key combobulate-key-map (kbd "C-M-p") nil)
    (define-key combobulate-key-map (kbd "C-M-d") #'combobulate-backward-node)
    (define-key combobulate-key-map (kbd "C-M-b") #'combobulate-transpose-sexps)
    (define-key combobulate-key-map (kbd "M-N") #'combobulate-drag-down)
    (define-key combobulate-key-map (kbd "M-P") nil)
    (define-key combobulate-key-map (kbd "M-D") #'combobulate-drag-up)
    (define-key combobulate-key-map (kbd "M-h") nil)
    (define-key combobulate-key-map (kbd "M-i") #'combobulate-mark-node-dwim)
    (define-key combobulate-key-map (kbd "M-e") nil)
    (define-key combobulate-key-map (kbd "M-n") #'combobulate-navigate-logical-next)
    (define-key combobulate-key-map (kbd "M-a") nil)
    (define-key combobulate-key-map (kbd "M-d") #'combobulate-navigate-logical-previous)))
