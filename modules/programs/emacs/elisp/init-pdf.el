;; PDFs
(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-f") #'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

  ;; Minimak binds
  (define-key pdf-view-mode-map (kbd "C-n") #'image-scroll-up)
  (define-key pdf-view-mode-map (kbd "C-p") nil)
  (define-key pdf-view-mode-map (kbd "C-s") nil)
  (define-key pdf-view-mode-map (kbd "b") nil)
  (define-key pdf-view-mode-map (kbd "j") #'image-previous-frame)
  (define-key pdf-view-mode-map (kbd "f") nil)
  (define-key pdf-view-mode-map (kbd "l") #'image-next-frame)
  (define-key pdf-view-mode-map (kbd "n") nil)
  (define-key pdf-view-mode-map (kbd "e") #'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "p") nil)
  (define-key pdf-view-mode-map (kbd "i") #'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "w") nil)
  (define-key pdf-view-mode-map (kbd "c") #'image-mode-copy-file-name-as-kill))
