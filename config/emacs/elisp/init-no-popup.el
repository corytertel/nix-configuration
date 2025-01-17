
;;; Window management

(global-set-key (kbd "C-x 0") #'delete-window)
(global-set-key (kbd "C-x 1") #'delete-other-windows)
(global-set-key (kbd "C-x 2") #'split-and-follow-below)
(global-set-key (kbd "C-x 3") #'split-and-follow-right)
(global-set-key (kbd "C-x 4 q") #'kill-all-buffers-and-windows)

(global-set-key (kbd "<f2>") #'cory/other-window)
(global-set-key (kbd "C-<f2>") #'cory/other-window)
(global-set-key (kbd "M-<f2>") #'cory/other-window)
(global-set-key (kbd "C-M-<f2>") #'cory/other-window)
(global-set-key (kbd "S-<f2>") #'cory/other-window)
(global-set-key (kbd "C-S-<f2>") #'cory/other-window)
(global-set-key (kbd "M-S-<f2>") #'cory/other-window)
(global-set-key (kbd "C-M-S-<f2>") #'cory/other-window)

;; ;; Center text in window
;; ;; visual-fill-column works with golden-ratio
;; (use-package visual-fill-column
;;   :init
;;   ;; Set fringes before turning on visual-fill-column
;;   ;; Set the fringe to an big enough widthh
;;   (setq-default fringe-mode 20)
;;   (set-fringe-mode 20)
;;   :config
;;   (global-visual-fill-column-mode 1)
;;   :custom
;;   (visual-fill-column-width 100)
;;   (visual-fill-column-center-text t))

;; ;; Focus selected window
;; (use-package golden-ratio
;;   :bind
;;   (("<f7>" . cory/toggle-golden-ratio))
;;   :config
;;   ;; TODO have golden ratio on by default
;;   ;; (golden-ratio-mode 1)
;;   (defun cory/toggle-golden-ratio ()
;;     (interactive)
;;     (if golden-ratio-mode
;;         (progn
;;           (golden-ratio-mode 0)
;;           (balance-windows))
;;       (golden-ratio-mode 1)
;;       (golden-ratio))))

;; Modeline
(setq repeat-echo-function (lambda (keymap)
			     ;; (repeat-echo-message keymap)
			     (cond
			      (keymap
			       (set-face-attribute 'mode-line nil
					           :foreground "#141404"
					           :background "#ffdac0")
			       (set-face-attribute 'mode-line-inactive nil
					           :foreground "#141404"
					           :background "#ffffff"))
			      (t
			       (set-face-attribute 'mode-line nil
					           :foreground "#141404"
					           :background "#c0daff")
			       (set-face-attribute 'mode-line-inactive nil
					           :foreground "#141404"
					           :background "#ffffff")))))

;; Prefer vertical splits
;; From https://emacs.stackexchange.com/questions/39034/prefer-vertical-splits-over-horizontal-ones
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
          ;; Split window horizontally
          (with-selected-window window
            (split-window-right)))
       (and (window-splittable-p window)
          ;; Split window vertically
          (with-selected-window window
            (split-window-below)))
       (and
        ;; If WINDOW is the only usable window on its frame (it is
        ;; the only one or, not being the only one, all the other
        ;; ones are dedicated) and is not the minibuffer window, try
        ;; to split it horizontally disregarding the value of
        ;; `split-height-threshold'.
        (let ((frame (window-frame window)))
          (or
           (eq window (frame-root-window frame))
           (catch 'done
             (walk-window-tree (lambda (w)
                                 (unless (or (eq w window)
                                            (window-dedicated-p w))
                                   (throw 'done nil)))
                               frame)
             t)))
        (not (window-minibuffer-p window))
        (let ((split-width-threshold 0))
          (when (window-splittable-p window t)
            (with-selected-window window
              (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (interactive)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq split-height-threshold 4
      split-width-threshold 40
      split-window-preferred-function 'split-window-really-sensibly)

(defun cory/split-window (&optional window)
  (interactive)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (split-window-right nil window)
      (split-window-below nil window))
    (other-window 1)))

(global-set-key (kbd "C-x C-n") #'cory/split-window)

;;; Install applications to be used in emacs because emacs is no longer intergrates with the os.

;; Popup Terminal
(use-package popper
  :bind (("C-`"   . cory/toggle-eshell)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (defun cory/toggle-eshell ()
    (interactive)
    (cl-assert eshell-buffer-name)
    (unless (get-buffer eshell-buffer-name)
      (let ((buf (generate-new-buffer eshell-buffer-name)))
        (with-current-buffer eshell-buffer-name
          (eshell-mode))
        (setq popper-buried-popup-alist `((nil (nil . ,buf))))))
    (popper-toggle))

  (setq popper-reference-buffers '(eshell-mode)
        popper-window-height
        (lambda (win)
          (floor (frame-height) 2)))

  (popper-mode 1))

;; Pdf viewer
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
   ("C-n" . image-forward-hscroll)
   ("C-p" . image-scroll-up)
   ("b" . nil)
   ("f" . nil)
   ("d" . image-previous-frame)
   ("n" . image-next-frame)
   ("i" . describe-mode)
   ("h" . pdf-view-next-page-command)
   ("t" . pdf-view-previous-page-command)
   ("p" . nil))
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
  ;; ;; use normal isearch
  ;; (define-key pdf-view-mode-map (kbd "C-f") #'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; ;; keyboard shortcuts
  ;; (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  ;; (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  ;; (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  )
