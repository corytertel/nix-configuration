
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

(use-package golden-ratio)

;;; Install applications to be used in emacs because emacs is no longer intergrates with the os.

;; Browser for YouTube in emacs
;; https://github.com/emacs-eaf/emacs-application-framework/wiki/NixOS
(require 'eaf)
(require 'eaf-browser)

(defun youtube ()
  (interactive)
  (eaf-open-browser "https://youtube.com/"))

;; Pdf viewer
