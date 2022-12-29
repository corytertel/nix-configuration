;;; aside-eshell.el --- A shared global Eshell buffer -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(require 'rx)
(require 'eshell)

(defcustom aside-eshell-buffer-name
  "*Aside-Eshell*"
  "The name for the single, shared, global Aside-Eshell buffer."
  :group 'aside-eshell
  :type 'string
  :set 'aside-configuration-setter-function)

(defun aside--eshell ()
  "Create an Aside-Eshel buffer if necessary, and display it."
  (let ((buffer (get-buffer-create aside-eshell-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'eshell-mode)
        (setq default-directory (expand-file-name "~"))
        (eshell-mode))
      (select-window (display-buffer buffer)))))

(aside-define-configuration eshell
  (rx (literal aside-eshell-buffer-name))
  '((side . bottom)
    (slot . -40)
    (window-height . 40))
  '(aside-hook-enable-truncate-lines
    aside-hook-disable-display-line-numbers-mode)
  nil
  #'aside--eshell)

(aside-enable-configuration 'eshell)
(define-key global-map (kbd "C-`") 'aside-eshell-dwim)

;; (provide 'aside-eshell)

;;; aside-eshell.el ends here
