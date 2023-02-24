;;; aside-dired.el --- A global Dired dropdown buffer -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(require 'rx)
(require 'dired)

(defun aside--dired ()
  "Create a dired buffer if necessary, and display it."
  (let ((dir (dired default-directory)))
    (with-current-buffer dir
      (select-window (display-buffer dir)))))

(aside-define-configuration dired
  (rx (seq (one-or-more nonl)))
  '((side . bottom)
    (slot . -40)
    (window-height . 40))
  '(aside-hook-disable-display-line-numbers-mode)
  nil
  #'aside--dired)

(aside-enable-configuration 'dired)
(define-key global-map (kbd "C-/") 'aside-dired-dwim)

;; (provide 'aside-dired)

;;; aside-dired.el ends here
