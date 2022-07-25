;;; smart-mode-line-cory-theme.el --- Cory's theme for smart-mode-line
;;; Commentary:
;;; Code:

(deftheme smart-mode-line-cory
  "Cory theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-cory
 '(mode-line-inactive ((t :inverse-video nil)))
 '(mode-line     ((t :inverse-video nil)))
 '(sml/global    ((t :inherit font-lock-comment-face)))
 '(sml/filename  ((t :inherit mode-line-buffer-id)))
 '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
 '(sml/read-only ((t :inherit (font-lock-type-face sml/not-modified))))
 '(sml/modes     ((t :foreground nil :inherit sml/filename :weight normal))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-cory)

;;; smart-mode-line-cory-theme.el ends here.
