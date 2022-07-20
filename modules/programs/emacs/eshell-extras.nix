{ config, pkgs }:
with config.theme.color;

''
;; Eshell prompt
(use-package eshell-prompt-extras
  :config
  (defun epe-theme-cory ()
    "Cory's prompt."
    ;; If the prompt spans over multiple lines, the regexp should match
    ;; last line only.
    (setq eshell-prompt-regexp "^╰─λ ")
    (concat
     (epe-colorize-with-face (epe-status) 'epe-status-face)
     (when (epe-remote-p)
       (epe-colorize-with-face
	(concat "(" (epe-remote-user) "@" (epe-remote-host) ")")
	'epe-remote-face))
     (when (and epe-show-python-info (bound-and-true-p venv-current-name))
       (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))
     (let ((f (cond ((eq epe-path-style 'fish) 'epe-fish-path)
                    ((eq epe-path-style 'single) 'epe-abbrev-dir-name)
                    ((eq epe-path-style 'full) 'abbreviate-file-name))))
       (pcase (epe-extract-git-component (funcall f (eshell/pwd)))
	 (`(,prefix nil)
          (format
           (propertize "╭╴%s %s" 'face '(:weight regular))
           (propertize "" 'face '(:weight bold :foreground "${color6}"))
           (propertize prefix 'face '(:weight bold :foreground "${color4}"))))
	 (`(,prefix ,git-component)
          (format
           (epe-colorize-with-face "╭╴%s %s%s %s %s" '(:weight regular))
	   (epe-colorize-with-face "" '(:weight bold :foreground "${color6}"))
           (epe-colorize-with-face prefix '(:weight bold :foreground "${color4}"))
           (if (string-empty-p git-component)
               ""
             (concat "/"
                     (epe-colorize-with-face git-component '(:weight bold :foreground "${color6}"))))
           (epe-colorize-with-face
            (concat (or (epe-git-branch)
                       (epe-git-tag))
                    (epe-git-dirty)
                    (epe-git-untracked))
            '(:weight italic :foreground "${color2}"))
	   (epe-colorize-with-face
	    (let ((unpushed (epe-git-unpushed-number)))
                      (unless (= unpushed 0)
			(concat "↑" (number-to-string unpushed))))
	    '(:foreground "${color5}"))))))
     (epe-colorize-with-face "\n╰─λ" '(:weight regular))
     " "))
  (with-eval-after-load "esh-opt"
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-cory)))

;; Eshell undistract me
;; (use-package eshell-undistract-me
;;   :config
;;   (setq eshell-undistract-me-play-sound t)
;;   (setq eshell-undistract-me-sound-path "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/complete.oga")
;;   (add-hook 'eshell-pre-command-hook #'eshell-undistract-me-pre-command)
;;   (add-hook 'eshell-before-prompt-hook #'eshell-undistract-me-before-prompt))

(setq eshell-undistract-me-play-sound t)
(setq eshell-undistract-me-sound-path "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/complete.oga")
(add-hook 'eshell-pre-command-hook #'eshell-undistract-me-pre-command)
(add-hook 'eshell-before-prompt-hook #'eshell-undistract-me-before-prompt)
''
