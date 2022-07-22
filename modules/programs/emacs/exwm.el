;;; exwm.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun exwm-init ()
  "Ran when exwm starts."
  (run-at-time "1 sec" nil
	       (lambda () (start-process-shell-command
		      "feh" nil  "feh --bg-fill ~/.config/wallpaper"))))

(defun switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun mb/buffer-with-suffix (suffix)
  "Locates the buffer matching the `suffix'."
  (cl-find-if (lambda (buffer)
                (string-suffix-p suffix (buffer-name buffer))) (buffer-list)))

;; Configure EXWM.
(use-package exwm
  :init
  (setq
   ;; Use the primary clipboard.
   select-enable-primary t
   ;; Follow the mouse.
   focus-follows-mouse t
   ;; Move the focus to the followed window.
   mouse-autoselect-window t
   ;; Warp the cursor automatically after workspace switches.
   exwm-workspace-warp-cursor t
   ;; Start with a single workspace.
   exwm-workspace-number 1
   ;; But show buffers on other workspaces.
   exwm-workspace-show-all-buffers t
   ;; And allow switching to buffers on other workspaces.
   exwm-layout-show-all-buffers t)
  :config
  ;; Hooks.
  ;;
  ;; Do some extra configuration after EXWM starts.
  (add-hook 'exwm-init-hook #'exwm-init)

  ;; Fix buffer names.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Hide the modeline just on floating X windows.
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

  ;;
  ;; Keys.
  ;;

  ;; ;; These keys should always pass through to Emacs when in line mode.
  ;; (setq exwm-input-prefix-keys
  ;;       `,@(mapcar (lambda (vector) (aref vector 0))
  ;;                  `(,@(mapcar (lambda (i) (kbd (format "s-%s" i)))
  ;;                              (number-sequence 0 9)) ;; Pass s-[0-9] through.
  ;;                    ,(kbd "C-h")
  ;;                    ,(kbd "C-w")
  ;;                    ,(kbd "C-g")
  ;;                    ,(kbd "C-SPC")
  ;;                    ,(kbd "M-x")
  ;;                    ,(kbd "M-`")
  ;;                    ,(kbd "M-&")
  ;;                    ,(kbd "M-:")
  ;;                    ,(kbd "s-,")
  ;;                    ,(kbd "s-$")
  ;;                    ,(kbd "s-.")
  ;;                    ,(kbd "s-;")
  ;;                    ,(kbd "s-/")
  ;;                    ,(kbd "s-g"))))

  ;; ;; Ctrl+Q will enable the next key to be sent directly.
  ;; (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; ;; Configure global key bindings.
  ;; (setq exwm-input-global-keys
  ;;       `(([?\s-i] . exwm-input-toggle-keyboard)
  ;;         ([?\s-F] . exwm-layout-toggle-fullscreen)
  ;;         ([?\s-$] . (lambda (command)
  ;;                      (interactive (list (read-shell-command "$ ")))
  ;;                      (start-process-shell-command command nil command)))))
  ;; ;; ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
  ;; ;; ([?\s-/] . exwm-workspace-switch)

  ;; ;; Configure line-mode simulation key bindings.
  ;; (setq exwm-input-simulation-keys
  ;;       `(;; Add some macOSisms for compatability when in a macOS hosted VM.
  ;;         (,(kbd "s-a") . ,(kbd "C-a"))
  ;;         (,(kbd "s-c") . ,(kbd "C-c"))
  ;;         (,(kbd "s-f") . ,(kbd "C-f"))
  ;;         (,(kbd "s-k") . ,(kbd "C-k"))
  ;;         (,(kbd "s-l") . ,(kbd "C-l"))
  ;;         (,(kbd "s-P") . ,(kbd "C-P"))
  ;;         (,(kbd "s-t") . ,(kbd "C-t"))
  ;;         (,(kbd "s-T") . ,(kbd "C-T"))
  ;;         (,(kbd "s-v") . ,(kbd "C-v"))
  ;;         (,(kbd "s-w") . ,(kbd "C-w"))
  ;;         (,(kbd "s-x") . ,(kbd "C-x"))
  ;;         (,(kbd "s-N") . ,(kbd "C-N"))
  ;;         (,(kbd "s-P") . ,(kbd "C-P"))
  ;;         (,(kbd "s-<backspace>") . ,(kbd "C-<backspace>"))
  ;;         (,(kbd "M-<left>") . ,(kbd "C-<left>"))
  ;;         (,(kbd "M-<right>") . ,(kbd "C-<right>"))
  ;;         (,(kbd "s-<left>") . ,(kbd "C-<left>"))
  ;;         (,(kbd "s-<right>") . ,(kbd "C-<right>"))))

  ;; ;; Set local simulation keys for Firefox.
  ;; (add-hook 'exwm-manage-finish-hook
  ;;           (lambda ()
  ;;             (when (and exwm-class-name
  ;;                      (string= exwm-class-name "Firefox"))
  ;;               (exwm-input-set-local-simulation-keys
  ;;                `(,@exwm-input-simulation-keys
  ;;                  ;; Allow Emacs double C-c|w chord to send a C-c|w in Firefox.
  ;;                  ([?\C-c ?\C-c] . ?\C-c)
  ;;                  ([?\C-w ?\C-w] . ?\C-w))))))

  ;; Try to fix C-click.
  ;; (exwm-input-set-key (kbd "<s-mouse-1>") #'fake-C-down-mouse-1)

  ;; Close buffer
  (exwm-input-set-key (kbd "C-<escape>") #'kill-current-buffer)

  ;; Tab through buffers
  (exwm-input-set-key (kbd "C-<tab>") #'switch-to-last-buffer)

  ;; Change orientation of frames.
  (exwm-input-set-key (kbd "S-s-SPC") #'transpose-frame)

  ;; Allow resizing with mouse, of non-floating windows.
  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)

  ;; TODO: Emacs notification mode? https://github.com/sinic/ednc

  ;; Enable the window manager.
  (exwm-enable))

;; Configure `exwm-edit' to allow editing Firefox/Slack/etc. input fields in
;; Emacs buffers.
(use-package exwm-edit
	      :after (exwm)
	      :init
	      ;; Seems to fix things for my slow macOS-hosted NixOS VM.
	      (setq exwm-edit-clean-kill-ring-delay 0.5)
	      ;; Pop the edit buffer below.
	      (set-popup-rule! "^\\*exwm-edit"
			       :side 'bottom :size 0.2
			       :select t :quit nil :ttl t)
	      :config
	      (defalias 'exwm-edit--display-buffer 'pop-to-buffer)
	      ;; Use GFM mode in the edit buffers.
	      (add-hook 'exwm-edit-compose-hook (lambda () (funcall 'gfm-mode))))

(use-package desktop-environment
  :config
  (desktop-environment-mode)
  (setq desktop-environment-brightness-set-command "light %s")
  (setq desktop-environment-brightness-normal-decrement "-U 10")
  (setq desktop-environment-brightness-small-decrement "-U 5")
  (setq desktop-environment-brightness-normal-increment "-A 10")
  (setq desktop-environment-brightness-small-increment "-A 5")
  (setq desktop-environment-brightness-get-command "light")
  (setq desktop-environment-brightness-get-regexp "\\([0-9]+\\)\\.[0-9]+")
  (setq desktop-environment-screenlock-command "loginctl lock-session")
  (setq desktop-environment-screenshot-command "flameshot gui"))
