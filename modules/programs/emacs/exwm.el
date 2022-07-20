;;; exwm.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



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
  (add-hook 'exwm-init-hook #'mb/exwm-init)

  ;; Fix buffer names.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Use the page title in the case of Firefox.
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (pcase exwm-class-name
                ("Firefox" (progn
                             (exwm-workspace-rename-buffer (format "%s" exwm-title))
                             (mb/update-polybar-exwm))))))

  ;; Manipulate windows as they're created.
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              ;; Configure per-class.
              (mb/setup-window-by-class)

              ;; Switch to the EXWM modeline format.
              ;; (setq-local hide-mode-line-format (doom-modeline-format--exwm))
              (exwm-layout-hide-mode-line)))

  ;; Hide the modeline just on floating X windows.
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

  ;; Show `exwm' buffers in buffer switching prompts.
  (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Make C-u evil again.
  (add-hook 'exwm-mode-hook
            (lambda ()
              (evil-local-set-key 'motion (kbd "C-u") nil)))

  ;; Restore window configurations involving EXWM buffers by only changing names
  ;; of visible buffers. This fixes Pesp+EXWM.
  (advice-add #'exwm--update-utf8-title :around #'mb/exwm-update-utf8-title)

  ;;
  ;; Keys.
  ;;
  ;; These keys should always pass through to Emacs when in line mode.
  (setq exwm-input-prefix-keys
        `,@(mapcar (lambda (vector) (aref vector 0))
                   `(,@(mapcar (lambda (i) (kbd (format "s-%s" i)))
                               (number-sequence 0 9)) ;; Pass s-[0-9] through.
                     ,(kbd "C-h")
                     ,(kbd "C-w")
                     ,(kbd "C-g")
                     ,(kbd "C-SPC")
                     ,(kbd "M-x")
                     ,(kbd "M-`")
                     ,(kbd "M-&")
                     ,(kbd "M-:")
                     ,(kbd "s-,")
                     ,(kbd "s-$")
                     ,(kbd "s-.")
                     ,(kbd "s-;")
                     ,(kbd "s-/")
                     ,(kbd "s-g"))))

  ;; Ctrl+Q will enable the next key to be sent directly.
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Configure global key bindings.
  (setq exwm-input-global-keys
        `(([?\s-i] . exwm-input-toggle-keyboard)
          ([?\s-F] . exwm-layout-toggle-fullscreen)
          ([?\s-$] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))))
  ;; ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
  ;; ([?\s-/] . exwm-workspace-switch)

  ;; Configure line-mode simulation key bindings.
  (setq exwm-input-simulation-keys
        `(;; Add some macOSisms for compatability when in a macOS hosted VM.
          (,(kbd "s-a") . ,(kbd "C-a"))
          (,(kbd "s-c") . ,(kbd "C-c"))
          (,(kbd "s-f") . ,(kbd "C-f"))
          (,(kbd "s-k") . ,(kbd "C-k"))
          (,(kbd "s-l") . ,(kbd "C-l"))
          (,(kbd "s-P") . ,(kbd "C-P"))
          (,(kbd "s-t") . ,(kbd "C-t"))
          (,(kbd "s-T") . ,(kbd "C-T"))
          (,(kbd "s-v") . ,(kbd "C-v"))
          (,(kbd "s-w") . ,(kbd "C-w"))
          (,(kbd "s-x") . ,(kbd "C-x"))
          (,(kbd "s-N") . ,(kbd "C-N"))
          (,(kbd "s-P") . ,(kbd "C-P"))
          (,(kbd "s-<backspace>") . ,(kbd "C-<backspace>"))
          (,(kbd "M-<left>") . ,(kbd "C-<left>"))
          (,(kbd "M-<right>") . ,(kbd "C-<right>"))
          (,(kbd "s-<left>") . ,(kbd "C-<left>"))
          (,(kbd "s-<right>") . ,(kbd "C-<right>"))))

  ;; Set local simulation keys for Firefox.
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (when (and exwm-class-name
                       (string= exwm-class-name "Firefox"))
                (exwm-input-set-local-simulation-keys
                 `(,@exwm-input-simulation-keys
                   ;; Allow Emacs double C-c|w chord to send a C-c|w in Firefox.
                   ([?\C-c ?\C-c] . ?\C-c)
                   ([?\C-w ?\C-w] . ?\C-w))))))

  ;; Make Doom's leader work.
  (exwm-input-set-key (kbd doom-leader-alt-key) doom-leader-map)

  ;; Bind a default XRandr toggle for jigging displays.
  (exwm-input-set-key (kbd "<XF86Display>")
                      (lambda () (interactive) (start-process-shell-command
                                           "xrandr" nil  "xrandr --auto")))

  ;; Rofi-styled launcher.
  ;; (setq counsel-linux-app-format-function ;; Make the launcher list pretty.
  ;;       #'counsel-linux-app-format-function-name-pretty)
  ;; (exwm-input-set-key (kbd "s-SPC") #'counsel-linux-app)
  (exwm-input-set-key (kbd "s-SPC") #'app-launcher-run-app)

  ;; Try to fix C-click.
  ;; (exwm-input-set-key (kbd "<s-mouse-1>") #'fake-C-down-mouse-1)

  ;; Familiar macOS close behaviour.
  (exwm-input-set-key (kbd "s-q") #'kill-current-buffer)

  ;; Familiar macOS tabbing behaviour.
  (exwm-input-set-key (kbd "s-<tab>") #'mb/switch-to-last-buffer)

  ;; Change orientation of frames.
  (exwm-input-set-key (kbd "S-s-SPC") #'transpose-frame)

  ;; Pop to Firefox.
  (exwm-input-set-key (kbd "s-<return>")
                      (lambda () (interactive) (mb/raise-or-run "Firefox" "firefox")))
  (exwm-input-set-key (kbd "S-s-<return>")
                      (lambda () (interactive) (mb/split-with-browser)))

  ;; Allow resizing with mouse, of non-floating windows.
  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)

  ;; Emacs desktop management for non-VM based NixOS.
  (require 'desktop-environment)
  (desktop-environment-mode)
  (setq desktop-environment-brightness-set-command "light %s")
  (setq desktop-environment-brightness-normal-decrement "-U 10")
  (setq desktop-environment-brightness-small-decrement "-U 5")
  (setq desktop-environment-brightness-normal-increment "-A 10")
  (setq desktop-environment-brightness-small-increment "-A 5")
  (setq desktop-environment-brightness-get-command "light")
  (setq desktop-environment-brightness-get-regexp "\\([0-9]+\\)\\.[0-9]+")
  (setq desktop-environment-screenlock-command "loginctl lock-session")
  (setq desktop-environment-screenshot-command "flameshot gui")

  ;; TODO: Emacs notification mode? https://github.com/sinic/ednc

  ;; Use EXWM randr for setting external monitors correctly.
  (require 'exwm-randr)
  (add-hook 'exwm-randr-screen-change-hook #'mb/screen-switch)
  (exwm-randr-enable)

  ;; Enable the window manager.
  (exwm-enable))

;; Use the `ido' configuration for a few configuration fixes that alter
;; 'C-x b' worksplace switching behaviour. This also affects the functionality
;; of 'SPC .' file searching in doom regardless of the users `ido' configuration.
(use-package! exwm-config
	      :after (exwm)
	      :config
	      (exwm-config--fix/ido-buffer-window-other-frame))

;; Configure `exwm-edit' to allow editing Firefox/Slack/etc. input fields in
;; Emacs buffers.
(use-package! exwm-edit
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
