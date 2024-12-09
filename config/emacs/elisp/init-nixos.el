;;
;; --- NIXOS ---
;;

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook
  (nix-mode . subword-mode)
  (nix-mode . nix-prettify-mode))

(defvar cory/nixos-config-location "/home/cory/.config/nix")

(defun cory/nixos-flake-rebuild ()
  "Rebuilds a NixOS system."
  (interactive)
  (async-shell-command
   (concat "nixos-rebuild switch --flake "
	   cory/nixos-config-location
	   "#"
	   (completing-read "System name:" '("pc" "laptop"))
	   " --use-remote-sudo")))

(defun cory/nix-collect-garbage ()
  "Collects the garbage made by the Nix package manager."
  (interactive)
  (async-shell-command
   (concat "sudo nix-collect-garbage --delete-older-than "
	   (let ((time (completing-read "Delete files older than:" '("7 days" "3 days" "1 day"))))
	     (cond
	      ((string= time "7 days") "7d")
	      ((string= time "3 days") "3d")
	      ((string= time "1 day") "1d")
	      (t (error "Not a valid time")))))))

;; (global-set-key (kbd "C-c n") #'cory/nixos-flake-rebuild)
;; (global-set-key (kbd "C-c N") #'cory/nix-collect-garbage)

;; ;; This package adds a significant amount of time to emacs startup
;; (use-package nixos-options
;;   :config
;;   ;; FIXME the first completing-read returns a string, not the list item we want
;;   (defun cory/nixos-options ()
;;     (interactive)
;;     (let* ((opt (completing-read "NixOS Options:" nixos-options))
;; 	   (act (completing-read opt
;; 				 '("View documentation"
;; 				   "Insert into buffer"
;; 				   "Show the description"
;; 				   "Copy"))))
;;       (cond
;;        ((string-equal act "View documentation")
;; 	(let ((buf (get-buffer-create "*nixos-options-doc*")))
;; 	  (with-current-buffer buf
;; 	    (view-mode -1)
;; 	    (erase-buffer)
;; 	    (insert (nixos-options-get-documentation-for-option opt))
;; 	    (goto-char (point-min))
;; 	    (view-mode 1))
;; 	  (switch-to-buffer-other-window buf)))
;;        ((string-equal act "Insert into buffer")
;; 	(insert (nixos-options-get-name opt)))
;;        ((string-equal act "Show the description")
;; 	(let ((buf (get-buffer-create "*nixos-options-doc*")))
;; 	  (with-current-buffer buf
;; 	    (view-mode -1)
;; 	    (erase-buffer)
;; 	    (insert (message (format
;; 			      "%s: %s"
;; 			      (car opt)
;; 			      (nixos-options-get-description opt))))
;; 	    (goto-char (point-min))
;; 	    (view-mode 1))
;; 	  (switch-to-buffer-other-window buf)))
;;        ((string-equal act "Copy")
;; 	(kill-new (nixos-options-get-name opt)))))))
