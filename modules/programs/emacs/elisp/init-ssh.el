;; Tramp
;; (use-package tramp
;;   :defer t
;;   :config
;;   (setq tramp-default-method "ssh")

;;   ;; Only for debugging slow tramp connections
;;   ;;(setq tramp-verbose 7)

;;   ;; Skip version control for tramp files
;;   (setq vc-ignore-dir-regexp
;;         (format "\\(%s\\)\\|\\(%s\\)"
;;                 vc-ignore-dir-regexp
;;                 tramp-file-name-regexp))

;;   ;; Use ControlPath from .ssh/config
;;   (setq tramp-ssh-controlmaster-options "")

;;   ;; Backup tramp files like local files and don't litter the remote
;;   ;; file system with my emacs backup files
;;   (setq tramp-backup-directory-alist backup-directory-alist)

;;   ;; See https://www.gnu.org/software/tramp/#Ad_002dhoc-multi_002dhops
;;   ;; For all hosts, except my local one, first connect via ssh, and then apply sudo -u root:
;;   (dolist (tramp-proxies '((nil "\\`root\\'" "/ssh:%h:")
;;                            ((regexp-quote (system-name)) nil nil)
;;                            ("localhost" nil nil)
;;                            ("blif\\.vpn" nil nil)
;;                            ("skor-pi" nil nil)
;;                            ;; Add tramp proxy for atomx user
;;                            (nil "atomx" "/ssh:%h:")))
;;     (add-to-list 'tramp-default-proxies-alist tramp-proxies)))

;; SSH Functions
(defun cory/write-ssh-address-to-history (address)
  (write-region (concat address "
") nil "~/.emacs.d/ssh_history" t))

(defun cory/read-ssh-history ()
  "Return a list of ssh addresses previously connected to."
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/ssh_history")
    (split-string (buffer-string) "\n" t)))

(defun cory/clean-ssh-history ()
  (interactive)
  (write-region "" nil "~/.emacs.d/ssh_history"))

(defun cory/connect-ssh ()
  "Requires sshfs to be installed."
  (interactive)
  (let* ((server (completing-read "user@server.address:" (cory/read-ssh-history)))
	 (mount-path (concat temporary-file-directory ".emacs-sshfs-mount-" server))
	 (user (string-trim-right server "@.*")))
    (cory/write-ssh-address-to-history server)
    (unless (file-directory-p mount-path)
      (make-directory mount-path))
    (unless (file-exists-p (concat mount-path "/home"))
      (shell-command (concat "sshfs -o idmap=user,transform_symlinks "
			     server ":/ " mount-path)))
    (find-file (read-file-name "Find file:"
			       (concat mount-path "/home/" user "/")))))

(defun cory/disconnect-ssh ()
  "Manually disconnects the ssh connection to the server. Requires fusemount."
  (interactive)
  (shell-command
   (concat "fusermount -u "
	   (concat temporary-file-directory
		   ".emacs-sshfs-mount-"
		   (completing-read "user@server.address:"
				    (cory/read-ssh-history))))))
