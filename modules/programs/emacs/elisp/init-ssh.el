;; Tramp

(setq tramp-default-method "ssh")
(tramp-change-syntax 'simplified)

;; Only for debugging slow tramp connections
;;(setq tramp-verbose 7)

;; Skip version control for tramp files
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq remote-file-name-inhibit-cache nil)
(setq tramp-verbose 1)

;; Use ControlPath from .ssh/config
;; (setq tramp-ssh-controlmaster-options "")

;; Backup tramp files like local files and don't litter the remote
;; file system with my emacs backup files
(setq tramp-backup-directory-alist backup-directory-alist)

;; See https://www.gnu.org/software/tramp/#Ad_002dhoc-multi_002dhops
;; For all hosts, except my local one, first connect via ssh, and then apply sudo -u root:
(dolist (tramp-proxies '((nil "\\`root\\'" "/ssh:%h:")
                         ((regexp-quote (system-name)) nil nil)
                         ("localhost" nil nil)))
  (add-to-list 'tramp-default-proxies-alist tramp-proxies))

;; SSHFS Functions
(defun cory/write-ssh-address-to-history (address)
  (write-region (concat address "
") nil "~/.emacs.d/ssh_history" t))

(defun cory/read-ssh-history ()
  "Return a list of ssh addresses previously connected to."
  (when (file-exists-p "~/.emacs.d/ssh_history")
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/ssh_history")
      (split-string (buffer-string) "\n" t))))

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
  "Manually disconnects the ssh connection to the server. Requires fusermount."
  (interactive)
  (shell-command
   (concat "fusermount -u "
	   (concat temporary-file-directory
		   ".emacs-sshfs-mount-"
		   (completing-read "user@server.address:"
				    (cory/read-ssh-history))))))

(global-set-key (kbd "C-c s") #'cory/connect-ssh)
(global-set-key (kbd "C-c S") #'cory/disconnect-ssh)
