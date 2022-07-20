;;; ehsell-undistract-me --- Summary

;; Eshell port of undistract-me, by corytertel

;; The original bash undistract-me is Copyright (c) 2008-2012
;; by the orignal undistract-me developers.
;; See the LICENSE in the original undistract-me repo for more details.

;; Generate a notification for any command that takes longer than this amount
;; of seconds to return to the shell.  e.g. if LONG_RUNNING_COMMAND_TIMEOUT=10,
;; then 'sleep 11' will always generate a notification.

;; Relies on eshell-pre-command-hook

;; Hook eshell-undistract-me-notify-when-long-running-commands-finish-install
;; into eshell-pre-command-hook

;;; Commentary:
;;; Code:

(defgroup eshell-undistract-me nil
  "Eshell undistract-me."
  :group 'eshell)

(defcustom eshell-undistract-me-long-running-command-timeout 10
  "The minimum required duration (seconds) for a notification to send."
  :group 'eshell-undistract-me
  :type 'integer)

(defcustom eshell-undistract-me-play-sound nil
  "Whether or to play a sound with notifications.  Non-nil plays a sound."
  :group 'eshell-undistract-me
  :type 'boolean)

(defcustom eshell-undistract-me-sound-command "paplay"
  "Command to run to play the notification sound."
  :group 'eshell-undistract-me
  :type 'string)

(defcustom eshell-undistract-me-sound-path
  "/usr/share/sounds/freedesktop/stereo/complete.oga"
  "Path to the sound to play for notifications."
  :group 'eshell-undistract-me
  :type 'string)

(defvar eshell-undistract-me-last-command-started nil
  "When the last command was started in unix seconds.")

(defvar eshell-undistract-me-last-command nil
  "The last command run by eshell.")

(defvar eshell-undistract-me-last-window nil
  "The id of the active window.")

(defun eshell-undistract-me-get-now ()
  "Return the current time since 1970-01-01 in seconds."
  (time-convert nil 'integer))

(defun eshell-undistract-me-active-x-window-id ()
  "Return the Id of the currently focused X window."
  (if (and (eq (window-system) 'x) (getenv "DISPLAY"))
      (x-window-property "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t)
    "nowindowid"))

(eshell-undistract-me-active-x-window-id)

;; From https://gist.github.com/codecoll/66b40f524111f0dff75921784a4b2095
(defun eshell-undistract-me-sec-to-human (secs)
  "Convert `SECS' to human readable time format of HH:MM:SS."
  (let ((hours (/ secs 3600))
	(minutes (/ (% secs 3600) 60))
	 (seconds (% secs 60)))
    (format "%s%s%s"
            (if (> hours 0)
		(format "%sh " hours)
              "")
            (if (> minutes 0)
		(format "%sm " minutes)
              "")
            (if (> seconds 0)
		(format "%ss" seconds)
              ""))))

(defun eshell-undistract-me-before-prompt ()
  "Hook this command into `eshell-before-prompt-hook'."
  (if eshell-undistract-me-last-command-started
      (let ((now (eshell-undistract-me-get-now))
	    (current-window (eshell-undistract-me-active-x-window-id)))
	(if (or (not (eq current-window eshell-undistract-me-last-window))
	       (getenv "IGNORE WINDOW CHECK")
	       (eq current-window "nowindowid"))
	    (let ((time-taken (- now eshell-undistract-me-last-command-started))
		  (time-taken-human (eshell-undistract-me-sec-to-human (- now eshell-undistract-me-last-command-started))))
	      (if (and (> time-taken eshell-undistract-me-long-running-command-timeout)
		     (getenv "DISPLAY"))
		  (let ((icon "dialog-information")
			(urgency "low"))
		    (shell-command
		     (concat
		      "notify-send"
		      " -i " icon
		      " -u " urgency
		      " \"Command completed in " time-taken-human " " eshell-undistract-me-last-command "\""))
		    (if eshell-undistract-me-play-sound
			(shell-command
			 (concat
			  eshell-undistract-me-sound-command " " eshell-undistract-me-sound-path)))))))))
  nil)

(defun eshell-undistract-me-pre-command ()
  "Hook this command into `eshell-pre-command-hook'."
  (setq
   eshell-undistract-me-last-command-started (eshell-undistract-me-get-now)
   eshell-undistract-me-last-command (getenv "1")
   eshell-undistract-me-last-window (eshell-undistract-me-active-x-window-id))
  nil)

;;; eshell-undistract-me.el ends here
