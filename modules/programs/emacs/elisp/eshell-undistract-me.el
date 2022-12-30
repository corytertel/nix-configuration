;;; ehsell-undistract-me.el --- Notifies you when a long running command is complete. -*- lexical-binding: t -*-

;; Author: Cory Tertel <ctertel@comcast.net>
;; Maintainer: Cory Tertel <ctertel@comcast.net>
;; URL: https://github.com/corytertel/eshell-undistract-me
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: eshell

;; This file is an eshell port of the bash undistract-me.

;; The original bash undistract-me is Copyright (c) 2008-2012 by the
;; orignal undistract-me developers. See the LICENSE in the original
;; undistract-me repo for more details. Like the original undistract-me,
;; this file is under the Expat License.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Generates a notification for any command that takes longer than a given
;; ammount of seconds to return to the shell.
;; e.g. if LONG_RUNNING_COMMAND_TIMEOUT=10, then 'sleep 11' will always
;; generate a notification.

;; Relies on eshell-pre-command-hook and eshell-before-prompt-hook.

;; Hook eshell-undistract-me-pre-command into eshell-pre-command-hook
;; and eshell-undistract-me-before-prompt into ehsell-before-prompt-hook.

;; Requires libnotify to be installed.  By default uses pulseaudio's
;; `paplay'.  Any command can be used to play audio by changing
;; `eshell-undistract-me-sound-command'.

;;; Customization

;;; Code:

;; (require 'eshell)
;; (require 'esh-mode)
;; (require 'term)

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

;;;###autoload
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

;;;###autoload
(defun eshell-undistract-me-pre-command ()
  "Hook this command into `eshell-pre-command-hook'."
  (setq
   eshell-undistract-me-last-command-started (eshell-undistract-me-get-now)
   eshell-undistract-me-last-command (getenv "1")
   eshell-undistract-me-last-window (eshell-undistract-me-active-x-window-id))
  nil)

;; (provide 'eshell-undistract-me)

;;; eshell-undistract-me.el ends here
