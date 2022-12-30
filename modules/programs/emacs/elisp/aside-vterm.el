;;; aside-vterm.el --- A shared global Vterm buffer -*- lexical-binding: t -*-

;; Copyright 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>
;; URL: https://git.sr.ht/~mfa/aside
;; Version: 2.6.8
;; Package-Requires: ((emacs "27.1") (dash "2.19.1"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; It’s not a new idea – a terminal you can toggle the visibility of by
;; pressing a simple key binding.  Only a single, shared, global Vterm
;; instance is used.  This configuration is a bit more complex than most
;; others, due to the fact that it needs a custom function to create the Vterm
;; buffer the first time it is opened.

;; Many of the variables in this file defined with ‘defcustom’ use a custom
;; setter.  If you change the value of those variable outside of Customize and
;; do not use ‘customize-set-variable’ to do so, you may want to call
;; ‘aside-disable-configuration’ before changing the value, and
;; ‘aside-enable-configuration’ after the new value has been set.

;; To activate the Vterm configuration, do something like the following:
;; (require 'aside-vterm)
;; (aside-enable-configuration 'vterm)
;; (define-key global-map (kbd "C-`") 'aside-vterm-dwim)


;;; Code:

(require 'rx)
(when (require 'vterm-module nil t)
  (require 'vterm))

(defcustom aside-vterm-buffer-name
  "*Aside-Vterm*"
  "The name for the single, shared, global Aside-Vterm buffer."
  :group 'aside-vterm
  :type 'string
  :set 'aside-configuration-setter-function)

(defun aside--vterm ()
  "Create an Aside-Vterm buffer if necessary, and display it."
  (let ((buffer (get-buffer-create aside-vterm-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vterm-mode)
        (setq default-directory (expand-file-name "~"))
        (vterm-mode))
      (select-window (display-buffer buffer)))))

(aside-define-configuration vterm
			    (rx (literal aside-vterm-buffer-name))
			    '((side . bottom)
			      (slot . -10)
			      (window-height . 10))
			    '(aside-hook-change-default-face-height
			      aside-hook-enable-truncate-lines
			      aside-hook-disable-display-line-numbers-mode)
			    nil
			    #'aside--vterm)

;; (provide 'aside-vterm)

;;; aside-vterm.el ends here
