;;; aside.el --- Toggle-able side windows -*- lexical-binding: t -*-

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
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; The code in this package makes it straightforward to configure side windows
;; which display specific buffers, and commands which can toggle the
;; visibility of those windows.  The function of most interest here is
;; probably ‘aside-window-toggle-dwim’.  This function returns a closure which
;; can be used to toggle the visibility of specific buffers in a specific
;; window, as well as to jump directly to that window if it is already
;; visible.  There are two uses for this code in particular which I find
;; indispensable.  One is the Context configuration, defined in
;; ‘aside-configurations.el’.  The other is the Vterm configuration, which is
;; too complex to be defined entirely by the ‘aside-define-configuration’
;; macro alone.  That configuration is defined in ‘aside-vterm.el’.

;; Here is some more information on how ‘aside-window-toggle-dwim’ works which
;; was too large to put into its docstring:

;; After a matcher closure has been derived from BUFFER-MATCHER, the matcher
;; is used to try to find a matching buffer from ‘buffer-list’, and a window
;; from ‘window-list’ which is currently displaying a matching buffer.

;; When neither a matching buffer or window are found, BUFFER-CREATOR is
;; called, if a value for that argument was supplied.  If a matching buffer
;; was found, but there is no window displaying it, then that buffer will be
;; displayed, and its window selected (unless the command was given a prefix
;; argument).  If the matching buffer is the current buffer (i.e. the buffer
;; visible in the selected window) then the window is deleted.  Otherwise, the
;; matching window , which must already be displaying the buffer, is selected
;; (or deleted, if the command was given a prefix argument).

;; Note that even if both a matching buffer and matching window are found, the
;; buffer displayed in the window might not be the same as the buffer found
;; from searching ‘buffer-list’.  That these values are allowed to be
;; different is a good thing for consistent behaviour.  Specifically, this
;; means that if the function is called while the side window is selected, it
;; will always cause the side window to close, even if the side window could
;; have any one of multiple different buffers visible.  If the logic was
;; modified such that the buffer displayed in the side window had to be the
;; same buffer as the buffer found when searching ‘buffer-list’, then whether
;; the side window was hidden or not when this function was called would
;; depend not only on if the side window was selected, but also which buffer
;; the side window was displaying.  In my experience, this was even more
;; confusing than this attempt at an explanation makes it sound.


;;; Code:

(use-package dash)

;; Symbol builders

(defun aside--condition-var-name (sym)
  "Use SYM to generate a variable name ending with CONDITION."
  (intern (format "aside-%s-condition" sym)))

(defun aside--action-alist-var-name (sym)
  "Use SYM to generate a variable name ending with ACTION-ALIST."
  (intern (format "aside-%s-action-alist" sym)))

(defun aside--display-buffer-function-name (sym)
  "Use SYM to generate a variable name ending with DISPLAY-BUFFER."
  (intern (format "aside--%s-display-buffer" sym)))

(defun aside--hook-var-name (sym)
  "Use SYM to generate a variable name ending with HOOK."
  (intern (format "aside-%s-hook" sym)))

(defun aside--hook-run-var-name (sym)
  "Use SYM to generate a variable name ending with HOOK-RUN."
  (intern (format "aside--%s-hook-run" sym)))

(defun aside--configuration-id-from-var-name (sym)
  "Use SYM to generate a symbol from first word after hyphen."
  (intern (elt (split-string (symbol-name sym) "-") 1)))

(defun aside--group-var-name (sym)
  "Use SYM to generate a variable name for a Customize group."
  (intern (format "aside-%s" sym)))

(defun aside--dwim-func-name (sym)
  "Use SYM to generate a function name ending with ‘-dwim’."
  (intern (format "aside-%s-dwim" sym)))


;; Buffer and window matching

(defun aside--buffer-name-matcher (regexp)
  "Return closure that matches REGEXP against BUFFER name."
  (lambda (buffer)
    (string-match regexp (buffer-name buffer))))

(defun aside--matcher (thing)
  "Return closure for matching buffers, derived from THING.
If THING is a function, that function is returned.  If THING is a
string, it is assumed to be a regexp, and a closure which uses
that regexp to match the names of buffers is returned.  If THING
is a symbol, an attempt is made to generate a variable name from
it and its value is used in a recursive call."
  (cond ((symbolp thing)
         ;; Check if it’s a symbol first, before checking it’s a function,
         ;; because ‘functionp’ will return t when passed a symbol which a
         ;; function is bound to.
         (if-let ((var-name (aside--condition-var-name thing))
                  (value (ignore-errors (symbol-value var-name))))
             (aside--matcher value)
           (error "No value for symbol ‘%s’" var-name)))
        ((functionp thing)
         thing)
        ((stringp thing)
         (aside--buffer-name-matcher thing))
        (t
         (error "Unhandled THING type ‘%s’" thing))))

(defun aside--find-buffer-matching (matcher)
  "Return some buffer MATCHER returns non-nil for."
  (seq-some (lambda (buffer)
              (and (funcall matcher buffer)
                   buffer))
            (buffer-list)))

(defun aside--find-window-with-buffer-matching (matcher)
  "Return window displaying buffer MATCHER returns non-nil for."
  (seq-some (lambda (win)
              (and (funcall matcher (window-buffer win))
                 win))
            (window-list)))


;;; Configuration definition

(defun aside--configuration-display-buffer-alist (id)
  "Return ‘display-buffer-alist’ display options for ID."
  (when-let ((condition-var (aside--condition-var-name id))
             (condition (ignore-errors (symbol-value condition-var)))
             (action-alist-var (aside--action-alist-var-name id))
             (action-alist (ignore-errors (symbol-value action-alist-var)))
             (display-buffer (aside--display-buffer-function-name id)))
    `(,condition (,display-buffer) ,@action-alist)))

(defun aside--define-buffer-display-function (id)
  "Define a buffer display function for ID.

The function first defines a couple of variables, one to act as a
hook for a specific configuration, the other to track which
buffers the hook has been run in.  Then, it defines a function
which calls ‘display-buffer-in-side-window’) and then runs the
appropriate configuration’s hook if necessary."
  (let ((hook-var-name (aside--hook-var-name id))
        (hook-run-var-name (aside--hook-run-var-name id))
        (display-buffer-func-name (aside--display-buffer-function-name id)))
    (set hook-run-var-name nil)
    (make-local-variable hook-run-var-name)
    (fset display-buffer-func-name
          (lambda (buffer alist)
            (display-buffer-in-side-window buffer alist)
            (with-current-buffer buffer
              (unless (symbol-value hook-run-var-name)
                ;; This formerly used ‘set-local’
                (eval `(setq-local ,hook-run-var-name t))
                (run-hooks hook-var-name)))))))

(defun aside-enable-configuration (id)
  "Enable a configuration specified by ID.
If an ‘equal’ list is not already present in
‘display-buffer-alist’, this function pushes a list composed of
‘aside-<id>-condition’ and ‘aside-<id>-action-alist’ to it.  It
also calls ‘aside--define-buffer-display-function’ to define a
buffer display function which runs a hook specific to the
configuration specified by ID when the buffer is displayed."
  (aside--define-buffer-display-function id)
  (add-to-list 'display-buffer-alist
               (aside--configuration-display-buffer-alist id)))

(defun aside-configuration-enabled-p (id)
  "Return non-nil if a configuration with ID is enabled.
If non-nil, the return value will be the result of calling
‘aside--configuration-display-buffer-alist’ with ID."
  (member (aside--configuration-display-buffer-alist id)
          display-buffer-alist))

(defun aside-disable-configuration (id)
  "Disable a configuration specified by ID if one is enabled.
If a configuration with the given ID is not enabled, nil is
returned, otherwise the return value is the list removed from
‘display-buffer-alist’, composed of ‘aside-<id>-condition’ and
‘aside-<id>-action-alist’."
  (when (aside-configuration-enabled-p id)
    (setq display-buffer-alist
          (delete (aside--configuration-display-buffer-alist id)
                  display-buffer-alist))))

(defun aside-configuration-setter-function (name value)
  "Set customizable option NAME to VALUE.
This function first checks if there is already an enabled
configuration corresponding to NAME, and if there is, it will
disable the old configuration before setting the new value, and
then enabling a new configuration which uses the new value.  If a
matching configuration is not already enabled, all that happens is
that the variable NAME is ‘set’ to VALUE."
  (let ((configuration-id (aside--configuration-id-from-var-name name)))
    (if (aside-disable-configuration configuration-id)
        (progn
          (set name value)
          (aside-enable-configuration configuration-id))
      (set name value))))


;; Hook functions

(defun aside-hook-enable-truncate-lines ()
  "Enables line truncation, without displaying a message about it."
  (let ((inhibit-message t))
    (toggle-truncate-lines 1)))

(defun aside-hook-disable-display-line-numbers-mode ()
  "Call command ‘display-line-numbers-mode’ to disable line numbers."
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun aside-hook-change-default-face-pitch ()
  "Make the default face inherit from ‘variable-pitch’."
  (face-remap-add-relative 'default :inherit 'variable-pitch))

(defgroup aside ()
  "Options for Aside side windows."
  :group 'convenience)

(defcustom aside-face-height 120
  "The default face height to use in buffers in a side window.
Used in ‘aside-hook-change-default-face-height’."
  :group 'aside
  :type 'integer)

(defun aside-hook-change-default-face-height ()
  "Change the height of the default face in the current buffer."
  (face-remap-add-relative 'default :height aside-face-height))

(defcustom aside-ace-window-push t
  "When non-nil, enable support for ‘aw-flip-window’.
This is done by calling ‘aw--push-window’ before switching to a
different window."
  :group 'aside
  :type 'boolean)


;; Avoiding Aside-managed buffers in non-Aside-managed windows

(defun aside--display-buffer-alist-elements ()
  "Return alist elements which use Aside display functions."
  (seq-filter (lambda (element)
                (string-match "^aside--"
                              (symbol-name (caadr element))))
              display-buffer-alist))

(defun aside--window-p (window)
  "Return a non-nil value if WINDOW is managed by Aside."
  (let* ((params (window-parameters window))
         (side (alist-get 'window-side params))
         (slot (alist-get 'window-slot params)))
    (seq-find 'identity
              (mapcar
               (lambda (alist)
                 (and (equal side (alist-get 'side alist))
                      (equal slot (or (alist-get 'slot alist) 0))))
               (mapcar 'cddr
                       (aside--display-buffer-alist-elements))))))

(defun aside--buffer-p (buffer)
  "Return a non-nil value if BUFFER is managed by Aside.
Presence of a buffer-local var beginning with ‘aside--’
is used to determine this."
  (with-current-buffer buffer
    (seq-filter (lambda (binding)
                  (string-match "^aside--" (symbol-name (if (listp binding)
                                                            (car binding)
                                                          binding))))
                (buffer-local-variables))))

(defun aside--switch-to-prev-buffer-skip-p-advice-around (oldfun &rest args)
  "Return non-nil when Aside buffers should not be displayed."
  (or (and (not (aside--window-p (cadr args)))
        (aside--buffer-p (caddr args)))
     (and (aside--window-p (cadr args))
        (not (aside--buffer-p (caddr args))))
     (apply oldfun args)))


;; Preventing Aside windows from being resized

(defun aside--set-window-size-fixed (window value)
  "Set the buffer-local ‘window-size-fixed’ variable of the buffer
in WINDOW to VALUE."
  (with-current-buffer (window-buffer window)
    (setq-local window-size-fixed value)))

(defun aside--balance-windows-before (&rest _)
  "Fix the size of all Aside windows on selected frame."
  (-map (-rpartial #'aside--set-window-size-fixed t)
        (-filter #'aside--window-p (window-list))))

(defun aside--balance-windows-after (&rest _)
  "Un-fix the size of all Aside windows on selected frame."
  (-map (-rpartial #'aside--set-window-size-fixed nil)
        (-filter #'aside--window-p (window-list))))

(defun aside--tab-bar-new-tab-to-advice-before (&rest _)
  "Try to select a non-side window."
  (when (aside--window-p (selected-window))
    (select-window (--first (not (aside--window-p it)) (window-list)))))


;; Public interface

(defun aside-work-around-new-tab-bug-enable ()
  "Work around a bug when creating a new tab in some cases.
Specifically, with ‘tab-bar-new-tab-choice’ set to T and with an
Aside-managed window selected, calling ‘tab-bar-new-tab’ fails
and causes things to break."
  (advice-add #'tab-bar-new-tab-to
              :before #'aside--tab-bar-new-tab-to-advice-before))

(defun aside-work-around-new-tab-bug-disable ()
  "Disable work around in ‘aside-work-around-new-tab-bug-enable’."
  (advice-remove #'tab-bar-new-tab-to
                 #'aside--tab-bar-new-tab-to-advice-before))

(defun aside-avoid-buffers-in-regular-windows-enable ()
  "Avoid displaying Aside-managed buffers in non-Aside-managed windows."
  (advice-add 'switch-to-prev-buffer-skip-p
              :around 'aside--switch-to-prev-buffer-skip-p-advice-around))

(defun aside-avoid-buffers-in-regular-windows-disable ()
  "Undo ‘aside-avoid-buffers-in-regular-windows-enable’."
  (advice-remove 'switch-to-prev-buffer-skip-p
                 'aside--switch-to-prev-buffer-skip-p-advice-around))

(defun aside-avoid-resizing-windows-enable ()
  "Avoid resizing Aside-managed windows."
  (advice-add 'balance-windows
              :before #'aside--balance-windows-before)
  (advice-add 'balance-windows
              :after #'aside--balance-windows-after)
  (advice-add 'balance-windows-area
              :before #'aside--balance-windows-before)
  (advice-add 'balance-windows-area
              :after #'aside--balance-windows-after))

(defun aside-avoid-resizing-windows-disable ()
  "Undo ‘aside-avoid-resizing-windows-enable’."
  (advice-remove 'balance-windows
                 #'aside--balance-windows-before)
  (advice-remove 'balance-windows
                 #'aside--balance-windows-after)
  (advice-remove 'balance-windows-area
                 #'aside--balance-windows-before)
  (advice-remove 'balance-windows-area
                 #'aside--balance-windows-after))

(defun aside--preserving-selected-window (fn)
  "Save selected window, call FN, restore selected window."
  (let ((selected-window (selected-window)))
    (funcall fn)
    (select-window selected-window)))

(defun aside--maybe-push-current-window ()
  "Maybe call ‘aw--push-window’."
  (when (and aside-ace-window-push (fboundp 'aw--push-window))
    (aw--push-window (selected-window))))

(defun aside-window-toggle-dwim (buffer-matcher &optional buffer-creator)
  "Return a window toggling closure.
BUFFER-MATCHER is passed to ‘aside--matcher’, and may be a symbol,
string, or function.  Refer to that function’s docstring for more
information.  BUFFER-CREATOR is called if no matching buffer
exists.

For more detailed information about this function, see the
Commentary section of ‘aside.el’.  The overall effect of this
function is to allow creating a single ‘do what I mean’ key
binding which can: create a new buffer and display it in a
specific window; switch directly to that window from any other
window; hide the buffer by deleting the window; and re-open the
buffer if no visible window is currently displaying it."
  (lambda (&optional arg)
    (interactive "P")
    (let* ((matcher (aside--matcher buffer-matcher))
           (buffer (aside--find-buffer-matching matcher))
           (window (aside--find-window-with-buffer-matching matcher)))
      (cond ((and (null window) (null buffer))
             (when buffer-creator
               (if arg
                   (aside--preserving-selected-window buffer-creator)
                 (aside--maybe-push-current-window)
                 (funcall buffer-creator))))
            ((and (null window) buffer)
             (if arg
                 (aside--preserving-selected-window
                  (lambda () (display-buffer buffer)))
               (aside--maybe-push-current-window)
               (select-window (display-buffer buffer))))
            ((funcall matcher (current-buffer))
             (delete-window window))
            (t
             (if arg
                 (delete-window window)
               (aside--maybe-push-current-window)
               (select-window window)))))))

(defmacro aside-define-configuration
    (id condition action hook &optional buffer-matcher buffer-creator)
  "A macro for concisely defining Aside configurations.
This macro defines the following: A customization group named
‘aside-<ID>’; three customizable variables, named
‘aside-<ID>-condition’, ‘aside-<ID>-action-alist’, and
‘aside-<ID>-hook’; and a function named ‘aside-<ID>-dwim’.

ID is the identifier for the configuration, necessary to enable
it later.  CONDITION is used by ‘display-buffer-alist’ to match
buffers.  ACTION corresponds to the ACTION parameter of
‘display-buffer’.  HOOK specifies a normal hook run for buffers
in this configuration’s window.  BUFFER-MATCHER and
BUFFER-CREATOR are passed to ‘aside-window-toggle-dwim’.  If
BUFFER-MATCHER is nil, ID is used.

If a configuration has already been defined and is already enabled,
the configuration will be automatically disabled and re-enabled by
this macro to make it simpler to redefine existing configurations,
e.g. when adding or removing buffer names to match against."
  (declare (indent defun))
  (let ((title (format "Aside-%s" (capitalize (symbol-name id))))
        (group (aside--group-var-name id))
        (condition-name (aside--condition-var-name id))
        (action-name (aside--action-alist-var-name id))
        (hook-name (aside--hook-var-name id))
        (dwim-name (aside--dwim-func-name id)))
    `(let ((already-enabled (aside--configuration-display-buffer-alist ',id)))
       (when already-enabled
         (aside-disable-configuration ',id))
       (defgroup ,group ()
         ,(format "Options for the %s window." title)
         :group 'aside)
       (makunbound ',condition-name)
       (defcustom ,condition-name
         ,condition
         ,(format "Used as a CONDITION in ‘display-buffer-alist’.
Matches the names of buffers that should be displayed in the
%s window." title)
         :group ',group
         :type '(choice regexp function)
         :set 'aside-configuration-setter-function)
       (makunbound ',action-name)
       (defcustom ,action-name
         ,action
         ,(format "Alist used as the ACTION argument to ‘display-buffer’.
Applied to windows containing buffers matched by
‘%s’." condition-name)
         :group ',group
         :type 'sexp
         :set 'aside-configuration-setter-function)
       (makunbound ',hook-name)
       (defcustom ,hook-name
         ,hook
         ,(format "Normal hook run for buffers in the %s window." title)
         :group ',group
         :type 'hook
         :options '(aside-hook-enable-truncate-lines
                    aside-hook-disable-display-line-numbers-mode
                    aside-hook-change-default-face-pitch
                    aside-hook-change-default-face-height))
       (fmakunbound ',dwim-name)
       (defalias ',dwim-name
         (aside-window-toggle-dwim (or ,buffer-matcher ',id) ,buffer-creator)
         ,(format "DWIM command for the %s window." title))
       (when already-enabled
         (aside-enable-configuration ',id)))))

;; (provide 'aside)

;;; aside.el ends here
