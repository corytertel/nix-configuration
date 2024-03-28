
(defgroup vertico-frame nil
  "Using a new frame to show vertico."
  :group 'vertico)

(defcustom vertico-frame-show-minibuffer-rules
  (list "^eval-*")
  "A list of rule showed minibuffer.

a rule can be a regexp or a function.

1. when rule is a regexp and it match `this-command'.
2. when rule is a function and it return t.
3. when rule is a symbol, its value is t.

minibuffer will not be hided by minibuffer-cover."
  :type '(repeat (choice string function)))

(defcustom vertico-frame-frame-alist pop-up-frame-alist
  "The alist used when creating the vertico-frame frame."
  :type 'alist)

(defvar vertico-frame--buffer nil)

(defvar vertico-frame--frame nil)

(define-minor-mode vertico-frame-mode
  "Display Vertico in a new frame instead of the minibuffer."
  :global t)

(cl-defmethod vertico--setup
  :after (&context (vertico-frame-mode (eql t)))
  "Setup minibuffer overlay, which pushes the minibuffer content down."
  (add-hook 'minibuffer-exit-hook #'vertico-frame--minibuffer-exit-hook nil 'local))

(defun vertico-frame--minibuffer-exit-hook ()
  "The function used by `minibuffer-exit-hook'."
  ;; `vertico--resize-window' have set `max-mini-window-height' to
  ;; 1.0, so I think setting it to 1.0 here is safe :-).
  (setq-local max-mini-window-height 1.0)
  (when (= 1 (recursion-depth))
    (delete-frame vertico-frame--frame)
    (setq vertico-frame--frame nil)))

(cl-defmethod vertico--resize-window
  (_height &context (vertico-frame-mode (eql t))))

(cl-defmethod vertico--display-candidates
  :after (_candidates &context (vertico-frame-mode (eql t)))
  "Display candidates in a new frame.

1. Let minibuffer-window's height = 1
2. Hide the context of minibuffer-window by vscroll 100.
3. Show minibuffer in a new frame."
  (let ((buffer (current-buffer))
        (point (point)))
    ;; NOTE: buffer is minibuffer.
    (setq vertico-frame--buffer buffer)
    (vertico-frame--handle-minibuffer-window)
    (vertico-frame--show buffer point)))

(defun vertico-frame--handle-minibuffer-window ()
  "Handle minibuffer window."
  (let ((show-minibuffer-p (vertico-frame--show-minibuffer-p))
        (minibuffer-window (active-minibuffer-window)))
    (setq-local max-mini-window-height 1)
    ;; Let minibuffer-window's height = 1
    (window-resize minibuffer-window
                   (- (window-pixel-height minibuffer-window))
                   nil nil 'pixelwise)
    ;; Hide the context showed in minibuffer-window.
    (set-window-vscroll minibuffer-window 100)
    (when show-minibuffer-p
      (set-window-vscroll minibuffer-window 0))))

(defun vertico-frame--show-minibuffer-p ()
  "Test show minibuffer or not."
  (cl-some
   (lambda (rule)
     (cond ((functionp rule)
            (funcall rule))
           ((and rule
	       (stringp rule)
	       (symbolp this-command))
            (string-match-p rule (symbol-name this-command)))
           ((symbolp rule)
            (symbol-value rule))
           (t nil)))
   vertico-frame-show-minibuffer-rules))

(defun vertico-frame--show (buffer window-point)
  "`display-buffer' of vertico-frame."
  (with-selected-window (vertico-frame-last-window)
    (unless vertico-frame--frame
      (setq vertico-frame--frame (make-frame vertico-frame-frame-alist)))
    (select-frame vertico-frame--frame)
    (switch-to-buffer buffer)
    (setq-local mode-line-format nil)))

(defun vertico-frame-last-window ()
  "Get the last actived window before active minibuffer."
  (let ((window (minibuffer-selected-window)))
    (or (if (window-live-p window)
           window
         (next-window))
       (selected-window))))

(defun vertico-frame-cleanup ()
  "Remove frames and buffers used for vertico-frame."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (minibufferp buffer)
      (delete-frame (window-frame (get-buffer-window buffer)))
      (kill-buffer buffer))))
