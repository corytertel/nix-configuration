
(require 'vertico)
(require 'vertico-multiform)

(defgroup vertico-frame nil
  "Using posframe to show vertico."
  :group 'vertico)

(defcustom vertico-frame-fallback-mode #'ignore
  "When posframe is not workable, this mode will be used as fallback."
  :type 'function)

(defcustom vertico-frame-font nil
  "The font used by vertico-frame.
When nil, Using current frame's font as fallback."
  :type 'string)

(defcustom vertico-frame-width nil
  "The width of vertico-frame."
  :type 'number)

(defcustom vertico-frame-height nil
  "The height of vertico-frame."
  :type 'number)

(defcustom vertico-frame-min-width nil
  "The min width of vertico-frame."
  :type 'number)

(defcustom vertico-frame-min-height nil
  "The min height of vertico-frame."
  :type 'number)

(defcustom vertico-frame-truncate-lines t
  "Non-nil means truncate lines in vertico-frame."
  :type 'boolean)

(defcustom vertico-frame-poshandler #'posframe-poshandler-frame-center
  "The posframe poshandler used by vertico-frame."
  :type 'function)

(defcustom vertico-frame-refposhandler #'vertico-frame-refposhandler-default
  "The refposhandler used by vertico-frame.

NOTE: This variable is very useful to EXWM users."
  :type 'function)

(defcustom vertico-frame-size-function #'vertico-frame-get-size
  "The function which is used to deal with posframe's size."
  :type 'function)

(defcustom vertico-frame-border-width 2
  "The border width used by vertico-frame.
When 0, no border is showed."
  :type 'number)

(defcustom vertico-frame-parameters nil
  "The frame parameters used by vertico-frame."
  :type 'string)

(defcustom vertico-frame-show-minibuffer-rules
  (list "^eval-*")
  "A list of rule showed minibuffer.

a rule can be a regexp or a function.

1. when rule is a regexp and it match `this-command'.
2. when rule is a function and it return t.
3. when rule is a symbol, its value is t.

minibuffer will not be hided by minibuffer-cover."
  :type '(repeat (choice string function)))

(defface vertico-frame
  '((t (:inherit default)))
  "Face used by the vertico-frame."
  :group 'vertico-frame)

(defface vertico-frame-border
  '((t (:inherit default :background "gray50")))
  "Face used by the vertico-frame's border when minibuffer-depth = 1."
  :group 'vertico-frame)

(defface vertico-frame-border-2
  '((t (:inherit default :background "red")))
  "Face used by the vertico-frame's border when minibuffer-depth = 2."
  :group 'vertico-frame)

(defface vertico-frame-border-3
  '((t (:inherit default :background "green")))
  "Face used by the vertico-frame's border when minibuffer-depth = 3."
  :group 'vertico-frame)

(defface vertico-frame-border-4
  '((t (:inherit default :background "blue")))
  "Face used by the vertico-frame's border when minibuffer-depth = 4."
  :group 'vertico-frame)

(defface vertico-frame-border-fallback
  '((t (:inherit default :background "yellow")))
  "Face used by the vertico-frame's border when find no face."
  :group 'vertico-frame)

(defvar vertico-frame--buffer nil)

;; Fix warn
(defvar exwm--connection)
(defvar exwm-workspace--workareas)
(defvar exwm-workspace-current-index)

(define-minor-mode vertico-frame-mode
  "Display Vertico in posframe instead of the minibuffer."
  :global t
  (cond
   (vertico-frame-mode
    (when (not (posframe-workable-p))
      (funcall (buffer-local-value 'vertico-frame-fallback-mode (current-buffer)) 1)))
   (t
    (if (not (posframe-workable-p))
        (funcall (buffer-local-value 'vertico-frame-fallback-mode (current-buffer)) -1)
      ;; When vertico-frame-mode is disabled, hide posframe and let
      ;; the contents of minibuffer show again, this approach let
      ;; vertico-frame works with vertico multiform toggle.
      (set-window-vscroll (active-minibuffer-window) 0)
      (posframe-hide vertico-frame--buffer)))))

;; Support vertico-multiform
(cl-pushnew 'vertico-frame-mode vertico-multiform--display-modes)
(vertico-multiform--define-display-toggle posframe)
(define-key vertico-multiform-map (kbd "M-P") #'vertico-multiform-posframe)

(cl-defmethod vertico--setup :after (&context (vertico-frame-mode (eql t)))
  "Setup minibuffer overlay, which pushes the minibuffer content down."
  (add-hook 'minibuffer-exit-hook #'vertico-frame--minibuffer-exit-hook nil 'local))

(defun vertico-frame--minibuffer-exit-hook ()
  "The function used by `minibuffer-exit-hook'."
  ;; `vertico--resize-window' have set `max-mini-window-height' to
  ;; 1.0, so I think setting it to 1.0 here is safe :-).
  (setq-local max-mini-window-height 1.0)
  (posframe-hide vertico-frame--buffer))

(cl-defmethod vertico--resize-window (_height &context (vertico-frame-mode (eql t))))

(cl-defmethod vertico--display-candidates :after (_candidates &context (vertico-frame-mode (eql t)))
  "Display candidates in posframe.

1. Let minibuffer-window's height = 1
2. Hide the context of minibuffer-window by vscroll 100.
3. Show minibuffer with the help of posframe-show."
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
  "`posframe-show' of vertico-frame."
  (with-selected-window (vertico-frame-last-window) ;Some posframe poshandlers need infos of last-window.
    (apply #'posframe-show
           buffer
           :cursor 'box
           :window-point window-point
           :font (buffer-local-value 'vertico-frame-font buffer)
           ;; Variable settings in `vertico-multiform-commands' will
           ;; save to BUFFER as buffer-local variables, so we need to
           ;; get buffer local value from BUFFER, for example:
           ;;
           ;; (setq vertico-multiform-commands
           ;;       '((consult-line
           ;;          posframe
           ;;          (vertico-frame-poshandler . posframe-poshandler-frame-top-center))
           ;;         (t buffer)))
           ;;
           :poshandler (buffer-local-value 'vertico-frame-poshandler buffer)
           :background-color (face-attribute 'vertico-frame :background nil t)
           :foreground-color (face-attribute 'vertico-frame :foreground nil t)
           :border-width (buffer-local-value 'vertico-frame-border-width buffer)
           :border-color (vertico-frame--get-border-color)
           :override-parameters (buffer-local-value 'vertico-frame-parameters buffer)
           :refposhandler (buffer-local-value 'vertico-frame-refposhandler buffer)
           :hidehandler #'vertico-frame-hidehandler
           :lines-truncate (buffer-local-value 'vertico-frame-truncate-lines buffer)
           (funcall (buffer-local-value 'vertico-frame-size-function buffer) buffer))))

(defun vertico-frame-last-window ()
  "Get the last actived window before active minibuffer."
  (let ((window (minibuffer-selected-window)))
    (or (if (window-live-p window)
            window
          (next-window))
        (selected-window))))

(defun vertico-frame--get-border-color ()
  "Get color of vertico-frame border."
  (face-attribute
   (let* ((n (minibuffer-depth))
          (face (intern (format "vertico-frame-border-%s" n)))
          (face-fallback 'vertico-frame-border-fallback))
     (if (= n 1)
         'vertico-frame-border
       (if (facep face)
           face
         face-fallback)))
   :background nil t))

(defun vertico-frame-refposhandler-default (&optional frame)
  "The default posframe refposhandler used by vertico-frame.
Optional argument FRAME ."
  (cond
   ;; EXWM environment
   ((bound-and-true-p exwm--connection)
    (or (ignore-errors
         (let ((info (elt exwm-workspace--workareas
                          exwm-workspace-current-index)))
           (cons (elt info 0)
                 (elt info 1))))
       ;; Need user install xwininfo.
       (ignore-errors
         (posframe-refposhandler-xwininfo frame))
       ;; Fallback, this value will incorrect sometime, for example: user
       ;; have panel.
       (cons 0 0)))
   (t nil)))

(defun vertico-frame-hidehandler (_)
  "Hidehandler used by vertico-frame."
  (not (minibufferp)))

(defun vertico-frame-get-size (buffer)
  "The default functon used by `vertico-frame-size-function'."
  (list
   :height (buffer-local-value 'vertico-frame-height buffer)
   :width (buffer-local-value 'vertico-frame-width buffer)
   :min-height (or (buffer-local-value 'vertico-frame-min-height buffer)
                  (let ((height (+ vertico-count 1)))
                    (min height (or (buffer-local-value 'vertico-frame-height buffer) height))))
   :min-width (or (buffer-local-value 'vertico-frame-min-width buffer)
                 (let ((width (round (* (frame-width) 0.62))))
                   (min width (or (buffer-local-value 'vertico-frame-width buffer) width))))))

(defun vertico-frame-cleanup ()
  "Remove frames and buffers used for vertico-frame."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (minibufferp buffer)
      (posframe-delete-frame buffer))))
