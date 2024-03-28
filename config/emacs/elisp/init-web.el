
(use-package web-mode
  :bind
  (:map web-mode-map
   ("C-M-a" . backward-paragraph)
   ("C-M-i" . mark-paragraph)
   ("C-M-e" . forward-paragraph)
   ("<" . cory/insert-angled-pair-or-wrap)
   (">" . cory/close-angled-pair)
   ("M-i" . web-mode-element-select)
   ("C-c C-o" . cory/open-current-file-in-firefox)
   ("M-SPC" . cory/mark-word))

  :custom
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)

  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . web-mode))
  (add-to-list 'auto-mode-alist ' ("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  :config

  ;; Faces
  (set-face-attribute 'web-mode-html-tag-face nil
		      :foreground "Blue1")
  (set-face-attribute 'web-mode-html-attr-name-face nil
		      :foreground "sienna")
  (set-face-attribute 'web-mode-current-element-highlight-face nil
		      :foreground 'unspecified
		      :background "turquoise")

  ;; Functions have issues with falsely inserting while in strings

  (defun cory/insert-angled-pair-or-wrap ()
    (interactive)
    (cond (mark-active (call-interactively #'web-mode-element-wrap))
          (t (progn (insert ?<)
        	    (save-excursion
        	      (insert ?>))))))

  (defun cory/close-angled-pair ()
    (interactive)
    (if (looking-at ">")
        (forward-char 1)
      (insert ?>))
    (when (not (looking-back "/ *>"))
      (web-mode-element-close)))

  (defvar cory/web-mode-words
    '("![" "!attlist" "!doctype" "!element" "!entity" "_blank" "_parent" "_self" "_top" "abbr"
      "absbottom" "absmiddle" "accept" "accesskey" "acronym" "action" "address" "align" "alink"
      "alt" "anonymous" "application/x-www-form-urlencoded" "archive" "area" "array" "article"
      "aside" "au" "audio" "auto" "autocomplete" "autofocus" "autoplay" "b" "background" "base"
      "baseline" "bdi" "bdo" "bgcolor" "big" "blink" "blockquote" "body" "border" "bottom" "box"
      "br" "builtin" "button" "canvas" "caption" "captions" "cellpadding" "center" "chapters"
      "char" "charoff" "charset" "checkbox" "checked" "circle" "cite" "class" "classid" "clear"
      "code" "codebase" "codetype" "col" "colgroup" "color" "cols" "colspan" "comment" "constant"
      "content" "controls" "coords" "crossorigin" "data" "datalist" "date" "datetime" "dd" "declare"
      "default" "defer" "del" "descriptions" "dfn" "dir" "dirname" "disabled" "disc" "div" "dl" "dt"
      "em" "email" "embed" "fieldset" "figcaption" "figure" "file" "file:" "finger:" "fn" "font"
      "footer" "for" "form" "formaction" "formenctype" "formmethod" "formnovalidate" "formtarget"
      "frame" "frameborder" "frameset" "ftp:" "function-name" "get" "gopher:" "h1" "h2" "h3" "h4"
      "h5" "h6" "head" "header" "height" "hgroup" "hidden" "high" "hr" "href" "hspace" "html"
      "http-equiv" "http:" "https:" "i" "id" "iframe" "ignore" "image" "img" "include" "input"
      "inputmode" "ins" "isindex" "ismap" "justify" "kbd" "keyword" "kind" "label" "lang" "left"
      "legend" "li" "link" "list" "longdesc" "loop" "low" "ltr" "made" "mailto:" "main" "map"
      "marginheight" "marginwidth" "mark" "math" "max" "maxlength" "media" "mediagroup" "menu"
      "meta" "metadata" "meter" "method" "middle" "min" "minlength" "multipart/form-data" "multiple"
      "muted" "name" "nav" "news:" "next" "no" "nobr" "noframes" "nohref" "none" "noresize"
      "noscript" "noshade" "nowrap" "number" "object" "off" "ol" "on" "onblur" "onchange" "onfocus"
      "onload" "onunload" "optgroup" "optimum" "option" "output" "over" "p" "param" "parent"
      "password" "pattern" "person" "placeholder" "poly" "post" "poster" "pre" "preload" "previous"
      "progress" "prompt" "radio" "range" "readonly" "rect" "ref" "rel" "required" "reset" "rev"
      "right" "rlogin:" "rows" "rowspan" "rp" "rt" "rtl" "ruby" "samp" "scheme" "script" "scrolling"
      "search" "section" "select" "selected" "shape" "size" "small" "source" "span" "square" "src"
      "srclang" "standby" "step" "string" "strong" "style" "sub" "subdocument" "submit" "subtitles"
      "summary" "sup" "tabindex" "table" "tbody" "td" "tel" "telnet:" "text" "text/javascript"
      "text/plain" "textarea" "texttop" "tfoot" "th" "thead" "time" "title" "tn3270:" "top" "tr"
      "track" "tt" "type" "ul" "url" "use-credentials" "usemap" "valign" "value" "valuetype" "var"
      "variable-name" "video" "vlink" "vspace" "wais:" "warning" "wbr" "width" "yes"))

  (defun cory/web-mode-html-capf ()
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
	(list (car bounds)
              (cdr bounds)
              cory/web-mode-words
              :exclusive 'no))))

  ;; (require 'sgml-mode)
  ;; (add-hook 'web-mode-hook
  ;; 	    (lambda ()
  ;; 	      (when (and buffer-file-name
  ;; 		       (string-match ".*\.html" buffer-file-name))
  ;; 		(setq-local completion-at-point-functions '(html-mode--complete-at-point t)))))

  (defun cory/forward-indent ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (insert "  ")))

  (defun cory/backward-indent ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (when (looking-at " ")
	(delete-char 1))
      (when (looking-at " ")
	(delete-char 1))))

  (defun cory/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (whitespace-mode -1))

  (defun cory/web-mode-html-hook ()
    (setq-local completion-at-point-functions '(cory/web-mode-html-capf t))
    (aggressive-indent-mode 1))

  (defun cory/web-mode-css-hook ()
    (require 'css-mode)
    (setq-local completion-at-point-functions '(css-completion-at-point t))
    (aggressive-indent-mode 1))

  (defun cory/web-mode-js-hook ()
    (aggressive-indent-mode 1)
    (require 'eglot)
    (cory/eglot)
    (when (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")))

  (add-hook 'web-mode-hook
	    (lambda ()
	      (cory/web-mode-hook)
	      (cond
	       ((and buffer-file-name
		   (string-match ".*\\.html" buffer-file-name))
		(cory/web-mode-html-hook))
	       ((and buffer-file-name
		   (string-match ".*\\.css" buffer-file-name))
		(cory/web-mode-css-hook))
	       ((and buffer-file-name
		   (string-match "\\(.*\\.js\\)\\|\\(.*\\.jsx\\)\\|\\(.*\\.ts\\)\\|\\(.*\\.tsx\\)" buffer-file-name))
		(cory/web-mode-js-hook))
	       (t nil))))

  (defun cory/open-current-file-in-firefox ()
    "Opens the current file in Firefox."
    (interactive)
    (let ((file
	   (if (not (file-remote-p buffer-file-name))
	       buffer-file-name
	     (let ((buf (current-buffer)))
	       (write-file (concat "/tmp/" (replace-regexp-in-string "/" "!" buffer-file-name)))
	       (async-shell-command (concat "firefox file://" buffer-file-name) nil nil)
	       (switch-to-buffer buf))))))
    (async-shell-command (concat "firefox file://" buffer-file-name) nil nil)))

;; Javascript
(setq js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
