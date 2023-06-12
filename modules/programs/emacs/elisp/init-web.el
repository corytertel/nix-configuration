
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
  (async-shell-command (concat "firefox file://" buffer-file-name) nil nil))

(with-eval-after-load 'mhtml-mode
  (define-key mhtml-mode-map (kbd "C-c C-o") #'cory/open-current-file-in-firefox))

;; (defun cory/sgml-mark-tag ()
;;   (interactive)
;;   (call-interactively #'sgml-skip-tag-backward)
;;   (call-interactively #'set-mark-command)
;;   (call-interactively #'sgml-skip-tag-forward))

;; ;; Automatically close html tags when you type '</'
;; (setq sgml-quick-keys 'close)
;; (with-eval-after-load 'sgml-mode
;;   ;; remove annoying binds
;;   (define-key sgml-mode-map (kbd "<") nil)
;;   (define-key sgml-mode-map (kbd ">") nil)
;;   (define-key sgml-mode-map (kbd "\"") nil)
;;   (define-key sgml-mode-map (kbd "&") nil)
;;   (define-key sgml-mode-map (kbd "'") nil)
;;   (define-key sgml-mode-map (kbd "SPC") nil)
;;   (define-key sgml-mode-map (kbd "C-c C-o") nil)
;;   ;; Minimak binds
;;   (define-key sgml-mode-map  (kbd "C-M-a") nil)
;;   (define-key sgml-mode-map  (kbd "C-M-e") nil)
;;   (define-key sgml-mode-map  (kbd "C-M-i") nil)
;;   (define-key sgml-mode-map  (kbd "C-M-b") #'sgml-skip-tag-backward)
;;   (define-key sgml-mode-map  (kbd "C-M-y") #'sgml-skip-tag-forward)
;;   (define-key sgml-mode-map  (kbd "C-M-h") #'cory/sgml-mark-tag)
;;   (define-key sgml-mode-map  (kbd "C-M-s") #'ispell-complete-word)
;;   (define-key sgml-mode-map  (kbd "C-c C-b") nil)
;;   (define-key sgml-mode-map  (kbd "C-c C-f") nil)
;;   (define-key sgml-mode-map  (kbd "C-c C-j") #'sgml-skip-tag-backward)
;;   (define-key html-mode-map (kbd "C-c C-j") #'sgml-skip-tag-backward)
;;   (define-key sgml-mode-map  (kbd "C-c C-l") #'sgml-skip-tag-forward))

(use-package web-mode
  :bind
  (:map web-mode-map
   ("M-h" . cory/mark-sentence)
   ("C-M-b" . backward-paragraph)
   ("C-M-h" . mark-paragraph)
   ("C-M-y" . forward-paragraph)
   ("<" . cory/insert-angled-pair)
   (">" . cory/close-angled-pair)
   ;; Minimak binds
   ("C-c C-a b" . web-mode-attribute-beginning)
   ("C-c C-a e" . web-mode-attribute-end)
   ("C-c C-a i" . web-mode-attribute-insert)
   ("C-c C-a k" . web-mode-attribute-kill)
   ("C-c C-a n" . web-mode-attribute-next)
   ("C-c C-a p" . web-mode-attribute-previous)
   ("C-c C-a s" . web-mode-attribute-select)
   ("C-c C-a t" . web-mode-attribute-transpose)
   ("C-c C-b b" . web-mode-block-beginning)
   ("C-c C-b c" . web-mode-block-close)
   ("C-c C-b e" . web-mode-block-end)
   ("C-c C-b k" . web-mode-block-kill)
   ("C-c C-b n" . web-mode-block-next)
   ("C-c C-b p" . web-mode-block-previous)
   ("C-c C-b s" . web-mode-block-select)
   ("C-c C-d a" . web-mode-dom-apostrophes-replace)
   ("C-c C-d d" . web-mode-dom-errors-show)
   ("C-c C-d e" . web-mode-dom-entities-replace)
   ("C-c C-d n" . web-mode-dom-normalize)
   ("C-c C-d q" . web-mode-dom-quotes-replace)
   ("C-c C-d t" . web-mode-dom-traverse)
   ("C-c C-d x" . web-mode-dom-xpath)
   ("C-c C-e +" . web-mode-element-extract)
   ("C-c C-e -" . web-mode-element-contract)
   ("C-c C-e /" . web-mode-element-close)
   ("C-c C-e I" . web-mode-element-insert-at-point)
   ("C-c C-e a" . web-mode-element-content-select)
   ("C-c C-e b" . web-mode-element-beginning)
   ("C-c C-e c" . web-mode-element-clone)
   ("C-c C-e d" . web-mode-element-child)
   ("C-c C-e e" . web-mode-element-end)
   ("C-c C-e f" . web-mode-element-children-fold-or-unfold)
   ("C-c C-e i" . web-mode-element-insert)
   ("C-c C-e k" . web-mode-element-kill)
   ("C-c C-e m" . web-mode-element-mute-blanks)
   ("C-c C-e n" . web-mode-element-next)
   ("C-c C-e p" . web-mode-element-previous)
   ("C-c C-e r" . web-mode-element-rename)
   ("C-c C-e s" . web-mode-element-select)
   ("C-c C-e t" . web-mode-element-transpose)
   ("C-c C-e u" . web-mode-element-parent)
   ("C-c C-e v" . web-mode-element-vanish)
   ("C-c C-e w" . web-mode-element-wrap)
   ("C-c C-f" . web-mode-fold-or-unfold)
   ("C-c C-h" . web-mode-buffer-fontify)
   ("C-c C-j" . web-mode-jshint)
   ("C-c C-l" . web-mode-file-link)
   ("C-c C-n" . web-mode-navigate)
   ("C-c C-r" . web-mode-reload)
   ("C-c C-s" . web-mode-snippet-insert)
   ("C-c C-t a" . web-mode-tag-attributes-sort)
   ("C-c C-t b" . web-mode-tag-beginning)
   ("C-c C-t e" . web-mode-tag-end)
   ("C-c C-t m" . web-mode-tag-match)
   ("C-c C-t n" . web-mode-tag-next)
   ("C-c C-t p" . web-mode-tag-previous)
   ("C-c C-t s" . web-mode-tag-select)
   ("C-c C-w" . web-mode-whitespaces-show)
   ("C-c RET" . web-mode-mark-and-expand)
   ("C-c TAB" . web-mode-buffer-indent))

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

  :config
  ;; (setq web-mode-engines-alist
  ;; 	'(("php"    . "\\.phtml\\'")))

  ;; Faces
  (set-face-attribute 'web-mode-html-tag-face nil
		      :foreground "Blue1")
  (set-face-attribute 'web-mode-html-attr-name-face nil
		      :foreground "sienna")
  (set-face-attribute 'web-mode-current-element-highlight-face nil
		      :foreground 'unspecified
		      :background "turquoise")

  (defun cory/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2))
  (add-hook 'web-mode-hook  'cory/web-mode-hook)

  (defun cory/insert-angled-pair ()
    (interactive)
    (if (looking-at "<")
	(forward-char 1)
      (insert ?<)
      (save-excursion
	(insert ?>))))

  (defun cory/close-angled-pair ()
    (interactive)
    (if (looking-at ">")
	(forward-char 1)
      (insert ?>)))

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

  (defun cory/web-mode-html-hook ()
    (setq-local completion-at-point-functions '(cory/web-mode-html-capf t))
    (local-set-key (kbd "TAB") #'cory/forward-indent)
    (local-set-key (kbd "<backtab>") #'cory/backward-indent)
    (local-set-key (kbd "C-<tab>") #'indent-for-tab-command))

  (defun cory/web-mode-css-hook ()
    (require 'css-mode)
    (setq-local completion-at-point-functions '(css-completion-at-point t))
    (aggressive-indent-mode 1))

  (add-hook 'web-mode-hook
	    (lambda ()
	      (cond
	       ((and buffer-file-name
		   (string-match ".*\.html" buffer-file-name))
		(cory/web-mode-html-hook))
	       ((and buffer-file-name
		   (string-match ".*\.css" buffer-file-name))
		(cory/web-mode-css-hook))
	       (t nil)))))

;; Typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

;; Javascript
(setq js-indent-level 2)
