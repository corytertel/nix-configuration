
;; Eww
(setq eww-search-prefix "http://frogfind.com/?q=")

(with-eval-after-load 'eww
  (setq eww-auto-rename-buffer 'title
        browse-url-browser-function #'eww-browse-url)
  (define-key eww-mode-map (kbd "C-M-s") #'shr-previous-link)
  (define-key eww-mode-map (kbd "C-M-i") nil))

(defun cory/eww ()
  "Prompt for a URL or keywords to search the web for."
  (interactive)
  (eww (mapconcat #'identity
                  (completing-read-multiple "Browse or search: "
                                            eww-prompt-history
                                            nil nil nil
                                            'eww-prompt-history
                                            (car (eww-suggested-uris)))
                  " ")))

(defun cory/eww-wikipedia (query)
  "QUERY wikipedia"
  (interactive "sQuery: ")
  (eww (concat "https://en.wikipedia.org/wiki/"
               (replace-regexp-in-string " " "_" query))))

(defun cory/eww-wikipedia-at-point ()
  "Query wikipedia for thing at point."
  (interactive)
  (cory/eww-wikipedia-at-point (word-at-point)))

(defun cory/eww-wiktionary-search ()
  (interactive)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (let ((region (buffer-substring-no-properties (mark) (point))))
	(deactivate-mark)
	(eww (concat "https://en.wiktionary.org/wiki/" region "#Russian")))
    (eww (concat "https://en.wiktionary.org/wiki/"
		 (read-string (concat "Word (default: " (word-at-point) "): ") nil nil (word-at-point))
		 "#Russian"))))

;; (define-key org-mode-map (kbd "C-c <C-h>") 'eww-wiktionary-search)

;; (global-set-key (kbd "C-c e") #'cory/eww)
