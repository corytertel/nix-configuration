;;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode))

;; F#
(use-package fsharp-mode)

;; (use-package eglot-fsharp
;;   :hook (fsharp-mode . cory/eglot-ensure))

;; prettify symbols
(defun fsharp-enable-prettify-symbols ()
  (let ((alist '(("->" . #x2192)
                 ("<-" . #x2190)
                 ("|>" . #x22b3)
                 ("<|" . #x22b2))))
    (setq-local prettify-symbols-alist alist)))

(add-hook 'fsharp-mode-hook
          (lambda ()
            (fsharp-enable-prettify-symbols)))

;;; Fvwm
(use-package fvwm-mode)

;;; SQL

(defvar cory/sql-words
  '("ADD " "CONSTRAINT " "ALL " "ALTER " "COLUMN " "TABLE " "AND " "ANY " "AS " "ASC" "BACKUP "
    "DATABASE" "BETWEEN " "CASE " "CHECK " "CREATE " "DATABASE " "INDEX " "OR " "REPLACE " "VIEW "
    "PROCEDURE " "UNIQUE" "VIEW " "DEFAULT " "DELETE " "DESC" "DISTINCT " "DROP " "EXEC " "EXISTS "
    "FOREIGN " "KEY " "FROM " "FULL" "OUTER " "JOIN " "GROUP " "BY " "HAVING " "IN " "INNER "
    "INSERT " "INTO " "SELECT " "IS " "NULL " "NOT" "LEFT " "LIKE " "LIMIT " "ORDER " "PRIMARY "
    "KEY " "RIGHT " "ROWNUM " "TOP " "SET " "TRUNCATE " "UNION" "ALL " "UNIQUE " "UPDATE "
    "VALUES " "VIEW " "WHERE " "ON " "COUNT "))

(defun cory/sql-capf ()
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            cory/sql-words
            :exclusive 'no))))

(add-hook 'sql-mode-hook
	  (lambda ()
	    (setq-local completion-at-point-functions
			(list (cape-super-capf #'cory/sql-capf #'cape-dabbrev) t))))
