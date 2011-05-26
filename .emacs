(add-to-list 'load-path "~/.emacs.d")
(require 'php-mode)
;; To use abbrev-mode, add lines like this:

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-hook 'php-mode-hook 'my-php-mode-stuff)

(defun my-php-mode-stuff ()
  (setq c-auto-newline t)
  (local-set-key (kbd "C-c h") 'my-php-symbol-lookup))


 (defun company-my-php-backend (command &optional arg &rest ignored)
    (case command
      ('prefix (and (eq major-mode 'php-mode)
                    (company-grab-symbol)))
      ('sorted t)
      ('candidates (all-completions
                    arg 
                    (if (and (boundp 'my-php-symbol-hash)
                             my-php-symbol-hash)
                        my-php-symbol-hash

                      (message "Fetching completion list...")

                      (with-current-buffer
                          (url-retrieve-synchronously "http://php.net/quickref.php")

                        (goto-char (point-min))

                        (if (re-search-forward "<!-- result list start -->" nil t)
                            (let ((end (save-excursion
                                         (if (re-search-forward "<!-- result list end -->" nil t)
                                             (point)))))
                              (if end
                                  (let ((hash (make-hash-table)))
                                    (while (re-search-forward ">\\([^<]+\\)</a>" end t)
                                      (puthash (match-string 1) t hash))
                                    (setq my-php-symbol-hash hash)))))))))))


(defun my-php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")

      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))
;;(require 'php-electric)

(defun clean-php-mode ()
(interactive)
(php-mode)
(setq c-basic-offset 2) ; 2 tabs indenting
(setq indent-tabs-mode nil)
(setq fill-column 78)
(c-set-offset 'case-label '+)
(c-set-offset 'arglist-close 'c-lineup-arglist-operators))
(c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
(c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values

;; run php lint when press f8 key
;; php lint
(defun phplint-thisfile ()
(interactive)
(compile (format "php -l %s" (buffer-file-name))))
(add-hook 'php-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-c") 'phplint-thisfile)))
(add-hook 'php-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c h") 'my-php-symbol-lookup)))

;; end of php lintv

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t                    (self-insert-command (or arg 1))) ))
(global-set-key "%" `goto-match-paren)

;;enable line and column numbering
(line-number-mode 1)
(column-number-mode 1)
;; goto line is M-x g
(global-set-key "\M-g" 'goto-line)


;;put backup files in their own directory instead of the working directory
;;and version them for several versions
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

(setq c-auto-newline t)

;; This function toggles the truncate-lines variable
(defun toggle-truncate-lines()
  "toggle the truncate-lines variable between true and false"
  (interactive)
  (set-variable 'truncate-lines (not truncate-lines))
  (set-variable 'truncate-partial-width-windows (not truncate-partial-width-windows))
)

;; Map toggle truncate lines to F10
(global-set-key (kbd "C-c C-t") 'toggle-truncate-lines)

(require 'flymake)

(defun flymake-php-init ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	  (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(add-to-list 'flymake-err-line-patterns
  '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

;; Drupal-type extensions
(add-to-list 'flymake-allowed-file-name-masks '("\\.module$" flymake-php-init))
(add-to-list 'flymake-allowed-file-name-masks '("\\.install$" flymake-php-init))
(add-to-list 'flymake-allowed-file-name-masks '("\\.inc$" flymake-php-init))
(add-to-list 'flymake-allowed-file-name-masks '("\\.engine$" flymake-php-init))

(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))
(define-key php-mode-map (kbd "C-c C-e") 'flymake-goto-prev-error)
(define-key php-mode-map (kbd "C-c C-d") 'flymake-goto-next-error)

(defvar my-func-history nil)

(defun compile-tags (str)
  "compile etags for the current project"
  (interactive
  (list
  (read-from-minibuffer "What is the root of the project: "  (car my-func-history) nil nil 'my-func-history)))

  (interactive)
  (cd (car my-func-history))
  (compile "find . -name \"*.[chCHp]\" -print | etags -"))
(global-set-key (kbd "C-c C-y") 'compile-tags)

(require 'auto-complete)
(require 'anything)
(require 'php-completion)

(php-completion-mode t)
(define-key php-mode-map (kbd "C-c C-g") 'phpcmp-complete)

 (add-hook  'php-mode-hook
            (lambda ()
              (when (require 'auto-complete nil t)
                (make-variable-buffer-local 'ac-sources)
                (add-to-list 'ac-sources 'ac-source-php-completion)
                ;; if you like patial match,
                ;; use `ac-source-php-completion-patial' instead of `ac-source-php-completion'.
                ;; (add-to-list 'ac-sources 'ac-source-php-completion-partial)
                (auto-complete-mode t))))


