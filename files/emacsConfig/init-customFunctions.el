;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/bcompile-pchinen.el ()
  (interactive)

  ;; Compile pchinen.el so emacs starts up faster
  (byte-compile-file "/home/pchinen/git/dotfiles/files/pchinen.el"))

(defun my/open-initial-files ()
  (interactive)
  ;; help file exist?: open it
  (if (file-exists-p "~/git/org/help.org")
      (find-file "~/git/org/help.org"))
  
  ;; pchinen.org exist?: open it
  (if (file-exists-p "~/.pchinen.org")
      (find-file "~/.pchinen.org"))

  ;; Vulcanet User
  (if (equal (user-login-name) "pedro") 
      ;; Vulcanet notes exist?: open it
      (if (file-exists-p "~/vulcanet.org")
	  (find-file "~/vulcanet.org"))))

(defun c-comment-line ()
  ;; Comment line with /* 'line' */
  (interactive)
  (beginning-of-line)
  (insert "/*")
  (end-of-line)
  (insert " */"))

(defun c-uncomment-line ()
  ;; remove first 2 and last 2 characters in line
  ;; if used with (c-comment-line) remove characters that were inserted
  (interactive)
  (beginning-of-line)
  (delete-char 2)
  (end-of-line)
  (backward-char 3)
  (delete-char 3))

(defun my/find-function ()
  ;;
  ;; Find every function in actual file with helm-swoop
  ;;

  (interactive)
  ;; Python
  (setq-local python-function-syntax "\\(#\\|def\\)")

  ;; Concatenate every function syntax
  (setq-local function-syntax (concat python-function-syntax))

  (helm-swoop :$query function-syntax)
  )

(provide 'init-customFunctions)


