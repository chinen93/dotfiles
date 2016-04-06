
(message " ===============================================  Inicialização das Configurações  ================================================")

;; See the matching pair of parentheses and others characters
(show-paren-mode t)

;; Remove every warning, bell or visual
(setq ring-bell-function 'ignore)

;; Show number of line and column
(line-number-mode 1)
(setq column-number-mode t)

;; Change (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set initial message for *scratch* buffer
(setq initial-scratch-message "
;***********************************************
;******************* SCRATCH *******************
;***********************************************
   
")

;; Follow version controlled files without ask
(setq vc-follow-symlinks t)

;; Remove tool bar at top and scroll bar at right
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Don't show start up message
(setq inhibit-startup-message t)

;; Set directory to hold backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

;; A GNU Emacs library to ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :config (progn
            (exec-path-from-shell-initialize)))

;; Set directory to hold history
(setq savehist-file "~/.emacs.d/savehist")

;; Start mode
(savehist-mode 1)

;; Set configuration
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)

;; Save hist for kill rings, search rings and regex search rings
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(if (boundp 'start-up-emacs)
    (progn
      ;; True Body
      (message "Emacs is already up"))
  (progn
    (server-start)

    (setq start-up-emacs t)
    (message "Emacs is NOT up, so i started it")))

;; Load theme
(use-package monokai-theme
  :ensure t
  :config
  (progn
    (message "Monokai Theme - Loaded")
    (load-theme 'monokai t)
    (set-background-color "#121212")))

(use-package company
  :ensure t
  :config
  (progn
   (setq company-idle-delay 0
          company-echo-delay 0
         company-dabbrev-downcase nil
          company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-transformers '(company-sort-by-occurrence
                                 company-sort-by-backend-importance))
    (message "Company - Loaded")
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package expand-region
:ensure t
  :bind
  ("C-=" . er/expand-region)
  :config
  (progn
    (message "Expand Region - Loaded")
    ;; Bind
    (global-set-key (kbd "C-=") 'er/expand-region)))

(use-package flycheck
 :ensure t
 :config
 (progn
   (message "Flycheck - Loaded")
   (global-flycheck-mode)
   ))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (message "Helm - Loaded")
    (setq helm-candidate-number-limit 100)
   ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-split-window-in-side-p t
          helm-ff-skip-boring-files t)
    (helm-mode)
    (global-set-key (kbd "C-c h") 'helm-mini)
    (global-set-key (kbd "C-h a") 'helm-apropos)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "M-x") 'helm-M-x)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-swoop
  :ensure t
  :init
  (progn
    (message "Helm Swoop - Loaded")
    (setq helm-swoop-speed-or-color t)
    (setq helm-swoop-split-with-multiple-windows t)
    (global-set-key (kbd "C-f") 'helm-swoop)))

(use-package helm-descbinds
  :ensure t
  :init
  (progn
    (message "Helm Describe Bindings - Loaded")))

(use-package keyfreq
  :ensure t
  :config
  (progn
    (message "Keyfreq - Loaded")
    (setq keyfreq-excluded-commands
          '(self-insert-command
            abort-recursive-edit
            forward-char
            backward-char
            previous-line
            next-line))
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

(use-package magit
  :ensure t
  :init
  (progn
    (message "Magit - Loaded")))

;; Nyan Mode
(use-package nyan-mode
  :ensure t
  :config
  (progn
    (message "Nyan Mode - Loaded")
    (nyan-mode 1)))

(use-package org
  :ensure t
  :init
  (progn
    (setq org-return-follows-link t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (sh . t)
       (python . t)
       (R . t)
       (ruby . t)
       (ditaa . t)
       (dot . t)
       (octave . t)
       (sqlite . t)
       (perl . t)
       (latex . t)))
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c r") 'org-capture)))

(setq org-structure-template-alist
      '(("l"
         "#+begin_src emacs-lisp\n?\n#+end_src"
         "<src lang=\"emacs-lisp\">             \n?\n</src>")
        ("t"
         "#+begin_src text\n?\n#+end_src"
         "<src lang=\"text\">\n?\n</src>")))

(setq org-directory "~/git/org")
(setq org-default-notes-file "~/git/org/organizer.org")

(add-hook 'org-mode-hook
          (lambda()
            (visual-line-mode t)
            ))

(use-package re-builder
 :ensure t
 :config
 (progn
   (message "Rebuilder - Loaded")
   (setq reb-re-synstax 'string)))

(use-package yasnippet
 :ensure t
 :config
 (progn
   (message "Yasnippet - Loaded")
   ;; Change add Directories when looking for snippets
   (setq yas-snippet-dirs
         ;; Personal Collection
         '("~/.snippets"))
   (define-key yas-minor-mode-map (kbd "<tab>") nil)
   (define-key yas-minor-mode-map (kbd "TAB") nil)
   (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
   (define-key yas-minor-mode-map (kbd "C-v s") 'yas-insert-snippet)
   (yas-global-mode)
   ))

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

(setq yas-prompt-functions '(shk-yas/helm-prompt))

(defun my/bcompile-pchinen.el ()
  (interactive)
  (byte-compile-file "/home/pchinen/git/dotfiles/files/pchinen.el"))

(defun my/open-initial-files ()
   (interactive)
   (if (file-exists-p "~/git/org/help.org")
       (find-file "~/git/org/help.org"))
  
   (if (file-exists-p "~/.pchinen.org")
       (find-file "~/.pchinen.org"))

   ;; Vulcanet User
   (if (equal (user-login-name) "pedro") 
       (if (file-exists-p "~/vulcanet.org")
           (find-file "~/vulcanet.org"))))

(defun c-comment-line ()
  (interactive)
  (beginning-of-line)
  (insert "/*")
  (end-of-line)
  (insert " */"))

(defun c-uncomment-line ()
  (interactive)
  (beginning-of-line)
  (delete-char 2)
  (end-of-line)
  (backward-char 3)
  (delete-char 3))

(defun my/find-function ()
  (interactive)
  ;; Python
  (setq python-function-syntax "\\(#\\|def\\)")
  (helm-swoop :$query python-function-syntax)
  )

(my/find-function)

(define-prefix-command 'my-prefix-command)
(global-set-key (kbd "C-v") 'my-prefix-command)

(global-set-key (kbd "s-q") 'other-window)
(global-set-key (kbd "s-w") 'delete-window)

(global-set-key (kbd "C-s") 'isearch-forward-regexp) 
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f7>"))
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))


(global-unset-key (kbd "C-x DEL"))

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '((".bashrc" . shell-script-mode)
         (".bash_aliases" . shell-script-mode)
         (".bash_profile" . shell-script-mode)

         
         (".scss" . css-mode)
         ;; File name has no dot.
         ("/[^\\./]*\\'" . fundamental-mode)
         ;; File name ends in ‘.C’.
         ("\\.C\\'" . c++-mode))
       auto-mode-alist))

;; use the python 3.1
(setq py-python-command "/usr/bin/python3.1")
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(use-package company-jedi
  :ensure t
  :config (progn 
            (add-to-list 'company-backends 'company-jedi)))

(message " ===============================================  Fim das Configurações  ================================================")
