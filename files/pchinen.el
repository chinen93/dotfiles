
(message " ===============================================  Inicialização das Configurações  ================================================")

(show-paren-mode t)

(line-number-mode 1)
(setq column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq initial-scratch-message "
;; ***********************************************
;; ******************* SCRATCH *******************
;; ***********************************************
;;
")

(setq vc-follow-symlinks t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(if (boundp 'start-up-emacs)
    (progn
      ;; True Body
      (message "Emacs is already up"))
  (progn
    ;; False body
    (if (file-exists-p "~/git/org/help.org")
        (find-file "~/git/org/help.org"))
    
    (if (file-exists-p "~/.pchinen.org")
        (find-file "~/.pchinen.org"))
    
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
          (progn
            (visual-line-mode)
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
    (global-set-key (kbd "C-1") 'helm-swoop)))

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

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  :config
  (progn
    (message "Expand Region - Loaded")
    ;; Bind
    (global-set-key (kbd "C-=") 'er/expand-region)))

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

(use-package yasnippet
 :ensure t
 :config
 (progn
   (message "Yasnippet - Loaded")
   ;; Change add Directories when looking for snippets
   (setq yas-snippet-dirs
         (append yas-snippet-dirs
                 ;; Personal Collection
                 '("~/.snippets")))
   (define-key yas-minor-mode-map (kbd "<tab>") nil)
   (define-key yas-minor-mode-map (kbd "TAB") nil)
   (define-key yas-minor-mode-map (kbd "<f3>") 'yas-expand)
   (yas-global-mode)
   ))

(use-package re-builder
 :ensure t
 :config
 (progn
   (message "Rebuilder - Loaded")
   (setq reb-re-synstax 'string)))

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

(use-package flycheck
 :ensure t
 :config
 (progn
   (message "Flycheck - Loaded")
   (global-flycheck-mode)
   ))

(defun my/bcompile-pchinen.el ()
  (interactive)
  (byte-compile-file "/home/pchinen/git/dotfiles/files/pchinen.el"))

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

(global-set-key (kbd "C-s") 'isearch-forward-regexp) 
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "s-q") 'other-window)
(global-set-key (kbd "s-w") 'delete-window)


(global-set-key (kbd "<f2>") nil)
(global-set-key (kbd "<f3>") nil)
(global-set-key (kbd "<f4>") nil)
(global-set-key (kbd "<f5>") nil)
(global-set-key (kbd "<f6>") nil)
(global-set-key (kbd "<f7>") nil)
(global-set-key (kbd "<f8>") nil)
(global-set-key (kbd "<f9>") nil)

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

(add-hook 'prog-mode-hook
          (progn (setq-default indent-tabs-mode nil)))

(setq c-default-style "linux"
      c-basic-offset 4)

;; use the python 3.1
(setq py-python-command "/usr/bin/python3.1")
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(message " ===============================================  Fim das Configurações  ================================================")
