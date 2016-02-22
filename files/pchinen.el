
(message " ===============================================  Inicialização das Configurações  ================================================")



(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Show matching parenthesis. 
(show-paren-mode t)

;; Current line & column of cursor in the mode line (bar at the bottom)
(line-number-mode 1)
(setq column-number-mode t)

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

(setq initial-scratch-message "
;; ***********************************************
;; ******************* SCRATCH *******************
;; ***********************************************
;;
")

;; Open something when emacs starts
(if (file-exists-p "~/git/org/help.org")
    (progn(find-file "~/git/org/help.org")))

;;====================================================================
;;    Layout
;;====================================================================
;; Turn off mouse interface early in startup to avoid momentary display
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

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
       (latex . t)
       )))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c r" . org-capture)))

(setq org-structure-template-alist
      '(("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")))

(setq org-directory "~/git/org")
(setq org-default-notes-file "~/git/org/organizer.org")

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
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-1" . helm-swoop)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-swoop
  :ensure t
  :init
  (progn
    (message "Helm Swoop - Loaded"))
  :bind ("C-1" . helm-swoop))

(use-package magit
  :ensure t
  :init
  (progn
    (message "Magit - Loaded")))

(use-package guide-key
  :ensure t
  :config                    
  (progn
    (message "Guide Key - Loaded")
    (setq guide-key/guide-key-sequence nil)
    (defun enable-guide-key ()
      (interactive)
      (guide-key-mode 1)
      (setq guide-key/guide-key-sequence t)
      (message "Guide Key enabled"))
    (defun disable-guide-key ()
      (interactive)
      (guide-key-mode -1)
      (setq guide-key/guide-key-sequence nil)
      (message "Guide Key disabled"))
    (global-set-key (kbd "C-c =") 'enable-guide-key)
    (global-set-key (kbd "C-c -") 'disable-guide-key)))

;; Nyan Mode
(use-package nyan-mode
  :ensure t
  :config
  (progn
    (message "Nyan Mode - Loaded")
    (nyan-mode 1)))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  :config
  (progn
    (message "Expand Region - Loaded")))

(use-package company
  :ensure t
  :config
  (progn
    (message "Company - Loaded")
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package ace-jump-mode
  :ensure t)

(use-package yasnippet
 :ensure t
 :config
 (progn
   (yas-global-mode)
   (message "Yasnippet - Loaded")))

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
(global-set-key [(f1)] 'other-window)



(setq c-default-style "linux"
      c-basic-offset 4)

;; use the python 3.1
(setq py-python-command "/usr/bin/python3.1")

(use-package projectile  
  :ensure t)

(message " ===============================================  Fim das Configurações  ================================================")
