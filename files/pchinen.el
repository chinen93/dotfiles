
(message " ===============================================  algo pelo orgmode  ================================================")

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;====================================================================
;;    Layout
;;====================================================================
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Show an notication for invalid operations
(setq visible-bell t)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

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

;; Load theme
(use-package monokai-theme
  :ensure t
  :config
  (progn (message "Monokai Theme")
         (message "Loaded")
         (load-theme 'monokai t)
         (set-background-color "#121212")))

(setq c-default-style "linux"
      c-basic-offset 4)

;; use the python 3.1
(setq py-python-command "/usr/bin/python3.1")

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
   ))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

(use-package helm
  :ensure t
  :config
  (progn
    (require 'helm-config)
    (helm-autoresize-mode 1)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)))

(use-package guide-key
  :ensure t
  :config                    
  (progn (message "Guide Key")
         (message "Loaded")
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
  (progn (message "Nyan Mode")
         (message "Loaded")
         (nyan-mode 1)))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :config
  (progn (message "Expand Region")
         (message "Loaded")))

(defun my/reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reaload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))

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

(add-to-list 'auto-mode-alist '(".emacs" . lisp-mode))
