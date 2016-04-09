;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook
	  ;; Create hook when org mode is enabled
	  (lambda()
	    (visual-line-mode t)
	    ))


;; use the python 3.1
(setq py-python-command "/usr/bin/python3.1")
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(use-package company-jedi
  :ensure t
  :config (progn 
	    (add-to-list 'company-backends 'company-jedi)))
(provide 'init-hooks)


