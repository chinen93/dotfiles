;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define new prefix command
(define-prefix-command 'my-prefix-command)
(global-set-key (kbd "C-v") 'my-prefix-command)
(global-set-key (kbd "M-v") 'my-prefix-command)

;; Rebind default binding
(global-set-key (kbd "M-v M-s") 'save-buffer)
(global-set-key (kbd "M-v M-f") 'find-file)
(global-set-key (kbd "M-v TAB") 'list-buffers)

;; Go to other window
(global-set-key (kbd "s-q") 'other-window)

;; Delete window in which the cursor is in
(global-set-key (kbd "s-w") 'delete-window)

;; Search key binding
(global-set-key (kbd "C-s") 'isearch-forward-regexp) 
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Unset Key binding F(num)
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f7>"))
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))

;; Unset digit-arguments
(global-unset-key (kbd "C-1"))
(global-unset-key (kbd "C-2"))
(global-unset-key (kbd "C-3"))
(global-unset-key (kbd "C-4"))
(global-unset-key (kbd "C-5"))
(global-unset-key (kbd "C-6"))
(global-unset-key (kbd "C-7"))
(global-unset-key (kbd "C-8"))
(global-unset-key (kbd "C-9"))
(global-unset-key (kbd "C-0"))
(global-unset-key (kbd "C--"))
(global-unset-key (kbd "M-1"))
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(global-unset-key (kbd "M-4"))
(global-unset-key (kbd "M-5"))
(global-unset-key (kbd "M-6"))
(global-unset-key (kbd "M-7"))
(global-unset-key (kbd "M-8"))
(global-unset-key (kbd "M-9"))
(global-unset-key (kbd "M-0"))
(global-unset-key (kbd "M--"))

;; Unset key for frequent mistyped press
(global-unset-key (kbd "C-x DEL"))
(global-unset-key (kbd "C-x f"))

;; Unset every single key chord
(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-e"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-t"))
;; (global-unset-key (kbd "C-u")) ;; digit-argument
(global-unset-key (kbd "C-i"))
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-a"))
;; (global-unset-key (kbd "C-s")) ;; isearch-regexp
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-f"))
;; (global-unset-key (kbd "C-g")) ;; keyboard-quit
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
;; (global-unset-key (kbd "C-l")) ;; recenter-top-botton
(global-unset-key (kbd "C-ç"))
(global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "C-x")) ;; prefix-command
(global-unset-key (kbd "C-c"))
;; (global-unset-key (kbd "C-v")) ;; prefix-command
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-n"))
;; (global-unset-key (kbd "C-m")) ;; in emacs C-m is the same as RET

(global-unset-key (kbd "M-q"))
;; (global-unset-key (kbd "M-w")) ;; kill-ring-save
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-y"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-p"))
(global-unset-key (kbd "M-a"))
(global-unset-key (kbd "M-s"))
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-g"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-z"))
;; (global-unset-key (kbd "M-x")) ;; execute-extended-command
(global-unset-key (kbd "M-c"))
;; (global-unset-key (kbd "M-v")) ;; prefix-command
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-n"))
(global-unset-key (kbd "M-m"))

;; Rebind default binding
(global-set-key (kbd "M-v M-s") 'save-buffer)

;; Go to other window
(global-set-key (kbd "s-q") 'other-window)

;; Delete window in which the cursor is in
(global-set-key (kbd "s-w") 'delete-window)

;; Search key binding
(global-set-key (kbd "C-s") 'isearch-forward-regexp) 
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(provide 'init-keyBinding)




