;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define new prefix command
(define-prefix-command 'my-prefix-command)
(global-set-key (kbd "C-v") 'my-prefix-command)

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

;; Unset key for frequent mistyped press
(global-unset-key (kbd "C-x DEL"))

(provide 'init-keyBinding)




