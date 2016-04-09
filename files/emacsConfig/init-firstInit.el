;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start-up-emacs is set: do nothing
(if (boundp 'start-up-emacs)
    (progn
      ;; True Body
      (message "Emacs is already up"))
  
  ;; start-up-emacs is not set: start server and set start-up-emacs
  (progn
    (server-start)

    (setq start-up-emacs t)
    (message "Emacs is NOT up, so i started it")))

(provide 'init-firstInit)
