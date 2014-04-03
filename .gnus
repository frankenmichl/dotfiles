;; ==================
;; GNUS configuration
;; ==================

(setq user-mail-address "michael.moese@gmail.com")
(setq user-full-name "Michael Moese")

(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)))
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smt.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "michael.moese@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

(setq gnus-always-read-dribble-file t)

;(setq gnus-summary-thread-gathering-function
;      'gnus-gather-threads-by-subject)

;(defun my-gnus-group-list-subscribed-groups ()
;  "List all subscribed groups with or without un-read messages"
;  (interactive)
;  (gnus-group-list-all-groups 5)
;  )
; (add-hook 'gnus-group-mode-hook
           ;; list all the subscribed groups even they contain zero un-read messages
;           (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
;           )
