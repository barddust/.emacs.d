;;; setup-mu4e.el -*- lexical-binding: t; -*-

(setup mu4e
  (:doc "a tool for dealing with e-mail messages stored in the Maildir-format")
  (:load-path "/usr/share/emacs/site-lisp/mu4e/")
  (:require mu4e)
  (:after mu4e
    (:add-to-list mu4e-contexts
      (let* ((context-name "outlook")
             (dir-name (concat "/" context-name)))
        (make-mu4e-context
         :name context-name
         :match-func
         `(lambda (msg)
            (when msg
              (string-match-p
  	           ,(concat "^" dir-name)
  	           (mu4e-message-field msg :maildir))))
         :vars
         `((user-mail-address    . "mrdust1880@outlook.com")
           (user-full-name       . "Mr.Dust")
           (mu4e-sent-folder     . ,(concat dir-name "/Sent"))
           (mu4e-drafts-folder   . ,(concat dir-name "/Drafts"))
           (mu4e-trash-folder    . ,(concat dir-name "/Deleted"))
           (mu4e-refile-folder   . ,(concat dir-name "/存档"))
           (mu4e-compose-signature . "Dare to know.")))))
    (:option mu4e-change-filenames-when-moving t
             mu4e-get-mail-command "update-mail"
             gnus-blocked-images nil
             ))
  )

(provide 'setup-mu4e)
;;; setup-mu4e.el ends
