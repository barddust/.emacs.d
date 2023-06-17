;;; early-init.el -*- lexical-binding: t; -*-

(defconst user-home-directory
  (pcase system-type
    ('windows-nt "%userprofile%/")
    (otherwise "~/"))
  "User Home directory")

(defconst user-data-directory
  (expand-file-name "var" user-emacs-directory)
  "Where to save the emacs data")

(defconst user-config-directory
  (expand-file-name "etc" user-emacs-directory)
  "Where to save the configurations")

(defconst user-module-directory
  (expand-file-name "opt" user-emacs-directory)
  "Where to save the modules")

(defconst user-application-directory
  (expand-file-name "usr" user-emacs-directory)
  "Where to save the apps")

;; Create directories if necessary
(dolist (type '("data" "config" "module" "application"))
  (let* ((var-name (intern (concat "user-" type "-directory")))
         (dir (symbol-value var-name)))
    (unless (file-directory-p dir)
      (make-directory dir))))

;;; Basic settings

(setq
 gc-cons-percentage 1.0
 gc-cons-threshold most-positive-fixnum
 package-enable-at-startup nil
 inhibit-compacting-font-caches t
 
 url-proxy-services '(("http" . "127.0.0.1:7890")
                      ("https" . "127.0.0.1:7890"))
 ring-bell-function 'ignore
 make-backup-files nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
 mouse-wheel-progressive-speed nil
 next-line-add-newlines t
 confirm-kill-processes nil
 enable-recursive-minibuffers t
 frame-inhibit-implied-resize t
 ad-redefinition-action 'accept1

 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 )

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
    
;; Speed up for starting Emacs
(let ((old-file-name-handler-alist file-name-handler-alist))
  ;; https://emacs.nasy.moe/#%E5%88%9D--early-init-
  (setq-default file-name-handler-alist nil)
  (defun restore--default ()
    "Restore gc setting to default."
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist
                               old-file-name-handler-alist))
          inhibit-trace nil))
  (add-hook 'emacs-startup-hook #'restore--default))

(provide 'early-init)
;;; early-init.el ends
