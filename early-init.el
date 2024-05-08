;;; -*- lexical-binding: t; -*-

(defconst user-home-directory
  (pcase system-type
    ('windows-nt "%userprofile%/")
    (otherwise "~/"))
  "User Home directory")

;;; Basic settings
;;

;; Speed up starting
(setq gc-cons-percentage 1.0
      gc-cons-threshold most-positive-fixnum
      ;; package-enable-at-startup nil
      inhibit-compacting-font-caches t)

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

(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Coding System
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; GUI
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
;; (add-to-list 'default-frame-alist '(alpha-background . 90))

;; Misc
(setq
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
      ad-redefinition-action 'accept
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      initial-scratch-message nil
      echo-keystrokes 0.25
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'early-init)
