;;; setup-dirvish.el -*- lexical-binding: t; -*-

(setup dirvish
  (:doc "An improved version of the Emacs inbuilt package Dired")
  (:url "dirvish" "https://github.com/alexluigit/dirvish")
  (:require dirvish)
  (:load-path "extensions")
  (:also-load dirvish-icons
              dirvish-emerge
              dirvish-quick-access
              dirvish-subtree)
  (:option dirvish-quick-access-entries
           '(("h" "~/")
             ("e" "~/.emacs.d/")
             ("g" "~/gh-repo/")
             ("n" "~/Note/")
             ("G" "~/.emacs.d/etc/gnus/")))
  (defun dust/dirvish--truncate-line (&rest _)
    (setq-local truncate-lines t))
  (dirvish-emerge-define-predicate is-dir
    "If item is a directory"
    (equal (car type) 'dir))
  (:option dirvish-use-header-line nil
           dirvish-attributes '(subtree-state all-the-icons file-size)
           delete-by-moving-to-trash t
           dirvish-mode-line-height 21
           dired-listing-switches
           "-l --almost-all --human-readable --group-directories-first --no-group --time-style=iso"
           dirvish-emerge-groups
           '(("Hidden" (regex . "^\\."))
             ("Directory" (predicate . is-dir))
             ("Documents" (extensions "pdf" "tex" "bib" "epub"))
             ("Video" (extensions "mp4" "mkv" "webm"))
             ("Picture" (extensions "jpg" "png" "svg" "gif"))
             ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
             ("Archive" (extensions "gz" "rar" "zip"))
             ("Org" (extensions "org"))
             ("Emacs Lisp" (extensions "el"))
             ("Python" (extensions "py"))
             ("Files" (regex . ".*"))))
  (:option dirvish-override-dired-mode 1)
  (:with-hook dirvish-find-entry-hook
    (:hook dust/dirvish--truncate-line))
  (:with-mode dirvish-emerge-mode
    (:hook-into dirvish-setup))
  (:modalka "C-x C-d" dirvish)
  (:bind-into dirvish-mode-map
    "a" dirvish-quick-access
    "j" dired-next-line
    "k" dired-previous-line
    "f" dired-goto-file
    "b" dired-up-directory
    "n" dirvish-emerge-next-group
    "p" dirvish-emerge-previous-group
    "TAB" dirvish-subtree-toggle
    "M-t" dirvish-layout-toggle))

(provide 'setup-dirvish)
;;; setup-dirvish.el ends
