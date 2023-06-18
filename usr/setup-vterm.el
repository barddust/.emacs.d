(setup (:git "vterm" "https://github.com/akermu/emacs-libvterm")
  (:doc "A fully-fledged terminal emulator inside GNU Emacs")
  (:require vterm)
  (:option vterm-buffer-name-string "vterm @ %s")
  (:hook (lambda () (modalka-mode 0)))
  (advice-add 'yank
              :filter-args
              (lambda (arg)
                (kill-new (simpleclip-get-contents))
                arg)))

(provide 'setup-vterm)
