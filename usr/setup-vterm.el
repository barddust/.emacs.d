

(setup vterm
  (:doc "A fully-fledged terminal emulator inside GNU Emacs")
  (:url "vterm" "https://github.com/akermu/emacs-libvterm")
  (:require vterm)
  (:option vterm-buffer-name-string "vterm @ %s")
  (:hook (lambda () (modalka-mode 0)))
  ;; (:bind-into vterm-mode
  ;;   "C-b" vterm-end-of-line)
  (advice-add 'yank
              :filter-args
              (lambda (arg)
                (kill-new (simpleclip-get-contents))
                arg))
  )

(provide 'setup-vterm)
