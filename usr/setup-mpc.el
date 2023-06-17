;;; setup-mpc.el -*- lexical-binding: t; -*-


(setup simple-mpc
  ;; (:disabled)
  (:doc "A GNU Emacs major mode that acts as a front end to mpc.")
  (:url "simple-mpc" "https://github.com/jorenvo/simple-mpc")
  (:require simple-mpc)
  (:option simple-mpc-arguments "--host=/home/mrdust/.config/mpd/socket"
           simple-mpc-playlist-format "%title%\t%artist%\t%time%"
           simple-mpc-table-separator "\t"))


(provide 'setup-mpc)
;;; setup-mpc.el ends
