;;; init.el -*- lexical-binding: t; -*-

;;; Packages
(defun dust/package-installed (pkg)
  "If packages is installed"
  (file-exists-p (expand-file-name pkg user-module-directory)))

(defun dust/package-install (pkg url)
  "Install pacakges by Git"
  (let ((dir (expand-file-name pkg user-module-directory)))
    (unless (dust/package-installed dir)
      (message "Fetching %s..." pkg)
      (message "%s"
               (shell-command-to-string
                (format "git --no-pager clone \"%s\" \"%s\""
                        url
                        dir)))
      (message "Fetched %s." pkg))))

(defun dust/packge-update (pkg &optional string)
  "Update single package.
If STRING is non-nil, return the update COMMAND string
especially for `dust/package-update-all'."
  (if (dust/package-installed pkg)
      (let ((cmd (format "echo \"Updating: %s...\"\ncd \"%s\"\ngit pull\n"
                         pkg pkg)))
        (if string
            cmd
          (shell-command-to-string cmd)))
    (message "No such package: %s" pkg)
    ""))

(defun dust/package-update-all (&optional script)
  "Update all packages.
SCRIPT is the temporary location of shell script."
  (interactive)
  (let ((tmp-script (or script "/tmp/emasc.update.modules")))
    (with-temp-file tmp-script
      (insert "#! /bin/bash\n")
      (dolist (pkg (directory-files user-module-directory nil "[^.]"))
        (insert (dust/package-update pkg t)))
      (insert "echo \"DONE.\""))
    (async-shell-command (format "bash %s" tmp-script))))

;;; SetupEl
(dust/package-install "setup" "https://git.sr.ht/~pkal/setup")
(add-to-list 'load-path (expand-file-name "setup" user-module-directory))

(require 'setup)

(setup-define :doc
  (lambda (_)
    nil)
  :documentation "Document for the setup package."
  :repeatable t)

(setup-define :defer
  (lambda (&optional time)
    `(run-with-idle-timer ,(or time 1) nil
                          (lambda () (require ',(setup-get 'feature)))))
  :documentation "Delay loading the feature until a certain amount of idle time has passed.")

(setup-define :hooks
  (lambda (slot func)
    `(add-hook (intern (concat ,(symbol-name slot)
                               "-hook"))
               #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :load-after
    (lambda (features &rest body)
      (let ((body `(progn
                     (require ',(setup-get 'feature))
                     ,@body)))
        (dolist (feature (if (listp features)
                             (nreverse features)
                           (list features)))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES."
  :indent 1)

(setup-define :add-to-list
  (lambda (lst &rest items)
    (let (body)
      (dolist (item items)
        (push `(add-to-list ',lst ,item) body))
      (cons 'progn (reverse body))))
  :documentation "Load the current feature after FEATURES."
  :debug '(sexp &rest sexp)
  :indent 1)

(setup-define :load-path
  (lambda (object)
    `(add-to-list 'load-path ,object))
  :documentation "Update load-path."
  :repeatable t)

(setup-define :custom
  (lambda (var val)
    `(customize-set-variable ',var ,val))
  :documentation "Customize variables."
  :debug '(sexp form)
  ;; :after-loaded t
  :repeatable t)

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func)
                        '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :after
  (lambda (feature &rest body)
    `(with-eval-after-load ',feature
       ,@body))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :modalka
  (lambda (key command)
    `(define-key modalka-mode-map ,key ,command))
  :documentation "Bind KEY to COMMAND on Modalka Map."
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(setup-define :bind-into-after
  (lambda (feature-or-map feature &rest rest)
    (let ((keymap (if (string-match-p "-map\\'"
                                      (symbol-name feature-or-map))
                      feature-or-map
                    (intern (format "%s-map" feature-or-map))))
          (body `(',feature with-eval-after-load)))
      (dotimes (i (/ (length rest) 2))
        (push `(define-key ,keymap
                 (kbd ,(format "%s" (nth (* 2 i) rest)))
                 #',(nth (1+ (* 2 i)) rest))
              body))
      (reverse body)))
  :documentation "Bind into keys into the map of FEATURE-OR-MAP after FEATURE.
The arguments REST are handled as by `:bind'.
The whole is wrapped within a `with-eval-after-load'."
  :debug '(sexp sexp &rest form sexp)
  :ensure '(nil nil &rest kbd func)
  :indent 1)

(setup-define :diminish
  (lambda (key &optional val after)
    (let ((-macro (if after
                      `(with-eval-after-load ',after)
                    '(progn))))
      (-snoc -macro `(diminish ',key ,val))))
  :documentation "Bind KEY to COMMAND on Modalka Map.")

(setup-define :url
  (lambda (pkg url)
    `(let ((dir (expand-file-name ,pkg user-module-directory)))
       (dust/package-install ,pkg ,url)
       (add-to-list 'load-path dir)))
  :documentation "Url to fetch packages"
  :repeatable t)

(setup-define :disabled
  #'setup-quit
  :documentation "Unconditionally abort the evaluation of the current body.")

;;; Modules
(setup cl-lib
  (:doc "Common Lisp Library")
  (:require cl-lib))

(setup dash
  (:doc "A modern list API for Emacs.")
  (:url "dash" "https://github.com/magnars/dash.el")
  (:require dash))

(setup s
  (:doc "String manipulation library")
  (:url "s" "https://github.com/magnars/s.el")
  (:require s))

(setup f
  (:doc "Modern APIs for working with files and directories")
  (:url "f" "https://github.com/rejeep/f.el")
  (:require f))

(setup diminish
  (:doc "Hiding or abbreviation of the mode lighters of minor-modes.")
  (:url "diminish" "https://github.com/myrjola/diminish.el")
  (:require diminish)
  (:diminish overwrite-mode)
  (:diminish eldoc-mode nil eldoc))

(setup all-the-icons
  (:doc "A collection of various Icon Fonts within Emacs")
  (:url "all-the-icons" "https://github.com/domtronn/all-the-icons.el")
  (:require all-the-icons))

(setup no-littering
  (:doc "Help keeping `~/.config/emacs' clean.")
  (:url "no-littering" "https://github.com/emacscollective/no-littering"
        "compat" "https://github.com/emacsmirror/compat")
  (:option no-littering-etc-directory user-config-directory
           no-littering-var-directory user-data-directory)
  (:require no-littering)
  (:option auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
           custom-file (no-littering-expand-etc-file-name "custom.el")
           ac-comphist-file (no-littering-expand-var-file-name "ac-comphist.dat")
           recentf-save-file (no-littering-expand-var-file-name "recentf"))
  ;; (when (fboundp 'startup-redirect-eln-cache)
  ;;   (startup-redirect-eln-cache
  ;;    (convert-standard-filename
  ;;     (no-littering-expand-var-file-name  "eln-cache/"))))
  )

(setup modalka-mode
  (:doc "A building kit to help switch to modal editing in Emacs.")
  (:url "modalka" "https://github.com/mrkkrp/modalka")
  (:require modalka)
  (:diminish modalka-mode)
  (:hook-into minibuffer-mode)
  (:option modalka-global-mode 1)
  (:global "RET" newline-and-indent)
  (:bind-into messages-buffer-mode-map "q" quit-window)

  
  ;; cursor's movement
  (:modalka "C-h" backward-char
            "C-j" next-line
            "C-k" previous-line
            "C-l" forward-char
            "C-e" backward-word
            "C-r" forward-word)
  ;; cut and copy
  ;; (:modalka "C-w" kill-ring-save
  ;;           "M-w" kill-region)
  ;; mark (temporary)
  (:modalka "C-v C-v" set-mark-command
            "C-v C-h" mark-whole-buffer)
  ;; screen page
  (:modalka "M-r" scroll-up-command
            "M-e" scroll-down-command)
  ;; undo/redo
  (:modalka "C-z" undo
            "M-z" undo-redo)
  ;; adjust screen
  (:modalka "C-t" recenter-top-bottom
            "M-t" move-to-window-line-top-bottom)
  ;; comment
  (:modalka "C-q" comment-line
            "M-t" move-to-window-line-top-bottom)
  ;; save file
  (:modalka "C-s" save-buffer)
  ;; newline
  (defun newline-and-indent-1 ()
    (interactive)
    (end-of-line)
    (newline-and-indent))
  (defun newline-and-indent-2 ()
    (interactive)
    (beginning-of-line)
    (newline-and-indent)
    (previous-line))
  (:modalka "C-o" newline-and-indent-1
            "M-o" newline-and-indent-2))

(setup coding-system
  (:doc "Coding-System settings")
  (:option locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-buffer-file-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-next-selection-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8))

(setup editing-system
  (:doc "Editing-System settings")
  (:option global-visual-line-mode 1
           delete-selection-mode 1
           ;; electric-pair-mode 1
           indent-tabs-mode nil
           tab-width 4)
  (fset 'yes-or-no-p 'y-or-n-p)
  (:diminish visual-line-mode)
  
  (setup mwim
    (:doc "Operations of Cursor moving")
    (:url "mwim" "https://github.com/alezost/mwim.el")
    (:require mwim)
    (:modalka "C-a" mwim-beginning-of-code-or-line
              "C-b" mwim-end-of-code-or-line))

  (setup phi-search
    (:doc "Another incremental search & replace")
    (:url "phi-search" "https://github.com/zk-phi/phi-search")
    (:require phi-search phi-replace)
    ;; phi-search
    (:modalka "C-." phi-search
              "C-," phi-search-backward)
    (:option phi-search-limit 10000)
    ;; phi-replace
    (:modalka "C-f C-r" phi-replace-query))

  (setup autorevert
    (:doc "Revert buffers when files on disk change")
    (:defer 1)
    (:option global-auto-revert-mode 1))

  (setup savehist
    (:doc "Save minibuffer history")
    (:defer 1)
    (:option savehist-additional-variables '(mark-ring
                                             global-mark-ring
                                             search-ring
                                             regexp-search-ring
                                             extended-command-history)
             savehist-autosave-interval 300
             history-length 1000
             kill-ring-max 300
             history-delete-duplicates t)
    (:option savehist-mode 1))

  (setup saveplace
    (:doc "automatically save place in files")
    (:defer 1)
    (:option save-place-mode 1)))

(setup theme
  (:doc "theme settings")
  (:url "dracula-theme" "https://github.com/dracula/emacs")
  (:require dracula-theme)
  (load-theme 'dracula t))

(setup font
  (:doc "Fonts setting")
  
  ;; default fonts
  (set-face-attribute 'default nil
                      :font (font-spec :family "FantasqueSansM Nerd Font"
                                       :size 20))

  ;; unicode
  (set-fontset-font t 'unicode
                    (font-spec :family "WenQuanYi Zen Hei Mono"))

  ;; cn
  (set-fontset-font t '(#x4e00 . #x9fff)
                    (font-spec :family "思源宋体"))

  )

(setup rime
  (:doc "RIME input method in Emacs")
  (:url "rime" "https://github.com/DogLooksGood/emacs-rime"
        "posframe" "https://github.com/tumashu/posframe")
  (:custom default-input-method "rime")
  (:require rime)
  (:after rime
    (defun my-predicate-punctuation-after-space-cc-p ()
      (let* ((start (save-excursion
                      (re-search-backward
                       "[^\s]"
                       nil
                       t)))
             (string (buffer-substring
                      (if start start 1)
                      (point))))
        (string-match "\\cc +" string)))
    (:option rime-inline-ascii-trigger 'shift-l
             rime-show-preedit 'inline
             rime-show-candidate 'posframe)
    (:option rime-disable-predicates
             '(rime-predicate-prog-in-code-p
               my-predicate-punctuation-after-space-cc-p
               rime-predicate-after-alphabet-char-p
               rime-predicate-org-in-src-block-p))
    )
  (:modalka "C-<SPC>" toggle-input-method)
  ;; (:hooks kill-emacs rime-lib-finalize)
  )

(setup simpleclip
  (:doc "Simplified access to the system clipboard in Emacs.")
  (:url "simpleclip" "https://github.com/rolandwalker/simpleclip")
  (:require simpleclip)
  (simpleclip-mode 1)
  (:modalka "C-w" simpleclip-copy
            "M-w" simpleclip-cut
            "C-y" simpleclip-paste)
  (advice-add 'simpleclip-copy
              :after
              (lambda (&rest args)
                (when (region-active-p)
                  (keyboard-quit))))
  )

(setup helpful
  (:doc "An alternative to the built-in Emacs help")
  (:url "helpful" "https://github.com/Wilfred/helpful"
        "elisp-refs" "https://github.com/Wilfred/elisp-refs")
  (:require helpful)
  (:option helpful-max-buffers 1)
  (:modalka "C-p k" helpful-key
            "C-p f" helpful-callable
            "C-p v" helpful-variable
            "C-p t" help-with-tutorial)
  )

(setup window
  (:doc "Operations about windows")
  (:url "winum" "https://github.com/deb0ch/emacs-winum")
  (:require window winum)
  
  ;; window
  (defun split-window-right-for-buffer (buffer)
    (interactive "bChoose buffer: \n")
    (select-window (split-window-right))
    (switch-to-buffer buffer))
  (defun split-window-below-for-buffer (buffer)
    (interactive "bChoose buffer: \n")
    (select-window (split-window-below))
    (switch-to-buffer buffer))
  (:modalka "C-x 2" split-window-below-for-buffer
            "C-x 3" split-window-right-for-buffer)
  ;; winum
  (:modalka "M-0" winum-select-window-0
            "M-1" winum-select-window-1
            "M-2" winum-select-window-2
            "M-3" winum-select-window-3
            "M-4" winum-select-window-4
            "M-5" winum-select-window-5
            "M-6" winum-select-window-6
            "M-7" winum-select-window-7
            "M-8" winum-select-window-8
            "M-9" winum-select-window-9)
  (:option winum-auto-assign-0-to-minibuffer t
           winum-auto-setup-mode-line t)
  (:option winum-mode 1))

(setup smartparens
  (:doc "A minor mode for dealing with pairs in Emacs.")
  (:url "smartparens" "https://github.com/Fuco1/smartparens")
  (:require smartparens-config)
  (:diminish smartparens-mode)
  (:hooks prog-mode turn-on-smartparens-strict-mode)
  (:option smartparens-global-mode 1)
  (:bind-into smartparens-mode-map
    "M-a" sp-beginning-of-sexp
    "M-b" sp-end-of-sexp

    "M-s" sp-backward-up-sexp
    "M-d" sp-down-sexp

    "M-\\" sp-unwrap-sexp
    "M-]" sp-forward-slurp-sexp
    "M-[" sp-backward-slurp-sexp
    "M-S-[" sp-forward-barf-sexp
    "M-S-]" sp-backward-barf-sexp

    "M-m" sp-select-next-thing
    "C-d C-l" sp-kill-whole-line

    "M-j" sp-next-sexp
    "M-k" sp-previous-sexp
    "M-l" sp-forward-symbol
    "M-h" sp-backward-symbol))

(setup minibuffer-enhencements
  (:doc "Enhencements of minibuffer,
based on vertico, orderless, marginalia, embark and consult")

  (:hooks minibuffer-setup
          (lambda ()
            (when (eq this-command 'eval-expression)
              (corfu-mode 1)
              (smartparens-mode 1))))

  (setup vertico
    (:doc "a performant and minimalistic vertical completion UI")
    (:url "vertico" "https://github.com/minad/vertico")
    (:load-path (f-expand "vertico/extensions" user-module-directory))
    (:require vertico vertico-directory vertico-mouse)
    (:option vertico-scroll-margin 0
             vertico-cycle t
             vertico-resize t)
    (:option vertico-mode 1
             vertico-mouse-mode 1)
    (:bind-into-after vertico-map vertico
                      "RET" vertico-directory-enter
                      "DEL" vertico-directory-delete-char
                      "C-DEL" vertico-directory-delete-word
                      "M-DEL" vertico-directory-up))

  (setup orderless
    (:doc "An orderless completion style.")
    (:url "orderless"  "https://github.com/oantolin/orderless")
    (:require orderless)
    (:option completion-styles '( orderless
                                  substring
                                  partial-completion
                                  basic)
             completion-category-defaults nil
             completion-category-overrides nil
             completion-category-defaults nil
             completion-category-overrides '((file (styles . (partial-completion))))))

  (setup marginalia
    (:doc "Add marginalia to the minibuffer completions.")
    (:url "marginalia" "https://github.com/minad/marginalia"
          "all-the-icons-completion" "https://github.com/iyefrat/all-the-icons-completion")
    (:require marginalia all-the-icons-completion)
    (:hooks marginalia-mode all-the-icons-completion-marginalia-setup)
    (:option marginalia-mode 1))

  (setup embark
    (:doc "Make it easy to choose a command to run based on what is near point")
    (:url "embark" "https://github.com/oantolin/embark")
    (:require embark)
    (:option prefix-help-command #'embark-prefix-help-command)
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    (:modalka  "C-/" embark-act
               "C-?" embark-dwim))
  
  (setup consult
    (:doc "Search and navigation commands based on the Emacs completion function")
    (:url "consult" "https://github.com/minad/consult")
    (:require consult consult-imenu)
    (:modalka "C-f C-f" consult-line
              "C-f C-l" consult-goto-line
              "C-f C-m" consult-mark
              "C-f C-S-m" consult-global-mark
              "C-f C-d" consult-ripgrep
              "C-f C-s" consult-imenu))
  )

(setup corfu
  (:doc "Corfu enhances in-buffer completion with a small completion popup.")
  (:url "corfu" "https://github.com/minad/corfu"
        "kind-all-the-icons" "https://github.com/Hirozy/kind-all-the-icons")
  (:load-path (f-expand "corfu/extensions" user-module-directory))
  (:autoload corfu-mode)

  (:with-feature corfu-popupinfo
    (:autoload corfu-popupinfo-mode)
    (:hook-into corfu-mode))

  (:with-feature corfu-history
    (:autoload corfu-history-mode)
    (:hook-into corfu-mode))
  
  (:option tab-always-indent 'complete
           corfu-auto t
           corfu-auto-delay 0
           corfu-auto-prefix 1
           corfu-popupinfo-delay 0)

  (:after corfu
    (:bind-into corfu-map
    "C-r" corfu-scroll-down
    "C-e" corfu-scroll-up
    "C-j" corfu-next
    "C-k" corfu-previous
    "M-n" corfu-popupinfo-scroll-up
    "M-p" corfu-popupinfo-scroll-down)
    
    (:require kind-all-the-icons)
    (:add-to-list corfu-margin-formatters 'kind-all-the-icons-margin-formatter)
    )
  
  (:hook-into prog-mode ielm-mode)
  )

(setup eaf
  (:doc "A framework that revolutionizes the graphical capabilities of Emacs.")
  (:url "emacs-application-framework" "https://github.com/emacs-eaf/emacs-application-framework")
  (:require eaf))

(setup application
  (:doc "define entry of kind of apps.")
  (:load-path user-application-directory)
  
  (defun setup-all ()
    (require 'setup-all))

  (defun setup-terminal ()
    (require 'setup-vterm)
    (vterm)
    )

  (defun setup-mpc ()
    (require 'setup-mpc)
    ;; (emms-player-mpd-connect)
    (simple-mpc)
    )

  (defun setup-mu4e ()
    (require 'setup-mu4e)
    (mu4e)
    (mu4e-update-mail-and-index nil)
    )

  (defun setup-dirvish (&optional path)
    (require 'setup-dirvish)
    (message "%s" path)
    (dirvish))
  
  )

(provide 'init)
;;; init.el ends
