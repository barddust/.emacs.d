;;; -*- lexical-binding: t; -*-

;;;
;;; Packages Archive
;;;

(require 'package)
(require 'package-vc)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


;;;
;;; SetupEl
;;;

(unless (package-installed-p 'setup)
  (package-install 'setup))
(require 'setup)


(setup-define :doc
  (lambda (_)
    nil)
  :documentation "Document for the setup package."
  :repeatable t)

(defvar setup--package-refreshed-p nil
  "A indicator if the package database has been upgraded.")

(defun setup--package-install (pkg)
  (let (pkg-name url)
    (if (consp pkg)
        (setq pkg-name (car pkg)
              url (cadr pkg))
      (setq pkg-name pkg))

    (unless (package-installed-p pkg-name)
      (message "Installing package: %s" pkg-name)
      (if url
          (package-vc-install url nil nil pkg-name)
        (unless setup--package-refreshed-p
          (package-refresh-contents)
          (setq setup--package-refreshed-p t))
        (package-install pkg-name)))))

(setup-define :package
  (lambda (pkg)
    `(setup--package-install ',pkg))
  :documentation "Install package by Git if it hasn't been installed yet.
CON is a list, with package-name as first element,
and Git repository URL as second element.
The first PACKAGE can be used to deduce the feature context."
  :repeatable t
  :shorthand 
  (lambda (heads)
    (let ((pkg (cadr heads)))
      (if (consp pkg)
          (car pkg)
        pkg))))

(setup-define :load-path
  (lambda (path)
    (when (and (stringp path)
               (not (file-name-absolute-p path)))
      (let ((guess-path (directory-files package-user-dir t
                                         (format "%s" (setup-get 'feature)))))
        (when (and guess-path
                   (file-exists-p (car guess-path)))
          (setq path (expand-file-name path
                                       (car guess-path))))))
    `(add-to-list 'load-path ,path t))
  :documentation "Update load-path."
  :repeatable t)

(setup-define :after
  (lambda (features &rest body)
    (let (bodies)
      (dolist (feature (if (listp features) features (list features)))
        (push `(with-eval-after-load ',feature
                 (:with-feature ,feature
                   ,@body))
              bodies))
      (macroexp-progn (nreverse bodies))))
  :documentation "Eval BODY after FEATURES.
If FEATURES is a list, apply BODY to all elements inside."
  :indent 1)

(setup-define :diminish
  (lambda (&optional opt)
    `(diminish ',(setup-get 'mode) ,opt))
  :documentation "Hide the mode-line lighter of the current mode."
  :after-loaded t)

(setup-define :autoload
  (lambda (func)
    (let ((feature (format "%s" (setup-get 'feature))))
      `(autoload ',func ,feature "" t)))
  :documentation "Autoload FUNC in feature."
  :repeatable t)

(setup-define :advice
  (lambda (symbol where arglist &rest body)
    (let ((name (gensym "setup-advice-")))
      `(progn
         (defun ,name ,arglist ,@body)
         (advice-add ',symbol ,where #',name))))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :indent 3)

(setup-define :face
  (lambda (face &rest args)
    (cl-assert (eventp (length args)))
    (let ((ret '(progn)))
      (dotimes (i (/ (length args) 2))
        (push `(set-face-attribute ',face nil ,(nth (* 2 i) args) ,(nth (1+ (* 2 i)) args))
              ret))
      (reverse ret)))
  :documentation "Set FACE's attributes."
  :after-loaded t
  :indent 1)

(setup-define :global
  (lambda (key command)
    `(progn
       (global-set-key ,key ,command)
       (unless (vectorp ,key)
         (define-key minibuffer-mode-map ,key ,command))))
  :documentation "Globally bind KEY to COMMAND, also in minibuffer."
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(setup-define :disabled
  #'setup-quit
  :documentation "Unconditionally abort the evaluation of the current body.")

;;;
;;; Modules
;;;

(setup (:package dash f s ts ht diminish
                 nerd-icons)
  (:doc "Useful tools"))


(setup delsel
  (:doc "Delete selection if you insert")
  (:with-mode delete-selection-mode
    (:hook-into after-init)))


(setup savehist
  (:doc "Save minibuffer history")
  (:option savehist-additional-variables
           '(mark-ring
             global-mark-ring
             search-ring
             regexp-search-ring
             extended-command-history)
           
           savehist-autosave-interval 300
           history-length 1000
           kill-ring-max 300
           history-delete-duplicates t)
  (:hook-into minibuffer-setup))


(setup saveplace
  (:doc "automatically save place in files")
  (:with-hook find-file-hook
    (:hook save-place-mode)))


(setup autorevert
  (:doc "Revert buffers when files on disk change")
  (:with-hook find-file-hook
    (:hook global-auto-revert-mode)))


(setup bindings
  (:doc "Basic key bindings.")
  (define-prefix-command 'dust/goto-map)
  (:global "C-h" backward-char
           "C-j" next-line
           "C-k" previous-line
           "C-l" forward-char
           
           "C-e" backward-word
           "C-r" forward-word

           "C-w" kill-ring-save
           "M-w" kill-region
           
           "M-r" scroll-up-command
           "M-e" scroll-down-command
           
           "C-z" undo
           "M-z" undo-redo
           
           "C-t" recenter-top-bottom
           "M-t" move-to-window-line-top-bottom
           
           "C-q" comment-line
           "C-s" save-buffer
           "C-p" help-command

           "C-f" dust/goto-map))


(setup minibuffer
  (:doc "Settings on minibuffer.")
  (defun dust/minibuffer-setup-hook-func ()
    (when (eq this-command 'eval-expression)
      (corfu-mode t)
      (smartparens-mode t)))
  (:with-hook minibuffer-setup-hook
    (:hook dust/minibuffer-setup-hook-func)))


(setup faces
  (:doc "Faces setting")
  (defun dust/font--setup ()
    ;; default
    (set-face-attribute 'default nil :font "Maple Mono 13")
    ;; unicode
    (set-fontset-font t 'unicode "Symbols Nerd Font")
    ;; emoji
    (set-fontset-font t 'emoji "Noto Color Emoji")
    ;; CJK
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset "LXGW WenKai Mono"))
    ;; fix cn and eng words in Org Mode Table
    (setq face-font-rescale-alist '(("LXGW WenKai Mono". 1.2))))
  (:with-hook window-setup-hook
    (:hook dust/font--setup)))


(setup (:package circadian one-themes dracula-theme)
  (:doc "theme settings")
  (:option circadian-themes '(("7:00" . one-light)
                              ("19:00" . dracula)))
  
  (:with-hook after-init-hook
    (:hook circadian-setup)))


(setup modeline
  (:doc "Custom mode line")
  (:after nerd-icons
    (:option
     (prepend* nerd-icons-mode-icon-alist)
             '((messages-buffer-mode nerd-icons-octicon "nf-oct-log")
               (newsticker-treeview-mode nerd-icons-faicon "nf-fa-rss")
               (newsticker-treeview-list-mode nerd-icons-faicon "nf-fa-rss")
               (newsticker-treeview-item-mode nerd-icons-faicon "nf-fa-rss")
               (nov-mode nerd-icons-codicon "nf-cod-book")
               (yuck-mode nerd-icons-sucicon "nf-custom-common_lisp")
               (typst-ts-mode nerd-icons-faicon "nf-fa-tumblr_square"))

             (prepend* nerd-icons-extension-icon-alist)
             '(("yuck" nerd-icons-sucicon "nf-custom-common_lisp")
               ("db" nerd-icons-faicon "nf-fa-database")))
    )
  
  (defcustom setup--icon-for-file
    '(("books.org" nerd-icons-mdicon "nf-md-bookshelf")
      ("20230617T173209--emacs-配置__page_emacs.org" nerd-icons-sucicon "nf-seti-config"))
    "mode line file icons")

  (defun setup--icon ()
    (let* ((buf (buffer-name))
           (icon (cdr (assoc buf setup--icon-for-file))))
      (if icon
          (apply (car icon) (cdr icon))
        (nerd-icons-icon-for-mode major-mode))))
  
  (:option
   mode-line-format
   '("%e"
     ;; Window mode Number
     (winum-mode (:eval
                  (propertize
                   (format winum-format (winum-get-number-string))
                   'face (let ((colored (face-attribute
                                         'font-lock-constant-face :foreground))
                               (colorless (face-attribute
                                           'default :background)))
                           (if (eq (window-buffer) (current-buffer))
                               (list :background colored :foreground colorless)
                             (list :background colorless :foreground colored))))))
     ;; Select mode indicator
     (select-mode (:eval (propertize " [S]"
                                     'face `(:foreground
                                             ,(face-attribute
                                               'font-lock-string-face :foreground)))))
     " "
     ;; Basic status
     mode-line-mule-info
     (:eval (propertize "%*%+"
                        'face `(:foreground
                                ,(face-attribute
                                  (cond
                                   (buffer-read-only 'error)
                                   ((buffer-modified-p) 'warning)
                                   (t 'default))
                                  :foreground))))
     "  "
     ;; Icons for file or major mode
     (:eval (propertize (setup--icon)
                        'face `(:foreground
                                ,(face-attribute
                                  'font-lock-negation-char-face
                                  :foreground))
                        'help-echo (lambda (&rest _) (format "%s" major-mode))))
     "  "
     ;; Buffer name, fix too long name for denote
     (:eval
      (propertize
       (let ((buffile (buffer-file-name)))
         (if (and buffile
                  (string-prefix-p (expand-file-name denote-directory) 
                                   buffile)
                  (denote-filename-is-note-p buffile))
             (denote-retrieve-filename-title buffile)
           (buffer-name)))
       'face 'font-lock-keyword-face
       'help-echo (lambda (&rest _) (or (buffer-file-name) ""))))

     "  " 
     mode-line-percent-position
     " "
     (list "("
           ("" minor-mode-alist)
           ")")
     mode-line-misc-info
     mode-line-end-spaces)))


(setup simple
  (:doc "Basic editing commands for Emacs")
  (:option tab-width 4
           tab-always-indent 'complete
           indent-tabs-mode nil
           global-visual-line-mode t
           use-short-answers t ; `y-or-n-p' to replace `yes-or-no-p'
           )

  (:with-mode visual-line-mode
    (:diminish))

  (defun dust/newline-and-indent-1 ()
    (interactive)
    (end-of-line)
    (newline-and-indent))
  (defun dust/newline-and-indent-2 ()
    (interactive)
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    ;; (indent-to (current-indentation))
    )

  (:global "C-a" move-beginning-of-line
           "C-b" move-end-of-line
           "C-r" forward-word
           "C-e" backward-word
           "C-<backspace>" backward-kill-word
           "C-<delete>" kill-word
           "C-o" dust/newline-and-indent-1
           "M-o" dust/newline-and-indent-2))


(setup select-mode
  (:doc "shortcuts in select mode")
  (:load-path "~/Repository/select-mode")
  (:autoload global-select-mode)
  (global-select-mode)
  (:global "C-v" set-mark-command))


(setup window
  (:doc "Operations about windows")
  (defun dust/split-window-right (buffer)
    (interactive "bChoose buffer: \n")
    (select-window (split-window-right))
    (switch-to-buffer buffer))
  (defun dust/split-window-below (buffer)
    (interactive "bChoose buffer: \n")
    (select-window (split-window-below))
    (switch-to-buffer buffer))
  (:global "C-x 2" dust/split-window-below
           "C-x 3" dust/split-window-right))


(setup (:package winum)
  (:doc "Navigate Emacs windows and frames using numbers ")
  (:hook-into window-setup-hook)
  (:global "M-0" winum-select-window-0 ; mainly minibuffer
           "M-1" winum-select-window-1
           "M-2" winum-select-window-2
           "M-3" winum-select-window-3
           "M-4" winum-select-window-4
           "M-5" winum-select-window-5
           "M-6" winum-select-window-6
           "M-7" winum-select-window-7
           "M-8" winum-select-window-8
           "M-9" winum-select-window-9)
  (:option winum-auto-setup-mode-line nil ; use custom modeline
           winum-auto-assign-0-to-minibuffer t))


(setup (:package workgroups2)
  (:doc "A package can save&load multiple named workspaces")
  (:option wg-session-file (expand-file-name "workgroups" user-emacs-directory))
  )


(setup paren
  (:doc "Highlight matching paren")
  (:with-mode show-paren-mode
    (:hook-into prog-mode))
  (:option show-paren-delay 0
           show-paren-context-when-offscreen t)
  ;; Highlight parens when cursor is within.
  (:advice show-paren-function :around (fn)
    (cond ((looking-at-p "\\s(") (funcall fn))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn)))))
  ;; Add box for parens
  (:face show-paren-match
      :box `(:line-width -1
             :color ,(face-attribute 'default :foreground))))


(setup (:package smartparens)
  (:doc "A minor mode for dealing with pairs in Emacs.")
  (defun dust/smartparens-setup ()
    (require 'smartparens-config)
    (smartparens-global-mode t))

  (:with-hook window-setup-hook
    (:hook dust/smartparens-setup))

  (:with-mode turn-on-smartparens-strict-mode
    (:hook-into prog-mode))
  
  (:when-loaded
    (:diminish)

    ;; Parens in Org Mode
    (sp-with-modes 'org-mode
      (sp-local-pair "$" "$"))

    ;; Parens in Typst ts mode
    (sp-with-modes '(typst-mode typst-ts-mode)
      (sp-local-pair "$" "$"))
    
    (:bind "M-a" sp-beginning-of-sexp
           "M-b" sp-end-of-sexp

           "M-s" sp-backward-up-sexp
           "M-d" sp-down-sexp

           "M-\\" sp-unwrap-sexp
           "M-]" sp-forward-slurp-sexp
           "M-[" sp-backward-slurp-sexp
           "M-{" sp-forward-barf-sexp
           "M-}" sp-backward-barf-sexp

           "M-m" sp-select-next-thing
           ;; "C-d C-l" sp-kill-whole-line
           ;; "C-d C-d" sp-kill-sexp

           "M-j" sp-next-sexp
           "M-k" sp-previous-sexp
           "M-l" sp-forward-symbol
           "M-h" sp-backward-symbol)))


(setup (:package corfu)
  (:doc "Corfu enhances in-buffer completion with a small completion popup.")
  (:also-load corfu-popupinfo corfu-history)
  (:option corfu-cycle t
           corfu-auto t
           corfu-auto-delay 0
           corfu-auto-prefix 1
           corfu-popupinfo-delay 0)
  (:with-map corfu-map
    (:bind
     "C-r" corfu-scroll-down
     "C-e" corfu-scroll-up
     "C-j" corfu-next
     "C-k" corfu-previous
     "M-n" corfu-popupinfo-scroll-up
     "M-p" corfu-popupinfo-scroll-down))  
  (:hook corfu-popupinfo-mode corfu-history-mode)
  (:hook-into prog-mode ielm-mode))


(setup (:package nerd-icons-corfu)
  (:after corfu
    (:option (append corfu-margin-formatters) 'nerd-icons-corfu-formatter)))


(setup (:package cape)
  (:doc "Completion At Point Extensions")
  (:option (append completion-at-point-functions) 'cape-file
           (append completion-at-point-functions) 'cape-dabbrev))


(setup (:package tempel)
  (:doc "A tiny template package for Emacs")
  (:global "C-=" tempel-complete))


(setup (:package vertico)
  (:doc "a performant and minimalistic vertical completion UI")
  (:also-load vertico-directory vertico-mouse)
  (:option vertico-scroll-margin 0
           vertico-cycle t
           vertico-resize t)
  (:hook-into window-setup)
  (:hook vertico-mouse-mode)
  (:with-map vertico-map
    (:bind "RET" vertico-directory-enter
           "DEL" vertico-directory-delete-char
           "C-DEL" vertico-directory-delete-word
           "M-DEL" vertico-directory-up)))


(setup (:package orderless)
  (:doc "An orderless completion style.")  
  (:option completion-styles '(orderless
                               substring
                               partial-completion
                               basic)
           completion-category-defaults nil
           completion-category-overrides nil
           completion-category-defaults nil
           completion-category-overrides '((file (styles . (partial-completion))))))


(setup (:package marginalia)
  (:doc "Add marginalia to the minibuffer completions.")
  (:hook-into vertico-mode))


(setup (:package nerd-icons-completion)
  (:doc "Add icons to completion candidates")
  (:hook-into marginalia-mode))


(setup (:package embark)
  (:doc "Make it easy to choose a command to run based on what is near point")
  (:option prefix-help-command #'embark-prefix-help-command
           
           (append display-buffer-alist)
           '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
             nil
             (window-parameters (mode-line-format . none))))
  (:global  "C-/" embark-act
            "C-?" embark-dwim))


(setup (:package consult embark-consult)
  (:doc "Search and navigation commands based on the Emacs completion function")
  (:also-load consult-imenu)
  (:hook-into embark-collect-mode consult-preview-at-point-mode)
  (:global "C-f f" consult-line
         "C-f g" consult-goto-line
         "C-f m" consult-mark
         "C-f M" consult-global-mark
         "C-f d" consult-ripgrep
         "C-f i" consult-imenu))


(setup (:package phi-search)
  (:doc "Another incremental search & replace,"
        "with Chinese words supported.")
  (:global "C-." phi-search
           "C-," phi-search-backward
           "C-f r" phi-replace-query)
  (:option phi-search-limit 10000)
  (defvar dust/phi-search--last-search nil)
  (defun dust/phi--insert-last-search ()
    "Use last search if no region actives."
    (when dust/phi-search--last-search
      (insert dust/phi-search--last-search)
      (mark-whole-buffer)))
  (:when-loaded
    (:advice phi-search--search-initialize :before (&rest _)
      (unless (use-region-p)
        (setq dust/phi-search--last-search phi-search--last-executed))))

  (:with-function dust/phi--insert-last-search
    (:hook-into phi-search-init phi-replace-init)))


(setup display-line-numbers
  (:doc "Interface for display-line-numbers")
  (:option display-line-numbers-type 'relative)
  (:hook-into prog-mode)
  (:face line-number :slant 'italic)
  (:face line-number-current-line
    :foreground (face-attribute 'error :foreground)
    :background (face-attribute 'highlight :background)
    :weight 'bold
    :slant 'normal))


(setup hl-line
  (:doc "Highlight the current line")
  (:hook-into prog-mode))


(setup (:package rainbow-delimiters)
  (:doc "Highlight delimiters according to their depth.")
  (:hook-into prog-mode))


(setup eldoc
  (:doc "Show function arglist or variable docstring in echo area")
  (:diminish)
  (:option eldoc-echo-area-use-multiline-p nil))


(setup (:package highlight-indent-guides)
  (:doc "This minor mode highlights indentation levels via font-lock.")
  (:diminish)
  (:hook-into prog-mode)
  (:option highlight-indent-guides-method 'character
           highlight-indent-guides-auto-enabled nil)
  (:face highlight-indent-guides-odd-face
    :foreground (face-attribute 'shadow :foreground))
  (:face highlight-indent-guides-even-face
    :foreground (face-attribute 'shadow :foreground))
  (:face highlight-indent-guides-character-face
    :foreground (face-attribute 'shadow :foreground)))


(setup (:package mwim)
  (:doc "Operations of Cursor moving")
  (:global "C-a" mwim-beginning-of-code-or-line
           "C-b" mwim-end-of-code-or-line)
  (:after select-mode
    (defalias 'select-mode-beginning-of-line 'mwim-beginning-of-code-or-line)
    (defalias 'select-mode-end-of-line 'mwim-end-of-code-or-line)))


(setup hideshow
  (:doc "Minor mode cmds to selectively display code/comment blocks")
  (:with-mode hs-minor-mode
    (:diminish)
    (:hook-into prog-mode)
    (:bind "C-c C-v C-c" hs-toggle-hiding
           "C-c C-v C-h" hs-hide-block
           "C-c C-v C-s" hs-show-block
           "C-c C-v C-t" hs-hide-all
           "C-c C-v C-a" hs-show-all
           "C-c C-v C-l" hs-hide-level)))


(setup (:package treesit-auto)
  (:doc "A parser generator tool and an incremental parsing library.")
  (:disabled)
  (:require treesit-auto)
  (:option treesit-auto-install t)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(setup eglot
  (:doc "A emacs LSP client")
  (:with-function eglot-ensure
    (:hook-into python-mode
                python-ts-mode
                yaml-mode
                yaml-ts-mode
                js-mode
                js-ts-mode
                mhtml-mode
                scss-mode)))


(setup python
  (:doc "Python's flying circus support for Emacs")
  (defcustom setup--python-venvs
    '(("~/pyproj" "~/.pydev/bin/jedi-language-server")
      ("~/.config/qtile" "~/.qtile/bin/jedi-language-server"))
    "Python virtual environment")
  (defun dust/python-lsp-setup (_)
    (require 'f)
    (require 's)
    (let ((projdir (f-expand "~/pyproj"))
          (file-name (buffer-file-name))
          ret)
      (or (when file-name
            (cdar (--filter
                   (f-ancestor-of-p (car it) file-name)
                   setup--python-venvs)))
          '("jedi-language-server"))))
  (:option python-indent-guess-indent-offset nil)
  (:after eglot
    (:option (prepend eglot-server-programs)
             '((python-mode python-ts-mode) . dust/python-lsp-setup))))


(setup (:package yaml-mode)
  (:doc "Support for YAML Language")
  (:file-match "\\.\\(yml\\|yaml\\)\\'"))


(setup (:package elisp-def)
  (:doc "Go to the definition of the symbol at point.")
  (:hook-into emacs-lisp-mode ielm-mode)
  (:diminish))


(setup (:package yuck-mode)
  (:doc "Emacs major mode for editing yuck configuration files")
  (:file-match "\\.\\(yuck\\)\\'"))


(setup (:package (sxhkd-mode "https://github.com/xFA25E/sxhkd-mode"))
  (:doc "A major mode for editing sxhkdrc")
  (:file-match (rx "sxhkdrc" string-end)))


(setup (:package jinja2-mode)
  (:doc "Jinja2 mode for emacs")
  (:file-match "\\.\\(jinja2\\)\\'"))


(setup (:package markdown-mode)
  (:doc "Support for Markdown")
  (:file-match "\\.md\\'"))


(setup org
  (:doc "keeping notes, maintaining ToDo lists,"
        "and doing project planning")
  (:when-loaded
    (:option org-todo-keywords
             '((sequence "TODO(t)"
                         "DOING(i)"
                         "WAIT(w@/!)"
                         "|"
                         "DONE(d!)"
                         "CANCELED(c@)"))
             org-todo-keyword-faces
             '(("TODO" . (:background "red3" :foreground "white" :weight bold))
               ("DOING" . (:background "SteelBlue" :foreground "white" :weight bold))
               ("WAIT" . (:background "orange" :foreground "white" :weight bold))
               ("DONE" . (:background "SeaGreen4" :foreground "white" :weight bold))
               ("CANCELED" . (:foreground "#b6b6b2" :weight bold :strike-through "#b6b6b2"))))
    (:option org-edit-src-content-indentation 0
             ;; org-auto-align-tags nil
             org-tags-column 0
             org-use-sub-superscripts nil
             org-link-file-path-type 'relative
             org-startup-truncated t
             org-fontify-quote-and-verse-blocks t
             )

    (:bind "C-c e" org-edit-src-code
           "C-j" next-line
           "C-," phi-search-backward
           "C-'" imenu-list-smart-toggle)

    (:face org-checkbox :box nil)
    (:face org-level-1 :height 1.6)
    (:face org-level-2 :height 1.5)
    (:face org-level-3 :height 1.4)
    (:face org-level-4 :height 1.3)
    (:face org-level-5 :height 1.2)
    (:face org-level-6 :height 1.1)

    (defun dust/org-at-headline-stars-p ()
      (and (looking-at org-outline-regexp) (looking-back "^\**")))

    (:option org-use-speed-commands 'dust/org-at-headline-stars-p
             (append rime-disable-predicates) 'dust/org-at-headline-stars-p)
    (setcdr (assoc "a" org-speed-commands) 'org-attach)
    (setcdr (assoc "s" org-speed-commands) 'org-schedule)
    
    (:option (append* org-speed-commands)
             '(("A" . org-archive-subtree-default-with-confirmation)
               ("d" . org-deadline)))
    ))


(setup dust/org-heading
  (:after org
    (:require s)
    (defun dust/org-headline-set (N)
      "Set org headline"
      (when (and (org-at-heading-p)
                 (/= N
                     (save-excursion
                       (beginning-of-line)
                       (length
                        (nth 1
                             (s-match org-heading-regexp
                                      (buffer-substring-no-properties
                                       (point)
                                       (+ (point) 10))))))))
        (org-toggle-heading))
      (when (and (>= N 1)
                 (<= N 6))
        (org-toggle-heading N)))
    (defun dust/org-headline-set-1 () (interactive) (dust/org-headline-set 1))
    (defun dust/org-headline-set-2 () (interactive) (dust/org-headline-set 2))
    (defun dust/org-headline-set-3 () (interactive) (dust/org-headline-set 3))
    (defun dust/org-headline-set-4 () (interactive) (dust/org-headline-set 4))
    (defun dust/org-headline-set-5 () (interactive) (dust/org-headline-set 5))
    (defun dust/org-headline-set-6 () (interactive) (dust/org-headline-set 6))

    (:bind "C-c h 1" dust/org-headline-set-1
           "C-c h 2" dust/org-headline-set-2
           "C-c h 3" dust/org-headline-set-3
           "C-c h 4" dust/org-headline-set-4
           "C-c h 5" dust/org-headline-set-5
           "C-c h 6" dust/org-headline-set-6))
  )


(setup dust/org-insert
  (:after org
    (:require dash f)
    
    (defun dust/org-link--fetch-title-by-url (uri)
      (let (title)
        (with-current-buffer (url-retrieve-synchronously
                              uri t nil 10)
          (let* ((dom (libxml-parse-html-region
                       (point-min) (point-max))))
            (setq title 
                  (and dom
                       (dom-text (dom-by-tag dom 'title))))))
        (unless (or title
                    (length= title 0))
          (message "Failed to get title."))
        title))

    (defun dust/org-link-make-description-function (link desc)
      (let* ((tmp (string-split link ":")) 
             (protocol (car tmp))
             (path (string-join (cdr tmp) ":")))
        (pcase protocol
          ((or "http" "https")
           (dust/org-link--fetch-title-by-url link))
          ("denote"
           ;; TODO
           (denote-retrieve-title-value (denote-get-path-by-id path) 'org))
          (_ desc))))

    (:option org-link-make-description-function
             #'dust/org-link-make-description-function)

    
    (defun dust/org-insert-key-sequence ()
      "Insert key sequnce"
      (interactive)
      (insert (key-description
               (read-key-sequence-vector "Pressing... "))))
    
    (defcustom dust/org-image-directory "~/Note/assets"
      "Where the images are")

    (defun dust/org-image--sort-completion-table (images)
      (--sort (>= (f-modification-time it 'seconds)
                  (f-modification-time other 'seconds))
              (f-files dust/org-image-directory)))


    (defun dust/org-insert-image ()
      "Insert image from Special directory into current buffer."
      (interactive)
      (let ((images (--sort (>= (f-modification-time it 'seconds)
                                (f-modification-time other 'seconds))
                            (f-files dust/org-image-directory)))
            (cur-file (f-this-file))
            (completions-sort nil)
            (vertico-sort-function nil))
        (if (not cur-file)
            (message "Current buffer is not a file: %s" (buffer-name
                                                         (current-buffer)))
          (insert (format "[[file:%s]]\n"
                          (f-relative
                           (completing-read
                            "Insert image: "
                            (dust/org-image--sort-completion-table dust/org-image-directory)
                            ;; (dust/org-image--sort-completion-table images)
                            nil t)
                           (f-dirname cur-file)))))))

    (:with-map org-mode-map
      (:bind
       "C-c C-m" dust/org-insert-image
       "C-c M-k" dust/org-insert-key-sequence))))


(setup org-agenda
  (:doc "Org Agenda settings")
  (:global "C-\\" org-cycle-agenda-files)
  (:option org-agenda-files '("~/Agendas"))
  (:when-loaded
    
    (:option org-agenda-tags-column 0
             org-agenda-use-time-grid t
             org-agenda-time-grid '((daily today require-timed)
                                    (700 1200 1800 2300)
                                    "......"
                                    "----------------")
             org-agenda-start-with-follow-mode t
             org-agenda-skip-scheduled-if-done t)))


(setup (:package org-modern)
  (:doc "a modern style for Org buffers.")
  (:hook-into org-mode)
  (:when-loaded
    (:with-function org-modern-agenda
      (:hook-into org-agenda-finalize))

    ;; Org prettify
    (:option org-auto-align-tags nil
             org-hide-emphasis-markers t
             org-pretty-entities nil
             org-ellipsis "...")

    (:face org-modern-label :height 1.0)
    (:face org-modern-statistics
      :background  (face-attribute 'default :background)
      :box nil)
    (:face org-modern-tag :box nil)
    
    (:option org-modern-star 'replace
             org-modern-replace-stars "*"
             
             org-modern-checkbox '((?X . "󰄵")
                                   (?- . "󰄗")
                                   (?\s . "󰄱"))
             org-modern-progress '("󰝦 " "󰪞 " "󰪟 " "󰪠 "
                                   "󰪡 " "󰪢 " "󰪣 " "󰪤" "󰪥 ")
             org-modern-todo nil
             org-modern-block-name '(("src" . (" " ""))
                                     ("quote" . ("" "")))
             org-modern-keyword '(("title" . "  ")
                                  ("date" . "")
                                  ("filetags" . "")
                                  ("identifier" . "")
                                  ("options" . " ")
                                  ("location" . " ")
                                  ("caption" . " "))
             org-modern-list '((?+ . "")
                               (?- . "")
                               (?* . "")))))


(setup (:package olivetti)
  (:doc "A simple Emacs minor mode for a nice writing environment.")
  (:diminish)
  (:option olivetti-body-width 110)
  (:hook-into org-mode typst-mode typst-ts-mode))


(setup org-latex
  (:doc "Org Latex Environment")
  (:option org-highlight-latex-and-related '(latex native))
  (:after org
    (:option org-latex-listings t
             org-latex-pdf-process
             '("xelatex -interaction nonstopmode -no-mktex=tex -output-directory %o %f"
               ;; "xelatex -interaction nonstopmode -output-directory %o %f"
               ;; "xelatex -interaction nonstopmode -output-directory %o %f"
               )
             org-latex-compiler "xelatex"
             (append org-latex-packages-alist) '("" "xeCJK"))
    ;; Beamer
    (:option org-beamer-frame-level 2
             org-beamer-outline-frame-title "Content")))


(setup (:package org-ql)
  (:doc "A query language for Org files.")
  (:after org-ql-view
    (:when-loaded
      (:bind
       "q" (lambda () (interactive) (quit-window t))))))


(setup org-mobile
  (:doc "Asymmetric Sync With a Mobile Device")
  (:autoload org-mobile-push org-mobile-pull)
  (:option org-mobile-directory "/ssh:MyServer:/root/Org"))


(setup (:package rime)
  (:doc "RIME input method in Emacs")
  ;; (:require rime)
  (:option default-input-method "rime")

  (defun dust/rime-predicate-punctuation-after-space-cc-p ()
    (let* ((start (save-excursion
                    (re-search-backward
                     "[^\s]"
                     nil
                     t)))
           (string (buffer-substring
                    (if start start 1)
                    (point))))
      (string-match "\\cc +" string)))

  (defun dust/rime-predicate-org-in-src-block-p ()
    (and (derived-mode-p 'org-mode)
         (org-element-at-point)
         (org-src--on-datum-p (org-element-at-point))
         (org-in-src-block-p)))
  
  (:option rime-inline-ascii-trigger 'shift-l
           rime-show-preedit 'inline
           rime-show-candidate 'posframe
           rime-deactivate-when-exit-minibuffer nil)
  (:option rime-disable-predicates
           '(rime-predicate-prog-in-code-p
             dust/rime-predicate-punctuation-after-space-cc-p
             rime-predicate-after-alphabet-char-p
             dust/rime-predicate-org-in-src-block-p
             rime-predicate-org-latex-mode-p))

  (:face rime-default-face :background (face-attribute 'default :background))
  (:option rime-posframe-properties nil)
  
  (defun dust/rime-finalize ()
    (add-hook 'kill-emacs-hook #'rime-lib-finalize)
    (remove-hook 'input-method-activate-hook #'dust/rime-finalize))
  (:with-hook input-method-activate-hook
    (:hook dust/rime-finalize))
  
  (:hook (lambda ()
           (when (rime--ascii-mode-p)
             (rime--inline-ascii))))
  (:global "C-<SPC>" toggle-input-method)
  )


(setup (:package dirvish)
  (:doc "An improved version of the Emacs inbuilt package Dired")
  (:load-path "extensions")
  (:also-load dirvish-icons
              dirvish-emerge
              dirvish-quick-access
              dirvish-subtree
              subr-x)
  (:when-loaded
    (:option dirvish-quick-access-entries
             '(("h" "~/")
               ("e" "~/.emacs.d/")
               ("g" "~/Repository/")
               ("n" "~/Note/")
               ("G" "~/.emacs.d/etc/gnus/")))
    
    (:option dirvish-preview-dispatchers '(image gif audio archive))
    (dirvish-define-preview lsd (file)
                            "Use `lsd' to generate directory preview."
                            :require ("lsd") ; tell Dirvish to check if we have the executable
                            (when (file-directory-p file) ; we only interest in directories here
                              `(shell . ("lsd" "-al" "--icon" "never" ,file))))
    (:option (append dirvish-preview-dispatchers) 'lsd)
    
    (defun dust/dirvish--truncate-line (&rest _)
      (setq-local truncate-lines t))
    
    (dirvish-emerge-define-predicate is-dir
                                     "If item is a directory"
                                     (equal (car type) 'dir))

    (:option dirvish-use-header-line nil
             dirvish-attributes '(subtree-state nerd-icons file-size)
             delete-by-moving-to-trash t
             dirvish-mode-line-height 21
             dirvish-redisplay-debounce 1
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
    
    (:option dirvish-override-dired-mode t)
    
    (:with-hook dirvish-find-entry-hook
      (:hook dust/dirvish--truncate-line))
    (:with-mode dirvish-emerge-mode
      (:hook-into dirvish-setup))
    
    (:bind
     "a" dirvish-quick-access
     "j" dired-next-line
     "k" dired-previous-line
     "f" dired-goto-file
     "b" dired-up-directory
     "n" dirvish-emerge-next-group
     "p" dirvish-emerge-previous-group
     "TAB" dirvish-subtree-toggle
     "M-t" dirvish-layout-toggle
     "z" dired
     "SPC" dired-find-file))
  (:global "C-x C-d" dirvish)
  )


(setup (:package (jieba "https://github.com/fingerknight/jieba.el"))
  (:doc "Things about selection and deletion supporting CN words")
  (:diminish)
  (defalias 'dust/forward-word 'jieba-forward-word)
  (defalias 'dust/backward-word 'jieba-backward-word)
  (defalias 'dust/select-word-at-point 'jieba-mark-word)
  (:option jieba-current-backend 'python)
  (:hook-into window-setup)
  )


(setup (:package imenu-list)
  (:doc "A automatically updated buffer with imenu entries.")  
  (:global "C-'" imenu-list-smart-toggle)
  (:option imenu-list-focus-after-activation t
           imenu-list-position 'left
           imenu-list-size 0.2)
  (defun dust/imenu-list--truncate-line (&rest _)
    (with-current-buffer (imenu-list-get-buffer-create)
      (setq-local truncate-lines t)))
  (:when-loaded
    (:with-hook imenu-list-after-jump-hook
      (:hook recenter-top-bottom))
    (:with-hook imenu-list-update-hook
      (:hook dust/imenu-list--truncate-line))))


(setup (:package (excerpt "https://github.com/fingerknight/excerpt.el")
                 emacsql)
  (:doc "Excerpt management")
  (:option excerpt-dir (expand-file-name "~/Note"))
  )


(setup (:package simple-mpc)
  (:doc "A GNU Emacs major mode that acts as a front end to mpc.")
  (:option simple-mpc-playlist-format "%title%\t%artist%\t%time%"
           simple-mpc-table-separator "\t")
  (:when-loaded
    (defun dust/simple-mpc-load-playlist (playlist-name)
      "Rewrite load playlist such that:
1. Clean play list before loading;
2. Shuffle play lists after loading.
3. Show play list after loading."
      (interactive
       (list
        (completing-read "Playlist: " (simple-mpc-call-mpc-strings "lsplaylists"))))
      (simple-mpc-clear-current-playlist)
      (simple-mpc-call-mpc nil (list "load" playlist-name))
      (simple-mpc-shuffle-current-playlist)
      (simple-mpc-maybe-refresh-playlist)
      (simple-mpc-view-current-playlist))
    (defalias 'simple-mpc-load-playlist 'dust/simple-mpc-load-playlist)))


(setup gnus
  (:doc "A newsreader for GNU Emacs")
  (:option gnus-home-directory (expand-file-name "gnus" user-emacs-directory))
  (:when-loaded
    (:option (prepend auth-sources) (expand-file-name "authinfo" gnus-home-directory))
    (:option gnus-select-method '(nnimap
                                  "outlook"
                                  (nnimap-address "outlook.office365.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

    (:with-feature gnus-sum
      (:when-loaded
        (:option (prepend gnus-article-sort-functions) 'gnus-summary-sort-by-most-recent-date)))
    
    (:option gnus-permanently-visible-groups
             "nls_.+\\|Inbox\\|Sent\\|Junk\\|nnrss:.+"

             gnus-blocked-images nil)

    (defcustom dust/gnus-rss-list
      '(("iDaily" "https://rsshub.app/idaily/today")
        ("每日早报" "https://rsshub.app/readhub/daily")
        ("理想生活实验室" "https://rsshub.app/toodaylab/posts")
        ("少数派" "https://rsshub.app/sspai/index")
        ("纽约时报" "https://rsshub.app/nytimes/dual")
        ("Hacker News" "https://rsshub.app/hackernews")
        ("Bing 每日壁纸" "https://rsshub.app/bing")
        ("Emacs China" "https://emacs-china.org/latest.rss")
        ("Linux Links" "https://www.linuxlinks.com/feed/"))
      "RSS Feeds")

    (defun dust/gnus-rss ()
      (dolist (it dust/gnus-rss-list)
        (unless (gnus-group-entry (concat "nnrss:" (car it)))
          (let ((title (car it))
                (href (cadr it)))
            (gnus-group-make-group title '(nnrss ""))
            (push (list title href title) nnrss-group-alist))))
      (nnrss-save-server-data nil))
    
    (:with-hook gnus-group-prepare-hook
      (:hook dust/gnus-rss))))


(setup (:package org-journal)
  (:doc "A simple personal diary / journal using in Emacs.")
  (:when-loaded
    (defun org-journal-file-header-func (time)
      "Custom function to create journal header."
      (concat "#+title: Daily Journal\n"
              "#+startup: overview\n\n"))
    (defun org-journal-find-location ()
      ;; Open today's journal, but specify a non-nil prefix argument in order to
      ;; inhibit inserting the heading; org-capture will insert the heading.
      (org-journal-new-entry t)
      (goto-char (point-max)))
    
    ;; "https://github.com/bastibe/org-journal/issues/369"
    ;; (defconst org-journal--cache-file
    ;; (no-littering-expand-var-file-name "org-journal.cache"))
    (:option org-journal-dir "~/Journal"
             org-journal-find-file 'find-file

             org-journal-date-format "%Y-%m-%d, %a"
             org-journal-carryover-items
             "TODO=\"TODO\"|TODO=\"DOING\"|TODO=\"WAIT\""

             org-journal-file-type 'daily
             org-journal-file-header 'org-journal-file-header-func
             
             org-journal-enable-cache t
             
             org-journal-file-format "%Y%m%d.org"
             org-journal-hide-entries-p nil

             org-journal-enable-agenda-integration t))
  (defun dust/org-journal-new-journal ()
    (interactive)
    (org-journal-new-entry t))
  (:global "C-c C-j" dust/org-journal-new-journal)
  )


(setup (:package denote)
  (:doc "A simple note-taking tool for Emacs")
  (defun denote-subdirectory-with-template ()
    "Create note while prompting for a subdirectory.

Available candidates include the value of the variable
`denote-directory' and any subdirectory thereof.

This is equivalent to calling `denote' when `denote-prompts' is
set to '(subdirectory title keywords)."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts '(template subdirectory title keywords)))
      (call-interactively #'denote)))
  (:option
   denote-directory "~/Note"

   denote-date-format "<%Y-%m-%d %a>"
   
   denote-infer-keywords t
   denote-sort-keywords t
   denote-file-type 'org

   denote-prompts '(subdirectory title keywords)
   denote-excluded-directories-regexp "^assets\\|^static"

   ;; Pick dates, where relevant, with Org's advanced interface:
   denote-date-prompt-use-org-read-date t
   denote-allow-multi-word-keywords nil

   xref-search-program 'ripgrep

   denote-templates
   `((book . ,(concat "#+book_author: \n"
                      "#+book_translator: \n"
                      "#+book_publisher: \n"
                      "#+book_publish_date: \n\n"
                      "#+startup: overview\n\n"
                      "* Outline\n"
                      ":PROPERTIES:\n"
                      ":VISIBILITY: all\n"
                      ":END:\n\n"
                      "* Conclusion\n"
                      ":PROPERTIES:\n"
                      ":VISIBILITY: all\n"
                      ":END:\n\n"
                      "* Excerpts\n"
                      ":PROPERTIES:\n"
                      ":VISIBILITY: children\n"
                      ":END:\n"))
     (blog . ,(concat "#+publish: nil\n\n"))))
  (:when-loaded
    (:face denote-faces-date
      :foreground (face-attribute 'font-lock-function-name-face :foreground))
    (:face denote-faces-delimiter
      :foreground (face-attribute 'font-lock-comment-face :foreground))
    (:face denote-faces-extension
      :foreground (face-attribute 'font-lock-comment-face :foreground))
    (:face denote-faces-keywords
      :foreground (face-attribute 'font-lock-keyword-face :foreground))
    (:face denote-faces-title
      :foreground (face-attribute 'font-lock-string-face :foreground)))
  (:with-mode denote-dired-mode
    (:hook-into dired-mode)))


(setup (:package consult-notes)
  (:doc "Easily select notes via consult.")
  (:with-hook minibuffer-setup-hook
    (:hook consult-notes-denote-mode)))


(setup (:package denote-menu)
  (:doc "An interface for viewing your denote files")
  (:option denote-menu-show-file-type nil)
  (:global "C-x d" denote-menu-list-notes)
  (:bind "c" denote-menu-clear-filters
         "f" denote-menu-filter
         "k" denote-menu-filter-by-keyword
         "K" denote-menu-filter-out-keyword))


(setup time-stamp
  (:doc "Maintain last change time stamps in files edited by Emacs")
  (:after org
    (defun time-stamp-denote ()
      "automatically update time stamp before saving denote files"
      (when (and (buffer-file-name)
                 (denote-filename-is-note-p (buffer-file-name)))
        (require 'time-stamp)
        (let* ((time-stamp-start "#\\+date:[ ]*<")
               (time-stamp-end ">")
               (time-stamp-format (substring denote-date-format 1 -1)))
          (time-stamp))))
    (:local-hook before-save-hook time-stamp-denote)))


(setup org-book
  (:doc "book management")
  (:option dust/org-book-file "~/Books/books.org")
  (:after org-attach
    (:option (prepend org-attach-id-to-path-function-list)
             (lambda (id) id))
    (defun org-attach-file-list (dir)
      (--filter
       (and (not (string-prefix-p "." it))
            (not (string-suffix-p "~" it)))
       (directory-files dir nil))))
  (defun org-book ()
    (interactive)
    (find-file dust/org-book-file)))


(setup (:package pdf-tools pdf-view-restore)
  (:doc "A replacement of DocView for PDF files.")

  (:with-hook window-setup-hook
    (:hook pdf-loader-install))

  (:when-loaded
    (:with-map pdf-view-mode-map
      (:bind "j" pdf-view-next-line-or-next-page
             "k" pdf-view-previous-line-or-previous-page
             "<delete>" pdf-view-scroll-down-or-previous-page))
    
    (:option (remove pdf-tools-enabled-modes)
             '(pdf-sync-minor-mode
               pdf-virtual-global-minor-mode))
    (:with-hook pdf-view-mode-hook
      (:hook (lambda ()
               (pdf-view-restore-mode)
               (display-line-numbers-mode 0)
               (auto-revert-mode 0))))))


(setup (:package nov)
  (:doc "Major mode for reading EPUBs in Emacs.")
  (:file-match "\\.epub\\'")
  (:bind "<delete>" nov-scroll-down)
  )


(setup (:package djvu)
  (:doc "Edit and view Djvu files via djvused")
  
  )


(setup (:package (typst-ts-mode "https://git.sr.ht/~meow_king/typst-ts-mode"))
  (:doc "Tree Sitter support for Typst.")
  (:option typst-ts-mode-watch-options "--root ~/Kuafu"
           typst-ts-mode-compile-options "--root ~/Kuafu"
           typst-ts-mode-indent-offset 2))


;; (setup (:package (typst-mode "https://github.com/Ziqi-Yang/typst-mode.el"))
;;   (:doc "Typst mode")
;;   (:disabled)
;;   (:require typst-mode)
;;   (:file-match "\\.typ\\'")
;;   (:option typst-indent-offset 2))


(setup (:package flymake-aspell)
  (:doc "Aspell checker for flymake")
  (:disabled)
  (:with-function flymake-aspell-setup
    (:hook-into org-mode)))

(defun startup-echo-area-message ()
  (emacs-init-time "Emacs' started in %.2f sec. Enjoy hacking :)"))

(with-eval-after-load 'init
  (setq gc-cons-threshold (* 16 1024 1024)))

(provide 'init)
