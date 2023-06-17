;;; setup-all.el -*- lexical-binding: t; -*-

(setup dirvish
  (:doc "An improved version of the Emacs inbuilt package Dired")
  ;; setup from application
  (:require setup-dirvish)
  (:option dirvish-default-layout nil))

(setup cape
  (:doc "Completion At Point Extensions")
  (:url "cape" "https://github.com/minad/cape")
  (:require cape)
  (:option (append completion-at-point-functions) 'cape-dabbrev
           (append completion-at-point-functions) 'cape-file))

(setup hideshow
    (:doc "Minor mode cmds to selectively display code/comment blocks")
    (:with-mode hs-minor-mode
      (:diminish)
      (:hook-into prog-mode))
    (:bind-into hs-minor-mode-map
      "C-c C-v C-c" hs-toggle-hiding
      "C-c C-v C-h" hs-hide-block
      "C-c C-v C-s" hs-show-block
      "C-c C-v C-t" hs-hide-all
      "C-c C-v C-a" hs-show-all
      "C-c C-v C-l" hs-hide-level))

(setup (:require display-line-numbers)
  (:doc "Interface for display-line-numbers")
  (:option display-line-numbers-type 'relative)
  (:hook-into prog-mode)
  (set-face-attribute 'line-number nil
                      :slant 'italic)
  (set-face-attribute 'line-number-current-line nil
                      :foreground (face-attribute 'error :foreground)
                      :background (face-attribute 'highlight :background)
                      :weight 'bold
                      :slant 'normal))

(setup hl-line
  (:doc "Highlight the current line")
  (:hook-into prog-mode))

(setup paren
    (:doc "Highlight matching paren")
    (:with-mode show-paren-mode
      (:hook-into prog-mode))
    (:option show-paren-delay 0)
    (define-advice show-paren-function (:around (fn) fix)
      "Highlight enclosing parens."
      (cond ((looking-at-p "\\s(") (funcall fn))
            (t (save-excursion
                 (ignore-errors (backward-up-list))
                 (funcall fn)))))
    (set-face-attribute 'show-paren-match nil
                        :box
                        `(:line-width -1
                          :color ,(face-attribute 'default :foreground))))

(setup rainbow-delimiters
    (:doc "Highlight delimiters according to their depth.")
    (:url "rainbow-delimiters" "https://github.com/Fanael/rainbow-delimiters")
    (:autoload rainbow-delimiters-mode)
    (:hook-into prog-mode))

(setup (:require ibuffer)
    (:doc "Operate on buffers like dired")
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (file-size-human-readable (buffer-size)))
    (:hook (lambda ()
             (ibuffer-switch-to-saved-filter-groups "Buffer")
             (setq ibuffer-hidden-filter-groups '("Base" "Hidden"))))
    (:option ibuffer-marked-char 42
             ibuffer-show-empty-filter-groups nil
             ibuffer-display-summary nil
             ibuffer-filter-group-name-face 'font-lock-doc-face
             ibuffer-formats (quote ((" " mark " "
                                      (name 18 18 :left :elide)
                                      " "
                                      (mode 16 16 :left :elide)
                                      " "
                                      (size-h 9 9 :right)
                                      " "
                                      (filename-and-process 30 30))))
             
             ibuffer-saved-filter-groups
             (quote (("Buffer"
                      ("Dired" (mode . dired-mode))
                      ("Emacs Lisp" (name . "\\.el$\\|\\.el\\.gz$"))
                      ("Python" (name . "\\.py$"))
                      ("Org"  (name . "\\.org$"))
                      ("GNUS" (or
                               (mode . message-mode)
                               (mode . bbdb-mode)
                               (mode . mail-mode)
                               (mode . gnus-group-mode)
                               (mode . gnus-summary-mode)
                               (mode . gnus-article-mode)
                               (name . "^\\.bbdb$")
                               (name . "^\\.newsrc-dribble")))
                      ("Base" (or
                               (name . "^\\*scratch\\*$")
                               (name . "^\\*Messages\\*$")
                               (name . "^\\*Completions\\*$")
                               (mode . help-mode)
                               (mode . debug-mode)))
                      ("Hidden" (name . "^\\*[^*]*\\*$"))))))
    (:bind-into ibuffer-mode-map
                "j" ibuffer-forward-line
                "k" ibuffer-backward-line
                "M-j" ibuffer-forward-line
                "M-k" ibuffer-backward-line
                "f" ibuffer-jump-to-buffer
                "S-f" ibuffer-forward-line
                "M-f" ibuffer-forward-line)
    (:modalka "C-x C-b" ibuffer))

(setup jieba
  (:doc "Things about selection and deletion supporting CN words")
  (:url "jsonrpc" "https://github.com/paritytech/jsonrpc/"
        "jieba" "https://github.com/cireu/jieba.el")
  (:require jieba)
  (:diminish)
  (defun select-word-at-point ()
    (interactive)
    (when (use-region-p)
      (keyboard-escape-quit))
    ;; 确保回到单词的第一个字符
    (jieba-forward-word)
    (jieba-backward-word)
    (set-mark-command nil)
    ;; 落到单词结尾
    (jieba-forward-word))
  (defun select-next-word (&optional N)
    (interactive "p")
    (when (use-region-p)
      (keyboard-escape-quit))
    (set-mark-command nil)
    (jieba-forward-word N))
  (defun select-previous-word (&optional N)
    (interactive "p")
    (when (use-region-p)
      (keyboard-escape-quit))
    (set-mark-command nil)
    (jieba-backward-word N))
  (defun select-current-line-and-forward-line (&optional N)
    "Select a line, with cursor locating at the beginning of next line.
    If N given, then select N lines.(-N backward)"
    (interactive "p")
    (when (use-region-p)
      (keyboard-escape-quit))
    (forward-line 0)    ; 移动到行首
    (set-mark-command nil)
    (forward-line N))
  (defun select-current-lin-without-indentation (&optional N)
    "Select a line, withn cursor locating at the end of current line.
    If N given, then select N lines.(-N backward)"
    (interactive "p")
    (when (use-region-p)
      (keyboard-escape-quit))
    (mwim-beginning-of-code-or-line)
    (set-mark-command nil)
    (move-end-of-line N))
  (defun select-to-forward-word (&optional N)
    "Select to next word.
    If N given, then select to next N words."
    (interactive "P")
    (when (use-region-p)
      (keyboard-escape-quit))
    (set-mark-command nil)
    (jieba-forward-word N))
  (defun select-to-backword-word (&optional N)
    "Select to previous word.
    If N given, then select to previous N words."
    (interactive "P")
    (when (use-region-p)
      (keyboard-escape-quit))
    (set-mark-command nil)
    (jieba-backward-word N))
  (defun select-to-beginning-of-line (&optional REAL)
    "Select to the beginning of line.
    IF REAL, then indentions include."
    (interactive "P")
    (when (use-region-p)
      (keyboard-escape-quit))
    (set-mark-command nil)
    (if REAL
        (mwim-beginning-of-line)
      (mwim-beginning-of-code-or-line)))
  (defun select-to-end-of-line (&optional REAL)
    "Select to the end of line.
    IF REAL, then spaces include."
    (interactive "P")
    (when (use-region-p)
      (keyboard-escape-quit))
    (set-mark-command nil)
    (if REAL
        (mwim-end-of-line)
      (mwim-end-of-code-or-line)))
  (defun delete-word-at-point ()
    (interactive)
    (select-word-at-point)
    (delete-active-region))
  (defun delete-next-word (&optional N)
    (interactive "p")
    (select-next-word N)
    (delete-active-region))
  (defun delete-previous-word (&optional N)
    (interactive "p")
    (select-previous-word N)
    (delete-active-region))
  (defun delete-lines (&optional N)
    (interactive "p")
    (select-current-line-and-forward-line N)
    (delete-active-region))
  (defun delete-to-beginning-of-line (&optional REAL)
    (interactive "P")
    (select-to-beginning-of-line REAL)
    (delete-active-region))
  (defun delete-to-end-of-line (&optional REAL)
    (interactive "P")
    (select-to-end-of-line REAL)
    (delete-active-region))
  (:modalka "C-r" jieba-forward-word
            "C-e" jieba-backward-word
            "C-v C-d" select-word-at-point
            "C-v C-r" select-next-word
            "C-v C-e" select-previous-word
            "C-v C-l" select-current-line-and-forward-line
            "C-v C-L" select-current-lin-without-indentation
            "C-v C-a" select-to-beginning-of-line
            "C-v C-b" select-to-end-of-line
            "C-d C-d" delete-word-at-point
            "C-d C-r" delete-next-word
            "C-d C-e" delete-previous-word
            "C-d C-l" delete-lines
            "C-d C-a" delete-to-beginning-of-line
            "C-d C-b" delete-to-end-of-line
            "C-<backspace>" delete-previous-word
            "C-<delete>" delete-next-word)
  (:option jieba-mode 1))

(setup eglot
  (:doc "A emacs LSP client")
  (:autoload eglot-ensure)
  
  (:with-function eglot-ensure
    (:hook-into python-mode
                yaml-mode
                js-mode
                mhtml-mode
                scss-mode
                latex-mode))
  (:option eldoc-echo-area-use-multiline-p 1))

(setup denote
  (:doc "A simple note-taking tool for Emacs")
  (:url "denote" "https://github.com/protesilaos/denote")
  (:require denote)
  (defun denote-template ()
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
   denote-known-keywords '("emacs" "philosophy" "politics" "history" "aesthetics"
                           "math" "physics" "python" "book" "pedagogy" "muse" )

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
  (:after denote
    (set-face-attribute 'denote-faces-date nil
                        :foreground
                        (face-attribute 'font-lock-function-name-face
                                        :foreground))
    (set-face-attribute 'denote-faces-delimiter nil
                        :foreground
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (set-face-attribute 'denote-faces-extension nil
                        :foreground
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (set-face-attribute 'denote-faces-keywords nil
                        :foreground
                        (face-attribute 'font-lock-keyword-face
                                        :foreground))
    (set-face-attribute 'denote-faces-title nil
                        :foreground
                        (face-attribute 'font-lock-string-face
                                        :foreground)))
  (:with-mode denote-dired-mode
    (:hook-into dired-mode)))

;; (setup modeline
;;   (:doc "Configuration of mode line"
;;         "https://emacs.stackexchange.com/questions/16654/how-to-re-arrange-things-in-mode-line"
;;         "https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line")
;;   (defcustom modeline-colors '((background . ("#F0F0F0" "#44475a"))
;;                                (background-deep . ("#FAFAFA" "#282a36"))
;;                                (foreground . ("#494B53" "#f8f8f2"))
;;                                (red . ("#E45649" "#ff5555"))
;;                                (blue . ("#4078F2" "#7b6bff"))
;;                                (green . ("#50A14F" "#50fa7b"))
;;                                (yellow . ("#986801" "#f1fa8c"))
;;                                (pink . ("#CA1243" "#ff79c6"))
;;                                (cyan . ("#0184BC" "#8be9fd"))
;;                                (purple . ("#A626A4" "#bd93f9"))
;;                                (orange . ("#C18401" "#ffb86c")))
;;     "colors in modeline.

;; This variable is a ALIST, in which keys are colors' name,
;; and values are the lists with two elements representing
;; color in light theme and dark theme respectively.

;; LIGHT: one-light
;; DARK: dracula-theme")
;;   (defvar modeline-dark-theme nil
;;     "If use dark theme colors")
;;   (defun modeline-get-color (color-name)
;;     "Return the string of COLOR-NAME.

;; If IF-DARK is not-nil, then return the color-string in
;; dark theme."
;;     (let ((idx (if modeline-dark-theme 1 0)))
;;       (nth idx (assoc-default color-name modeline-colors))))
;;   (defvar modeline-left
;;     '(modeline-winum
;;       modeline-major-mode
;;       modeline-buffer-name))
;;   (defvar modeline-right
;;     '(modeline-rime-state
;;       modeline-cursor-info))
;;   (defun modeline-winum ()
;;     "窗口编号"
;;     (propertize (format " %d " (winum-get-number))
;;                 'face `(:foreground ,(modeline-get-color 'background)
;;                                     :background ,(modeline-get-color 'foreground))))
;;   (defun modeline-major-mode ()
;;     "主模式"
;;     (propertize (format "  %s"
;;                         (s-upcase (s-replace-regexp "[\s-]*mode$" ""
;;                                                     (format "%s" major-mode))))
;;                 'help-echo (s-spaced-words (s-titleize (format "%s" major-mode)))
;;                 'face `(:foreground ,(modeline-get-color 'foreground)
;;                                     :background ,(modeline-get-color 'background)
;;                                     :family "Sans Serif"                      
;;                                     :height 0.8
;;                                     :weight bold)))
;;   (defun modeline-buffer-name ()
;;     "Buffer"
;;     (let ((name-color (cond
;;                        (buffer-read-only (modeline-get-color 'red))
;;                        ((buffer-modified-p) (modeline-get-color 'blue))
;;                        (t (modeline-get-color 'green)))))
;;       (propertize " %b"
;;                   'face `(:foreground ,name-color
;;                                       :background ,(modeline-get-color 'background))
;;                   'display '(raise -0)
;;                   'help-echo (if buffer-file-name
;;                                  buffer-file-name
;;                                (buffer-name)))))
;;   (defun modeline-rime-state ()
;;     "Whether RIME Mode is activated."
;;     (let ((state (length> (and (fboundp 'rime-lighter)
;;                                (rime-lighter))
;;                           0)))
;;       (propertize (format " %s " (if state " RIME" "    "))
;;                   'face `(:foreground ,(modeline-get-color 'pink)
;;                                       :background ,(modeline-get-color 'background))
;;                   'display '(raise -0)
;;                   )))
;;   (defun modeline-cursor-info ()
;;     "光标位置相关信息" 
;;     (propertize (s-replace "%" "%%" (format-mode-line " %p <%l,%c> "))
;;                 'face `(:foreground ,(modeline-get-color 'background)
;;                                     :background ,(modeline-get-color 'purple))))
;;   (defun modeline-pomodoro ()
;;     "番茄闹钟"
;;     (concat
;;      (propertize (format " ( %s ) " (if (and (listp org-pomodoro-mode-line)
;;                                              org-pomodoro-mode-line)
;;                                         (cadr org-pomodoro-mode-line)
;;                                       ""))
;;                  'face `(:foreground ,(modeline-get-color 'cyan)
;;                                      :background ,(modeline-get-color 'background)))
;;      (propertize (format "[%s] " (s-pad-right org-pomodoro-long-break-frequency
;;                                               " "
;;                                               (s-repeat org-pomodoro-count
;;                                                         "*")))
;;                  'face `(:foreground ,(modeline-get-color 'purple)
;;                                      :background ,(modeline-get-color 'background)))))
;;   (defun modeline-denote-title ()
;;     "Denote's Title"
;;     (let ((name-color (cond
;;                        (buffer-read-only (modeline-get-color 'red))
;;                        ((buffer-modified-p) (modeline-get-color 'blue))
;;                        (t (modeline-get-color 'green)))))
;;       (propertize (format " %s "
;;                           (denote-retrieve-title-value (f-this-file) 'org))
;;                   'face `(:foreground ,name-color
;;                                       :background ,(modeline-get-color 'background))
;;                   'display '(raise -0)
;;                   'help-echo (if buffer-file-name
;;                                  buffer-file-name
;;                                (buffer-name)))))
;;   (defun modeline-denote-keywords ()
;;     "Denote's keywords"
;;     (mapconcat
;;      (lambda (it)
;;        (propertize (format "%s" it)
;;                    'face `(:foreground ,(modeline-get-color 'orange)
;;                                        :background ,(modeline-get-color 'background))
;;                    'display '(raise -0)
;;                    ;; 'help-echo
;;                    'mouse-face `(:background ,(modeline-get-color 'background-deep))
;;                    'local-map (make-mode-line-mouse-map
;;                                'mouse-1 `(lambda ()
;;                                            (interactive)
;;                                            (let ((denote-ql--kws (list ,it)))
;;                                              (denote-ql--keyword))))))
;;      (denote-retrieve-keywords-value (f-this-file) 'org)
;;      " "))
;;   (defun modeline-empty ()
;;     "Nothing"
;;     "")
;;   (defun modeline-space ()
;;     (let* ((rp-lst (-map #'funcall modeline-right))
;;            (rp (if rp-lst
;;                    (apply #'concat rp-lst)
;;                  ""))
;;            (reserve (+ (length rp)
;;                        (if (s-match "%" rp) 0 1))))
;;       (setq reserve (1- reserve))
;;       (propertize " "
;;                   'display `((space :align-to
;;                                     (- (+ right right-fringe right-margin)
;;                                        ,reserve)))
;;                   'face `(:background ,(modeline-get-color 'background)))))
;;   (defun modeline-set-agenda ()
;;     (setq-local
;;      modeline-left '(modeline-winum
;;                      modeline-major-mode)
;;      modeline-right '(modeline-pomodoro)))
;;   (defun modeline-set-org ()
;;     (setq-local
;;      modeline-left `(modeline-winum
;;                      modeline-major-mode
;;                      ,(if (and (f-this-file)
;;                                (f-exists-p (f-this-file))
;;                                (f-descendant-of-p (f-this-file) denote-directory))
;;                           'modeline-denote-title
;;                         'modeline-buffer-name)
;;                      ,(if (and (f-this-file)
;;                                (f-exists-p (f-this-file))
;;                                (f-descendant-of-p (f-this-file) denote-directory))
;;                           'modeline-denote-keywords
;;                         'modeline-empty))
;;      modeline-right '(modeline-rime-state
;;                       modeline-cursor-info)))
;;   (:after org-agenda
;;     (:hooks org-agenda-mode modeline-set-agenda))
;;   (:hooks org-mode modeline-set-org)
;;   (setq-default mode-line-format
;;                 '("%e" (:eval
;;                         (let ((lp (-map #'funcall modeline-left))
;;                               (rp (-map #'funcall modeline-right)))
;;                           (apply #'concat
;;                                  (append lp
;;                                          (list (modeline-space))
;;                                          rp)))))))

(setup org
  (:doc "Org Mode settings")

  (defun dust/org-headline-set (N)
      "Set org headline"
      ;; (interactive "p")
      (when (and (org-at-heading-p)
                 (/= N
                     (save-excursion
                       (beginning-of-line)
                       (length
                        (nth 1
                             (s-match org-heading-regexp
                                      ;; "^\\([*]\\{1,6\\}\\) "
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

  (defun dust/org-insert-key-sequence ()
      "Insert key sequnce"
      (interactive)
      (insert (key-description
               (read-key-sequence-vector "Pressing... "))))
  (defun dust/org-insert-uri ()
    "Try to fetch HTML by URL, parsing it to get title."
    (interactive)
    (let ((title "")
          (uri (read-from-minibuffer "Uri: ")))
      (with-current-buffer
          (url-retrieve-synchronously uri t nil 10)
        (let* ((dom (libxml-parse-html-region
                     (point-min) (point-max))))
          (setq title 
                (and dom
                     (dom-text (dom-by-tag dom 'title))))))
      (unless (or title
                  (length= title 0))
        (setq title
              (read-from-minibuffer
               "Failed to get title. Manually descript: ")))
      (insert (format "[[%s][%s]]" uri (s-trim title)))))

  (:option dust/org-image-directory "~/Note/assets")
  (defun dust/org-image-insert ()
    "Insert image from Special directory into current buffer."
    (interactive)
    
    (defun image--sort-completion-table (completions)
      (lambda (string pred action)
        (if (eq action 'metadata)
            `(metadata (display-sort-function . ,#'identity))
          (complete-with-action action completions string pred))))
    
    (let ((images (--sort (>= (f-modification-time it 'seconds)
                              (f-modification-time other 'seconds))
                          (f-files dust/org-image-directory)))
          (cur-file (f-this-file)))
      (if (not cur-file)
          (message "Current buffer is not a file: %s" (buffer-name
                                                       (current-buffer)))
        (insert (format "[[file:%s]]\n"
                        (f-relative
                         (completing-read
                          "Insert image: "
                          (image--sort-completion-table images)
                          nil t)
                         (f-dirname cur-file)))))))

  (:option org-todo-keywords '((sequence "TODO(t)"
                                         "DOING(i)"
                                         "WAIT(w@/!)"
                                         "|"
                                         "DONE(d!)"
                                         "CANCELED(c@)")))
  (:option org-edit-src-content-indentation 0
           org-auto-align-tags nil
           org-tags-column 0
           org-use-sub-superscripts nil
           org-link-file-path-type 'relative)
  (:hook (lambda () (visual-line-mode 0)))
  
  (:bind
   "C-c C-i" dust/org-insert-uri
   "C-C C-k" dust/org-insert-key-sequence
   "C-c C-m" dust/org-image-insert
   "C-c s" org-schedule
   "C-c d" org-deadline
   "C-c h 1" dust/org-headline-set-1
   "C-c h 2" dust/org-headline-set-2
   "C-c h 3" dust/org-headline-set-3
   "C-c h 4" dust/org-headline-set-4
   "C-c h 5" dust/org-headline-set-5
   "C-c h 6" dust/org-headline-set-6)

  (:after org
    (set-face-attribute 'org-checkbox nil :box nil)
    (set-face-attribute 'org-level-1 nil :weight 'semi-bold :height 1.0)
    (set-face-attribute 'org-level-2 nil :weight 'semi-bold :height 1.0)
    (set-face-attribute 'org-level-3 nil :weight 'semi-bold :height 1.0)
    (set-face-attribute 'org-level-4 nil :weight 'semi-bold :height 1.0)
    (set-face-attribute 'org-level-5 nil :weight 'semi-bold :height 1.0)))
  
(setup org-agenda
  (:doc "Org Agenda settings")
  (:modalka "C-\\" org-cycle-agenda-files)
  (:option org-agenda-tags-column 0
           org-agenda-use-time-grid t
           org-agenda-time-grid '((daily today require-timed)
                                  (700 1200 1800 2300)
                                  "......"
                                  "----------------")
           org-agenda-start-with-follow-mode t
           org-agenda-skip-scheduled-if-done t))

(setup org-babel
  (:doc "Execute source code within Org")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (lisp . t))))

(setup org-modern
  (:doc "A modern style for Org buffers")
  (:url "org-modern" "https://github.com/minad/org-modern")
  (:autoload global-org-modern-mode)
  (:option org-hide-emphasis-markers t
           org-modern-list '((?+ . "◦")
                             (?- . "•")
                             (?* . "•"))
           org-modern-checkbox nil
           org-modern-todo-faces
           `(("TODO"  :background "red3" :foreground "white" :weight bold)
             ("DOING" :background "SteelBlue" :foreground "white" :weight bold)
             ("WAIT" :background "orange" :foreground "white" :weight bold)
             ("DONE" :background "SeaGreen4" :foreground "white" :weight bold)
             ("CANCELED" :foreground ,(face-attribute 'fringe
                                                      :foreground)
              :weight bold
              :strike-through ,(face-attribute 'fringe
                                               :foreground))))
  (:with-mode global-org-modern-mode
    (:hook-into org-mode)))

(setup olivetti
  (:doc "A simple Emacs minor mode for a nice writing environment.")
  (:url "olivetti" "https://github.com/rnkn/olivetti")
  (:diminish)
  (:autoload olivetti-mode)
  (:option olivetti-body-width 120)
  (:hook-into org-mode))

(setup org-pomodoro
  (:doc "Basic support for Pomodoro technique in Org Mode.")
  (:url "org-pomodoro" "https://github.com/marcinkoziej/org-pomodoro")
  (:url "alert" "https://github.com/jwiegley/alert")
  (:autoload org-pomodoro)
  (:option org-pomodoro-start-sound-p t
           org-pomodoro-length 25
           org-pomodoro-short-break-length 5
           org-pomodoro-long-break-length 30)
  (:after org-agenda
    (:bind "G" org-pomodoro))
  (:with-hook org-pomodoro-long-break-finished-hook
    (lambda () (setq org-pomodoro-count 0))))

(setup org-latex
  (:doc "Org Latex Environment")
  (:option org-latex-listings t
           org-latex-pdf-process
           '("xelatex -interaction nonstopmode -output-directory %o %f"
             "xelatex -interaction nonstopmode -output-directory %o %f"
             "xelatex -interaction nonstopmode -output-directory %o %f")
           org-latex-compiler "xelatex"
           (append org-latex-packages-alist) '("" "xeCJK"))
  ;; Beamer
  (:option org-beamer-frame-level 2
           org-beamer-outline-frame-title "Content"))

(setup org-drill
  (:doc "An Spaced Repetition System")
  (:url "persist" "https://github.com/emacs-straight/persist"
        "org-drill" "https://github.com/louietan/org-drill")
  (:autoload org-drill)
  (:option drill-directory "~/Drill/"
           org-drill-maximum-items-per-session 40
           org-drill-maximum-duration 30)
  (defun dust/drill ()
    "Start to drill."
    (interactive)
    (setq org-drill-scope
          (f-files (f-expand (completing-read
                              "What to drill: "
                              (-map #'f-base                
                                    (f-directories drill-directory))
                              nil t)
                             drill-directory)))
    (org-drill))
  (defalias 'destructuring-bind 'cl-destructuring-bind))

(setup org-journal
  (:doc "A simple personal diary / journal using in Emacs.")
  (:url "org-journal" "https://github.com/bastibe/org-journal")
  (:autoload org-journal-new-entry)
  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat "#+title: Daily Journal\n"
            "#+startup: overview\n\n"))
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (goto-char (point-max)))
  (:after org-journal
    (:doc "https://github.com/bastibe/org-journal/issues/369")
    (defconst org-journal--cache-file
      (no-littering-expand-var-file-name "org-journal.cache")))
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

           org-journal-enable-agenda-integration t)
  (defun dust/org-journal-new-journal ()
    (interactive)
    (org-journal-new-entry t))
  (:modalka "C-c C-j" dust/org-journal-new-journal))

(setup yaml-mode
  (:doc "Support for YAML Language")
  (:url "yaml-mode" "https://github.com/yoshiki/yaml-mode")
  (:autoload yaml-mode)
  (:file-match "\\.\\(yml\\|yaml\\)\\'"))

(setup markdown-mode
  (:doc "Support for Markdown")
  (:url "markdown-mode" "https://github.com/jrblevin/markdown-mode")
  (:autoload markdown-mode)
  (:option markdown-command "multimarkdown")
  (:file-match "\\.md\\'"))

(setup excerpt
  (:doc "Excerpt management")
  (:url "emacsql" "https://github.com/magit/emacsql"
        "excerpt" "https://github.com/fingerknight/excerpt.el")
  (:autoload excerpt)
  (:option excerpt-dir (no-littering-expand-var-file-name "excerpt")))

(setup org-blog
  (:doc "Blog publishing system")
  (:load-path "~/gh-repo/org-blog")
  (:autoload org-blog-publish)
  (:option org-blog-publish-directory "~/blog/"
           org-blog-posts-directory "~/Note/"
           org-blog-static-directory "~/Note/static/"
           org-blog-assets-directory "~/Note/assets/"
           org-blog-cache-file (no-littering-expand-var-file-name
                                "org-blog-cache.json"))
  (:option org-blog-meta-title "手指骑士的病房"
           org-blog-meta-slogan "I choose to see the beauty."
           org-blog-meta-author "Finger Knight"
           org-blog-index-length 5
           org-blog-use-preview t
           org-blog-enable-tags t

           org-export-with-sub-superscripts nil
           
           org-export-with-toc nil
           org-export-with-section-numbers nil)
  (:option org-blog-page-head-template
           (list "<link rel=\"icon\" href=\"/static/favicon.ico\">"
                 "<link href= \"/static/style/style.css\" rel=\"stylesheet\" type=\"text/css\" />"))
  
  (:option org-blog-page-header-template
           (concat "<div class=\"home-page\">\n"
                   (format "<a href=\"/\">%s</a>" org-blog-meta-title)
                   "</div>"
                   "<div class=\"nav\">\n"
                   "<div class=\"nav item\">\n"
                   "<a href=\"/tags\">标签</a>\n"
                   "</div>\n"
                   "<div class=\"nav item\">\n"
                   "<a href=\"/archive\">归档</a>\n"
                   "</div>\n"
                   "<div class=\"nav item\">\n"
                   "<a href=\"/about\">关于</a>\n"
                   "</div>\n"
                   "</div>\n"))
  (:option org-blog-third-party
           (list
            (cons 'math
                  (cons
                   "<link rel=\"stylesheet\" href=\"/static/katex/katex.min.css\">"
                   (concat "<script defer src=\"/static/katex/katex.min.js\"></script>\n"
                           "<script defer src=\"/static/katex/auto-render.min.js\"></script>\n"
                           "<script>\n"
                           "let macros = {
\"\\\\C\": \"\\\\mathbb{C}\",
\"\\\\N\": \"\\\\mathbb{N}\",
\"\\\\Q\": \"\\\\mathbb{Q}\",
\"\\\\R\": \"\\\\mathbb{R}\",
\"\\\\Z\": \"\\\\mathbb{Z}\",
\"’\": \"'\"
};\n"
                           "document.addEventListener(\"DOMContentLoaded\", function() {
renderMathInElement(document.getElementById(\"content\"), {
strict: false,
delimiters: [
{\"display\": true,\"left\": \"$$\",\"right\": \"$$\"},
{\"display\": true,\"left\": \"\\\\[\",\"right\": \"\\\\]\"},
{\"display\": true,\"left\": \"\\\\begin{equation}\",\"right\": \"\\\\end{equation}\"},
{\"display\": true,\"left\": \"\\\\begin{equation*}\",\"right\": \"\\\\end{equation*}\"},
{\"display\": true,\"left\": \"\\\\begin{align}\",\"right\": \"\\\\end{align}\"},
{\"display\": true,\"left\": \"\\\\begin{align*}\",\"right\": \"\\\\end{align*}\"},
{\"display\": true,\"left\": \"\\\\begin{alignat}\",\"right\": \"\\\\end{alignat}\"},
{\"display\": true,\"left\": \"\\\\begin{alignat*}\",\"right\": \"\\\\end{alignat*}\"},
{\"display\": true,\"left\": \"\\\\begin{gather}\",\"right\": \"\\\\end{gather}\"},
{\"display\": true,\"left\": \"\\\\begin{CD}\",\"right\": \"\\\\end{CD}\"},
{\"display\": false,\"left\": \"$\",\"right\": \"$\"},
{\"display\": false,\"left\": \"\\\\(\",\"right\": \"\\\\)\"}
],
macros});})\n"
                           "</script>")))

            (cons 'highlight
                  (cons
                   "<link rel=\"stylesheet\" href=\"/static/highlight/dracula.css\">"
                   (concat "<script src=\"/static/highlight/highlight.min.js\"></script>\n"
                           "<script>\n"
                           "Array.from(document.querySelectorAll('#content pre')).forEach(
node => {
let codeElement = node.querySelector(\"code\"),
    excludes = [\"mermaid\"];

if (!codeElement){
return
}
let language = codeElement.getAttribute(\"class\").split(\"-\")[1];
if (excludes.includes(language)) {
return
}

hljs.highlightElement(codeElement);
});\n"
                           "</script>")))

            (cons 'mermaid
                  (cons
                   ""
                   (concat "<script src=\"/static/mermaid/mermaid.min.js\"></script>\n"
                           "<script>\n"
                           "mermaid.initialize({theme: 'neutral', securityLevel: 'loose'});\n"
                           "</script>"))))))

(setup elisp-def
  (:doc "Go to the definition of the symbol at point.")
  (:url "elisp-def" "https://github.com/Wilfred/elisp-def"
        "phi-search" "https://github.com/zk-phi/phi-search")
  (:autoload elisp-def-mode)
  (:hook-into emacs-lisp-mode ielm-mode)
  (:diminish)
  )

(setup imenu-list
  (:doc "A automatically updated buffer with imenu entries.")
  (:url "imenu-list" "https://github.com/bmag/imenu-list")
  (:autoload imenu-list-smart-toggle)
  (:modalka "C-'" imenu-list-smart-toggle)
  (:option imenu-list-focus-after-activation t
           imenu-list-position 'left
           imenu-list-size 0.2)
  (defun dust/imenu-list--truncate-line (&rest _)
    (with-current-buffer (imenu-list-get-buffer-create)
      (setq-local truncate-lines t)))
  (:after imenu-list
    (:with-hook imenu-list-after-jump-hook
      (:hook recenter-top-bottom))
    (:with-hook imenu-list-update-hook
      (:hook dust/imenu-list--truncate-line)))
  )

;; TODO
(setup indent-guide
  (:doc "Show vertical lines to guide indentation")
  (:url "indent-guide" "https://github.com/zk-phi/indent-guide")
  (:autoload indent-guide-mode)
  (:diminish)
  (:hook-into prog-mode)
  (:option indent-guide-recursive t)
  (:after indent-guide
    (set-face-foreground 'indent-guide-face
                         (face-attribute 'font-lock-comment-face
                                         :foreground))))

(setup yuck-mode
  (:doc "Emacs major mode for editing yuck configuration files")
  (:url "yuck-mode" "https://github.com/mmcjimsey26/yuck-mode")
  (:autoload yuck-mode)
  (:file-match "\\.\\(yuck\\)\\'"))

(setup sxhkd-mode
  (:doc "A major mode for editing sxhkdrc")
  (:url "sxhkd-mode" "https://github.com/xFA25E/sxhkd-mode")
  (:autoload sxhkd-mode)
  (:file-match (rx "sxhkdrc" string-end)))

(setup jinja2-mode
  (:doc "Jinja2 mode for emacs")
  (:url "jinja2-mode" "https://github.com/paradoxxxzero/jinja2-mode")
  (:autoload jinja2-mode)
  (:file-match "\\.\\(jinja2\\)\\'"))

(setup tempel
  (:doc "A tiny template package for Emacs")
  (:url "tempel" "https://github.com/minad/tempel"
        "tempel-collection" "https://github.com/Crandel/tempel-collection")
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (require 'tempel-collection)
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (:with-function tempel-setup-capf
    (:hook-into conf-mode
                prog-mode)))

(provide 'setup-all)
;;; setup-all.el ends
