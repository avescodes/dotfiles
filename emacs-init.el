;; File:     ~/.emacs.d/emacs-init.el
;; Author:   Ryan Neufeld <neufelry@gmail.com>
;; Forked from: Burke Libbey <burke@burkelibbey.org>
;; Modified: <2009-01-11 11:44:02 CST>

;; This assumes ~/.emacs contains '(load "~/.emacs.d/emacs-init.el")'

(require 'cl)

(defvar *emacs-load-start* (current-time))
(setq debug-on-error t)

(defvar *user-name* "Ryan Neufeld <neufelry@gmail.com>")
(defvar *default-font*  "Anonymous")

;;; >>> Feature Selection <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *cedet*         t)   ;; Common emacs development tools. Big, but handy.
(defvar *clojure*       t)   ;; Using clojure? (Select slime as well.)
(defvar *color-theme*   t)   ;; Probably disable for GNU Emacs <22
(defvar *fuzzy-find*    t)   ;; Fuzzy find in project
(defvar *git*           t)   ;; Git & Gist integration
(defvar *ido*           t)   ;; Using ido?
(defvar *jess*          t)   ;; Jess, a java expert systems language
(defvar *joust*         t)   ;; Joust package manager
(defvar *merb*          t)   ;; Merb, Rails minor modes
(defvar *ruby*          t)   ;; Ruby
(defvar *slime*         t)   ;; Using lisp?
(defvar *timestamp*     t)   ;; Update "Modified: <>" comments on save
(defvar *yasnippet*     t)   ;; Snippets a la Textmate. Awesomeness, defined.

;;; >>> Configure Load Path <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq emacs-config-path "~/.emacs.d/")
(setq base-lisp-path "~/.emacs.d/site-lisp/")
(defun add-path (p)
  (add-to-list 'load-path (concat base-lisp-path p)))

;; I should really just do this recursively.
(add-path "")
(add-path "markdown-mode")
(add-path "themes")

;;; >>> Loading Packages <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(when *cedet*
  (load-file (concat base-lisp-path "cedet-1.0pre4/common/cedet.el"))
  (semantic-load-enable-code-helpers))

(when *clojure*
  (add-path "swank-clojure")
  (add-path "slime")
  (add-path "clojure-mode")

  (require 'clojure-auto)
  (require 'swank-clojure-autoload)
  (autoload 'slime "slime" nil t)
  (add-hook 'slime-mode 'set-newline-and-indent)
  (add-hook 'clojure-mode-hook '(lambda() (local-set-key "\C-j" 'slime-eval-print-last-expression)))
  (setq swank-clojure-binary "~/.emacs.d/clojure/clojure")


  ;; Slime-javadoc config
  (defun slime-java-describe (symbol-name)
    "Get details on Java class/instance at point."
    (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
    (when (not symbol-name)
      (error "No symbol given"))
    (save-excursion
      (set-buffer (slime-output-buffer))
      (unless (eq (current-buffer) (window-buffer))
        (pop-to-buffer (current-buffer) t))
      (goto-char (point-max))
      (insert (concat "(show " symbol-name ")"))
      (when symbol-name
        (slime-repl-return)
        (other-window 1))))

  (defun slime-javadoc (symbol-name)
    "Get JavaDoc documentation on Java class at point."
    (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
    (when (not symbol-name)
      (error "No symbol given"))
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(javadoc " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1)))

  (add-hook 'slime-connected-hook (lambda ()
                (interactive)
                (slime-redirect-inferior-output)
                (define-key slime-mode-map (kbd "C-c d") 'slime-java-describe)
                (define-key slime-repl-mode-map (kbd "C-c d") 'slime-java-describe)
                (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)
                (define-key slime-repl-mode-map (kbd "C-c D") 'slime-javadoc))))

(when *fuzzy-find*
  (add-path "fuzzy-find-in-project")
  (require 'fuzzy-find-in-project)
  (global-set-key "\C-c\C-f" 'fuzzy-find-in-project)
  (global-set-key "\C-cfr" 'fuzzy-find-project-root))

(when *git*
  (add-path "magit")
  (add-path "gist")
  (require 'magit)
  (require 'gist)
  (let ((private-el (concat *emacs-config-directory* "/private/private.el")))
    (when (file-exists-p private-el)
      (load-file private-el))))

(when *ido*
  (require 'ido)
  (ido-mode t)
  (setq ido-enable-flex-matching t)  ; fuzzy matching is a must have
  (add-hook 'ido-setup-hook  ;;Do I need?
    (lambda ()
      (define-key ido-completion-map [tab] 'ido-complete))))

(when *jess*
  (add-path "jess")
  (autoload 'jess-mode "jess-mode" "Jess Editing Mode" t nil)
  (autoload 'run-jess "inf-jess" "Inferior Jess Mode" t nil)
  (add-hook 'jess-mode-hook
    #'(lambda ()
        (auto-fill-mode t)
        (turn-on-font-lock)))
  (setq auto-mode-alist
    (append auto-mode-alist `(("\\.clp$" . jess-mode)))))

(when *joust*
  (add-to-list 'load-path (concat *emacs-config-directory* "/joust"))
  (require 'joust))

(when *ruby* 
  (add-path "ruby")
  (add-path "ri")
  (add-path "emacs-rails") 

  (require 'rails)
  (require 'find-recursive)
  (require 'snippet)
  (global-set-key "\C-c\C-f" 'rails-goto-file-on-current-line)

  (require 'ri)
  (require 'ruby-block)
  (require 'ruby-mode)
  
  (require 'inf-ruby) ;; Not working yet
  
  ;; Hooks
  (add-hook 'ruby-mode-hook     'set-newline-and-indent)
  (add-hook 'ruby-mode-hook
         '(lambda ()
            (define-key ruby-mode-map "\C-hr"
              'ri)))
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (define-key ruby-mode-map "\C-c\C-r"
                 'ruby-send-region)))
  (add-hook 'ruby-mode-hook
            (lambda()
              (add-hook 'local-write-file-hooks
                        '(lambda()
                           (save-excursion
                             (untabify (point-min) (point-max))
                             (delete-trailing-whitespace)
                             )))
              (set (make-local-variable 'indent-tabs-mode) 'nil)
              (imenu-add-to-menubar "IMENU")
              (define-key ruby-mode-map "\C-m" 'newline-and-indent) ;Not sure if this line is 100% right but it works!
              ))

  (when *merb* 
    (add-path "rinari")
    (require 'rinari-merb)))

(when *timestamp*
  ;; When files have "Modified: <>" in their first 8 lines, fill it in on save.
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-start  "Modified:[   ]+\\\\?[\"<]+")
  (setq time-stamp-end    "\\\\?[\">]")
  (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z"))

(when window-system

  (global-unset-key "\C-z")

  (when *color-theme*
    (require 'color-theme)
    (color-theme-initialize)
    (setq color-theme-is-global t)
    (require 'sunburst)
    (color-theme-sunburst))

  (when *yasnippet*
    (require 'yasnippet)
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/snippets"))

  (when (boundp 'aquamacs-version)
    (one-buffer-one-frame-mode 0)
    (setq mac-allow-anti-aliasing 1))

  (require 'java-complete)
  (require 'lorem-ipsum)
  (require 'mode-compile)
  (require 'unbound))

;;; ----[ Custom Set Variables ]------------------------------------------------

(custom-set-variables
  '(bookmark-save-flag       1)          ;; Autosave bookmarks on create/etc.
  '(c-default-style          "k&r")     ;; use k&r style for C indentation
  '(case-fold-search         t)          ;; case-insensitive search
  '(column-number-mode       t)          ;; show column number in the bottom bar
  '(default-major-mode       'text-mode) ;; open unknown in text mode
  '(default-tab-width        2)          ;; tabs of width 2
  '(global-font-lock-mode    t nil (font-lock)) ;; Syntax higlighting
  '(indent-tabs-mode         nil)        ;; soft tabs
  '(inhibit-startup-message  t)          ;; no startup message
  '(menu-bar-mode            nil)        ;; eww. bad.
  '(minibuffer-max-depth     nil)        ;; enable multiple minibuffers
  '(ring-bell-function       'ignore)    ;; turn off system beep
  '(scroll-bar-mode          nil)        ;; no scroll bar
  '(show-paren-mode          t)          ;; highlight matching paren on hover
  '(standard-indent          2)
  '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80)))    ;; Make tabs work better
  '(tool-bar-mode            nil)        ;; eww. bad.
  '(transient-mark-mode      t)          ;; highlight the marked region
  ;; buffers with duplicate names will be dir/file, not file<n>
  '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;;; ----[ Tab Functionality ]---------------------------------------------------

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(defun smart-tab ()
  "If mark is active, indents region. Else if point is at the end of a symbol,
   expands it. Else indents the current line. Acts as normal in minibuffer."
  (interactive)
  (if (boundp 'ido-cur-item)
      (ido-complete)
    (if (minibufferp)
        (minibuffer-complete)
      (if mark-active
          (indent-region (region-beginning) (region-end))
        (if (looking-at "\\_>")
            (hippie-expand nil)
          (indent-for-tab-command))))))
(global-set-key [(tab)] 'smart-tab)

;;; ----[ Functions ]-----------------------------------------------------------

;; Instead of pressing Enter > Tab all the time.
(defun set-newline-and-indent ()
  (local-set-key "\C-m" 'newline-and-indent))

(defun insert-name-email ()
  (interactive)
  (insert *user-name*))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
  two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
        ((not prefix) "%Y-%m-%d")
        ((equal prefix '(4)) "%d.%m.%Y")
        ((equal prefix '(16)) "%A, %d. %B %Y")))
      (system-time-locale "en_US"))
    (insert (format-time-string format))))

;; Switch back a window
(defun go-back-window ()
  (interactive)
  (select-window (previous-window)))

;; how patronizing could an editor possibly be? 'y' will do...
(fset 'yes-or-no-p 'y-or-n-p)

;;; ----[ Key Bindings ]--------------------------------------------------------

(set-register ?E '(file . "~/.emacs.d/emacs-init.el")) ;; Easy access!
(set-register ?Z '(file . "~/.emacs.d/zshrc.zsh"))     ;; Ditto.
(set-register ?T '(file . "~/Documents/todo.txt"))     ;; And more for me

(windmove-default-keybindings 'meta)

(global-set-key "\C-cd"      'insert-date)
(global-set-key "\C-ce"      'insert-name-email)

;; How did I live without this?
(global-set-key "\C-w"       'backward-kill-word)
(global-set-key "\C-x\C-k"   'kill-region)

;; Select all. Apparently some morons bind this to C-a.
(global-set-key "\C-c\C-a"   'mark-whole-buffer)
(global-set-key "\C-ct"      '(lambda () (interactive) (ansi-term "/bin/zsh")))

;; Alternative to RSI-inducing M-x, and extra insurance.
(global-set-key "\C-xm"      'execute-extended-command)
(global-set-key "\C-cm"      'execute-extended-command)
(global-set-key "\C-x\C-m"   'execute-extended-command)
(global-set-key "\C-c\C-m"   'execute-extended-command)

(global-set-key "\C-c\C-q" 'query-replace)
(global-set-key "\C-c\C-e" 'query-replace-regex)

(global-set-key "\C-ch" 'hs-hide-block)
(global-set-key "\C-cs" 'hs-show-block)

(global-set-key "\C-xg" 'magit-status)

;; short form for query regex replace
(defalias 'qrr 'query-replace-regexp)

;;; ----[ Backups and Autosaves ]-----------------------------------------------
(setq
  backup-by-copying      t             ;; don't clobber symlinks
  backup-directory-alist
    '(("." . "~/.emacs.d/autosaves/")) ;; don't litter
  delete-old-versions    t
  kept-new-versions      6
  kept-old-versions      2
  version-control        t)            ;; use versioned backups


;;; ----[ Mode Hooks and Auto loading ]-----------------------------------------
(add-hook 'c-mode-common-hook 'set-newline-and-indent)
(add-hook 'clojure-mode-hook  'set-newline-and-indent)
(add-hook 'ruby-mode-hook     'set-newline-and-indent)
(add-hook 'lisp-mode          'set-newline-and-indent)

;; No syntax highlighting on plain text
(add-hook 'text-mode-hook     'turn-off-auto-fill)

;; C and C-ish
(add-hook 'c-mode-common-hook 'set-newline-and-indent)
(add-hook 'erlang-mode-hook 'set-newline-and-indent)

;;This isn't working
(add-hook 'lisp-mode          'set-newline-and-indent)

(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'haml-mode "haml-mode" nil t)
(autoload 'yaml-mode "yaml-mode" nil t)
(autoload 'markdown-mode "markdown-mode" nil t)
(setq auto-mode-alist
  (nconc
    '(("\\.xml$"   . nxml-mode))
    '(("\\.html$"  . nxml-mode))
    '(("\\.haml$"  . haml-mode))
    '(("\\.yml$"   . yaml-mode))
    '(("\\.json$"  . yaml-mode))
    '(("\\.rb$"    . ruby-mode))
    '(("\\.md$"    . markdown-mode))
    '(("\\.zsh$"   . sh-mode))
    '(("Rakefile$" . ruby-mode))
    '(("\\.pro$" . prolog-mode))
    auto-mode-alist))

(setq magic-mode-alist ())

(message "Loaded .emacs in %ds" (destructuring-bind (hi lo ms) (current-time)
  (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(setq debug-on-error nil)