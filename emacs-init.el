;; File:     ~/.emacs.d/emacs-init.el
;; Author:   Ryan Neufeld <neufelry@gmail.com>
;; Forked from: Burke Libbey <burke@burkelibbey.org>
;; Modified: <2008-12-11 00:36:54 CST>

;; This assumes ~/.emacs contains '(load "~/.emacs.d/emacs-init.el")'

(require 'cl)

(defvar *emacs-load-start* (current-time))
(setq debug-on-error t)

(defvar *user-name* "Ryan Neufeld <neufelry@gmail.com>")

(defvar *default-font*  "Anonymous")

;;; >>> Feature Selection <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *folding*       nil) ;; Code folding (buggy).
(defvar *tramp*         t)   ;; Enable remote file access
(defvar *cedet*         t)   ;; Common emacs development tools. Big, but handy.
(defvar *color-theme*   t)   ;; Probably disable for GNU Emacs <22
(defvar *yasnippet*     t)   ;; Snippets a la Textmate. Awesomeness, defined.
(defvar *timestamp*     t)   ;; Update "Modified: <>" comments on save
(defvar *hippie-expand* nil) ;; Unintelligent code completion
(defvar *slime*         t)   ;; Using lisp?
(defvar *clojure*       t)   ;; Using clojure? (Select slime as well.)
(defvar *ido*           t)   ;; Using ido?
(defvar *fuzzy-find*    t)   ;; Fuzzy find in project


(defvar *user-name* "Burke Libbey <burke@burkelibbey.org>")
(defvar *default-font*  "pragmata tt")

;;; >>> Configure Load Path <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq emacs-config-path "~/.emacs.d/")
(setq base-lisp-path "~/.emacs.d/site-lisp/")
(defun add-path (p)
  (add-to-list 'load-path (concat base-lisp-path p)))

;; I should really just do this recursively.
(add-path "")

(add-path "slime")
(add-path "rails")
(add-to-list 'load-path "~/.emacs.d/themes")

(when *ido*
  (require 'ido)
  (ido-mode t)
  (setq ido-enable-flex-matching t)  ; fuzzy matching is a must have
  (add-hook 'ido-setup-hook  ;;Do I need?
    (lambda ()
      (define-key ido-completion-map [tab] 'ido-complete))))

(when *hippie-expand*
  (autoload 'hippie-expand "hippie-exp" t)
  (eval-after-load "hippie-exp"
    '(progn (global-set-key [C-tab] 'hippie-expand))))

(when *cedet*
  (load-file (concat base-lisp-path "cedet-1.0pre4/common/cedet.el"))
  (semantic-load-enable-code-helpers))

(when window-system

  (global-unset-key "\C-z")

  (when *color-theme*
    (require 'color-theme)
    (color-theme-initialize)
    (setq color-theme-is-global t)
    (require 'sunburst)
    (color-theme-sunburst))

  (autoload 'speedbar "speedbar" t)
  (eval-after-load "speedbar"
    '(progn (speedbar-disable-update)))
  (global-set-key "\C-c\C-s" 'speedbar)

  (require 'rcodetools)
  (require 'rails)
  (require 'find-recursive)
  (require 'snippet)
  (require 'inf-ruby)
  (global-set-key "\C-c\C-f" 'rails-goto-file-on-current-line)

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
  (require 'ri)
  (require 'ruby-block)
  ;(require 'ruby-compilation)
  (require 'ruby-electric)
  (require 'ruby-mode)
  (require 'unbound))

(custom-set-variables
  '(global-font-lock-mode    t nil (font-lock)) ;; Syntax higlighting
  ;; buffers with duplicate names will be dir/file, not file<n>
  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
  '(minibuffer-max-depth     nil)        ;; enable multiple minibuffers
  '(indent-tabs-mode         nil)        ;; soft tabs
  '(default-tab-width        2)          ;; tabs of width 2
  '(standard-indent          2)
  '(scroll-bar-mode          nil)        ;; no scroll bar
  '(tool-bar-mode            nil)        ;; eww. bad.
  '(menu-bar-mode            nil)        ;; eww. bad.
  '(column-number-mode       t)          ;; show column number in the bottom bar
  '(show-paren-mode          t)          ;; highlight matching paren on hover
  '(case-fold-search         t)          ;; case-insensitive search
  '(transient-mark-mode      t)          ;; highlight the marked region
  '(inhibit-startup-message  t)          ;; no startup message
  '(default-major-mode       'text-mode) ;; open unknown in text mode
  '(ring-bell-function       'ignore)    ;; turn off system beep
  '(bookmark-save-flag       1)          ;; Autosave bookmarks on create/etc.
  '(c-default-style          "k&r"))     ;; use k&r style for C indentation

(when *timestamp*
  ;; When files have "Modified: <>" in their first 8 lines, fill it in on save.
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-start  "Modified:[   ]+\\\\?[\"<]+")
  (setq time-stamp-end    "\\\\?[\">]")
  (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z"))

(set-register ?E '(file . "~/.emacs.d/emacs-init.el")) ;; Easy access!
(set-register ?Z '(file . "~/.emacs.d/zshrc.zsh"))     ;; Ditto.

(when *fuzzy-find*
  (add-path "fuzzy-find-in-project")
  (require 'fuzzy-find-in-project)
  (global-set-key "\C-c\C-f" 'fuzzy-find-in-project)
  (global-set-key "\C-cfr" 'fuzzy-find-project-root))

;; how patronizing could an editor possibly be? 'y' will do...
(fset 'yes-or-no-p 'y-or-n-p)

;; Switch back a window
(defun go-back-window ()
  (interactive)
  (select-window (previous-window)))

(defun smart-tab ()
  "If mark is active, indents region. Else if point is at the end of a symbol,
   expands it. Else indents the current line. Acts as normal in minibuffer."
  (interactive)
  (if (boundp 'ido-cur-item)
      (ido-complete)
    (if (minibufferp)
        (unless (minibuffer-complete)
          (dabbrev-expand nil))
      (if mark-active
          (indent-region (region-beginning)
                         (region-end))
        (if (looking-at "\\_>")
            (dabbrev-expand nil)
          (indent-for-tab-command))))))
(global-set-key [(tab)] 'smart-tab)



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


;(global-set-key (kbd "<down>") '())
;(global-set-key (kbd "<up>") '())
;(global-set-key (kbd "<right>") '())
;(global-set-key (kbd "<left>") '())

(global-set-key [(meta up)]   '(lambda() (interactive) (scroll-other-window -1)))
(global-set-key [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
(global-set-key [(meta -)]    '(lambda() (interactive) (shrink-window 1)))
(global-set-key [(meta =)]    '(lambda() (interactive) (shrink-window -1)))

(global-set-key "\C-cd"      'insert-date)
(global-set-key "\C-ce"      'insert-name-email)

;; How did I live without this?
(global-set-key "\C-w"       'backward-kill-word)
(global-set-key "\C-x\C-k"   'kill-region)

;; Select all. Apparently some morons bind this to C-a.
(global-set-key "\C-c\C-a"   'mark-whole-buffer)

;; Alternative to RSI-inducing M-x, and extra insurance.
(global-set-key "\C-xm"      'execute-extended-command)
(global-set-key "\C-cm"      'execute-extended-command)
(global-set-key "\C-x\C-m"   'execute-extended-command)
(global-set-key "\C-c\C-m"   'execute-extended-command)

(global-set-key "\C-c\C-r" 'query-replace)
(global-set-key "\C-c\C-e" 'query-replace-regex)


;; short form for query regex replace
(defalias 'qrr 'query-replace-regexp)

;;; >>> Backups and Autosaves <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
  backup-by-copying      t             ;; don't clobber symlinks
  backup-directory-alist
    '(("." . "~/.emacs.d/autosaves/")) ;; don't litter
  delete-old-versions    t
  kept-new-versions      6
  kept-old-versions      2
  version-control        t)            ;; use versioned backups

;;; >>> Git <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'vc-git)
(add-to-list 'vc-handled-backends 'git)
(require 'git)
(autoload 'git-blame-mode "git-blame"
           "Minor mode for incremental blame for Git." t)
(global-set-key "\C-xg" 'git-status)



;; No syntax highlighting on plain text
(add-hook 'text-mode-hook     'turn-off-auto-fill)

;; C and C-ish
(add-hook 'c-mode-common-hook 'set-newline-and-indent)
(add-hook 'erlang-mode-hook 'set-newline-and-indent)

;; Ruby
(add-hook 'ruby-mode-hook     'set-newline-and-indent)
(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (define-key ruby-mode-map "C-m" 'newline-and-indent) ;Not sure if this line is 100% right but it works!
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))
;(add-hook 'ruby-mode-hook     'enable-rct)
;;This isn't working
(add-hook 'lisp-mode          'set-newline-and-indent)

;; Remote File Editing
(when *tramp*
  (require 'tramp)
  (setq tramp-default-method "ssh"))



(when *clojure*
  (add-path "swank-clojure")
  (add-path "slime")
  (add-path "clojure-mode")

  (require 'clojure-auto)
  (require 'slime)
  (add-hook 'slime-mode 'set-newline-and-indent)
  (add-hook 'clojure-mode-hook '(lambda() (local-set-key "\C-j" 'slime-eval-print-last-expression)))
  (slime-setup)
  (setq swank-clojure-binary "~/opt/clojure-extra/sh-script/clojure")
  (require 'swank-clojure-autoload)

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

;; Make "*.pro" files load in prolog-mode
(setq auto-mode-alist (cons '("\\.pro\\'" . prolog-mode) auto-mode-alist))

;;; >>> Code Folding <<< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If only this didn't suck so hard...
(when *folding*
  (require 'folding)
  (folding-mode-add-find-file-hook)
  (setq folding-mode-marks-alist
    (nconc
      '((ruby-mode       "#{{{"    "#}}}" ))
      '((conf-space-mode "#{{{"    "#}}}" ))
      '((scheme-mode     ";{{{"    ";}}}" ))
      '((clojure-mode    ";{{{"    ";}}}" ))
      '((css-mode       "/*{{{"   "/*}}}" ))
      '((nxml-mode    "<!--{{{" "<!--}}}" ))
      folding-mode-marks-alist))
  (add-hook 'find-file-hooks 'folding-mode)
  (global-set-key "\C-co" 'folding-open-buffer)
  (global-set-key "\C-cf" 'folding-toggle-show-hide)
  (global-set-key "\C-cc" 'folding-whole-buffer))

;;}}}


(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'haml-mode "haml-mode" nil t)
(autoload 'yaml-mode "yaml-mode" nil t)
(setq auto-mode-alist
  (nconc
    '(("\\.xml$"   . nxml-mode))
    '(("\\.html$"  . nxml-mode))
    '(("\\.haml$"  . haml-mode))
    '(("\\.yml$"   . yaml-mode))
    '(("\\.json$"  . yaml-mode))
    '(("\\.rb$"    . ruby-mode))
    '(("\\.js$"    . js2-mode))
    '(("Rakefile$" . ruby-mode))
    auto-mode-alist))

(setq magic-mode-alist ())


(message "Loaded .emacs in %ds" (destructuring-bind (hi lo ms) (current-time)
  (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(setq debug-on-error nil)


(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
"This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
(interactive)
(if (minibufferp)
(unless (minibuffer-complete)
(dabbrev-expand nil))
(if mark-active
(indent-region (region-beginning)
(region-end))
(if (looking-at "\\_>")
(dabbrev-expand nil)
(indent-for-tab-command)))))




















