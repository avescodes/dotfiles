;; File:     ~/.emacs.d/emacs-init.el
;; Author:   Burke Libbey <burke@burkelibbey.org>
;; Modified: <2008-08-27 16:01:20 CDT>

;; This assumes ~/.emacs contains '(load "~/.emacs.d/emacs-init.el")'

(setq emacs-load-start-time (current-time))
(setq debug-on-error t)

(defvar *default-font*  "pragmata tt")
(defvar *folding*       nil)
(defvar *emacs-server*  nil)
(defvar *tramp*         t)
(defvar *cedet*         t)
(defvar *ecb*           t)
(defvar *icicles*       t)
(defvar *rails*         t)
(defvar *color-theme*   t)
(defvar *speedbar*      t)
(defvar *yasnippet*     t)
(defvar *timestamp*     t)
(defvar *slime*         nil)

(setq base-lisp-path "~/.emacs.d/lisp/")

(defun add-path (p)
  (add-to-list 'load-path (concat base-lisp-path p)))

(add-path "")
(add-path "icicles")
(add-path "rails")
(add-path "ecb")


(when *cedet*
  (load-file (concat base-lisp-path "cedet-1.0pre4/common/cedet.el"))
  (semantic-load-enable-code-helpers))

(when window-system

  (global-unset-key "\C-z")

  (when *color-theme*
    (require 'color-theme)
    (color-theme-initialize)
    (setq color-theme-is-global t)
    (color-theme-comidia))

  (when *speedbar*
    (require 'speedbar)
    (speedbar t)
    (speedbar-disable-update)
    (global-set-key "\C-c\C-s" 'speedbar))

  (when *ecb*
    (require 'ecb))

  (when *rails*
    (require 'rcodetools)
    (require 'rails)
    (global-set-key "\C-c\C-f" 'rails-goto-file-on-current-line))

  (when *icicles*
    (require 'icicles)
    (when *rails*
      (require 'icicles-rcodetools))
    (icy-mode))

  (when *yasnippet*
    (require 'yasnippet)
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/snippets"))


  (when (not (boundp 'aquamacs-version))
    (set-default-font (concat *default-font* "-10"))
    (defun change-font-size (arg)
      (interactive "P")
      (set-default-font (concat *default-font* "-" (number-to-string arg))))
    (global-set-key "\C-cf" 'change-font-size))

  (when (boundp 'aquamacs-version)
    (one-buffer-one-frame-mode 0)
    (setq mac-allow-anti-aliasing nil)
    (set-default-font "-apple-monaco-medium-r-normal--10-120-72-72-m-120-mac-roman")))


(when *emacs-server*
  (global-set-key "\C-x\C-c" 'server-edit)
  (global-set-key "\C-x\#" 'kill-emacs)
  (add-hook 'server-switch-hook
            (lambda nil
              (let ((server-buf (current-buffer)))
                (bury-buffer)
                (switch-to-buffer-other-frame server-buf))))
  (add-hook 'server-done-hook 'delete-frame))

;;{{{ Custom-set-variables


(custom-set-variables
  '(global-font-lock-mode    t nil (font-lock)) ;; Syntax higlighting
  ;; buffers with duplicate names will be dir/file, not file<n>
  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
  '(minibuffer-max-depth     nil)        ;; enable multiple minibuffers
  '(indent-tabs-mode         nil)        ;; soft tabs
  '(default-tab-width        2)          ;; tabs of width 2
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

;;}}}

(when *timestamp*
  ;; When files have "Modified: <>" in their first 8 lines, fill it in on save.
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-start  "Modified:[   ]+\\\\?[\"<]+")
  (setq time-stamp-end    "\\\\?[\">]")
  (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z"))

(set-register ?E '(file . "~/.emacs.d/emacs-init.el")) ; Easy access!
(set-register ?Z '(file . "~/.zshrc")) ; (C-x r j <r>)


;; how patronizing could an editor possibly be? 'y' will do...
(fset 'yes-or-no-p 'y-or-n-p)

;; Switch back a window
(defun go-back-window ()
  (interactive)
  (select-window (previous-window)))

;; Instead of pressing Enter > Tab all the time.
(defun set-newline-and-indent ()
  (local-set-key "\C-m" 'newline-and-indent))

(defun insert-name-email ()
  (interactive)
  (insert "Burke Libbey <burke@burkelibbey.org>"))

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

(global-set-key [(meta up)] '(lambda() (interactive) (scroll-other-window -1)))
(global-set-key [(meta down)] '(lambda() (interactive) (scroll-other-window 1)))
(global-set-key [(meta -)] '(lambda() (interactive) (shrink-window 1)))
(global-set-key [(meta =)] '(lambda() (interactive) (shrink-window -1)))

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

(global-set-key "\C-x\C-r"   'query-replace-regexp)

(global-set-key "\C-cw"      'toggle-truncate-lines)

(global-set-key "\C-cg"      'goto-line)
(global-set-key "\C-cG"      'goto-char)
(global-set-key "\C-c\C-k"   'kill-region)

;; I'll take "functions that should have keybindings" for 100, Alex.
(global-set-key "\C-cc"      'comment-region)
(global-set-key "\C-cu"      'uncomment-region)

;; Next and previous for grep and compile errors
(global-set-key "\C-cn"      'next-error)
(global-set-key "\C-cp"      'previous-error)

;; Disable right click. yuck.
(global-set-key (kbd "<down-mouse-2>")  '())
(global-set-key (kbd "<mouse-2>")       '())


;;{{{ Backups and Autosaves

(setq
  backup-by-copying      t             ;; don't clobber symlinks
  backup-directory-alist
    '(("." . "~/.emacs.d/autosaves/")) ;; don't litter
  delete-old-versions    t
  kept-new-versions      6
  kept-old-versions      2
  version-control        t)            ;; use versioned backups

;;}}}




;;{{{ Git Stuff

(require 'vc-git)
(add-to-list 'vc-handled-backends 'git)
(require 'git)
(autoload 'git-blame-mode "git-blame"
           "Minor mode for incremental blame for Git." t)
(global-set-key "\C-xg" 'git-status)

;;}}}

;; Hippie Expand
(when (require 'hippie-exp nil t)
  (global-set-key [C-tab] 'hippie-expand))

;; No syntax highlighting on plain text
(add-hook 'text-mode-hook     'turn-off-auto-fill)

;; C and C-ish
(add-hook 'c-mode-common-hook 'set-newline-and-indent)


(defun enable-rct ()
  (local-set-key [C-tab] 'rct-complete-symbol--icicles))

;; Ruby
(add-hook 'ruby-mode-hook     'set-newline-and-indent)
(add-hook 'ruby-mode-hook     'enable-rct)

(add-hook 'lisp-mode          'set-newline-and-indent)

;; Remote File Editing
(when *tramp*
  (require 'tramp)
  (setq tramp-default-method "scpc"))

(when *slime*
  (require 'slime)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-hook 'slime-mode 'set-newline-and-indent)
  (slime-setup))

;;{{{ Code Folding

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
    '(("Rakefile$" . ruby-mode))
    auto-mode-alist))

(setq magic-mode-alist ())



(when *ecb*
  (ecb-activate))


(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

(setq debug-on-error nil)

