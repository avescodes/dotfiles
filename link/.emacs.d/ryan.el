;; General
;; =======
;; *scratch* starts empty
(setq initial-scratch-message nil)

;; Set *scratch* to Clojure mode
(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

;; Quickly navigate projects using Projectile (C-c p C-h for available commands)
(projectile-global-mode)
;; Projectile shows full relative paths
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

;; Make the Shift-up binding work in iTerm
(define-key input-decode-map "\e[1;2A" [S-up])

(global-auto-revert-mode nil)

;; Visual
;; ======

(load-theme 'monokai t)

;; Enable rainbow delimiters when programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Always show line numbers on left
(global-linum-mode t)
;; Line numbers gutter should be four characters wide
(setq linum-format "%4d ")

;; Mode line shows line numbers
(line-number-mode 1)

;; Tab width of 2
(setq-default tab-width 2)

;; Emacs prompts should accept "y" or "n" instead of the full word
(fset 'yes-or-no-p 'y-or-n-p)

;; Evil-mode
;; ---------

(require 'evil)
;;(evil-mode 1)
(setq evil-shift-width 2)

;; C-c exits insert mode
(define-key evil-insert-state-map (kbd "C-c") 'normal-mode)

;; Modes
;; =====
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; Markdown
;; --------
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; AsciiDoc
;; --------
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))
(add-hook 'adoc-mode-hook 'cider-mode) ;; For book writing

;; Clojure
;; -------
(rename-modeline "clojure-mode" clojure-mode "λ")
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;; Paredit
;; -------
(dolist (mode '(emacs-lisp lisp clojure clojurescript))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'paredit-mode))

;; Scala
;; -----
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; Generate the initial TAGS file for a project. (deprecated)
(defun create-tags (project-root)
  "Create TAGS file for project."
  (interactive "DProject Root:")
  (eshell-command
   (format "ctags -f %s/TAGS -Re %s" project-root project-root)))


;; nREPL customizations
;; --------------------
;; Don't show cider/nrepl's special buffer
(setq nrepl-hide-special-buffers t)
;; ;; Don't pop-up error buffers
(setq cider-popup-on-error nil)
;; ;; Show stacktraces in REPL
(setq cider-repl-popup-stacktraces t)
;; ;; Don't auto-display REPL
;(setq cider-repl-pop-to-buffer-on-connect nil)

;; ;; Enable eldoc - shows fn argument list in echo area
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; ;; Make C-c C-z switch to *nrepl*
(setq cider-repl-display-in-current-window t)

;; ;; Key bindings *just* for nrepl
(add-hook 'cider-repl-mode-hook
          (lambda ()
            ;; "Up" is history backwards
            (define-key cider-repl-mode-map [down] 'cider-repl-forward-input)
            ;; "Down" is history forwards
            (define-key cider-repl-mode-map [up] 'cider-repl-backward-input)))

(defun rkn-print-results-on-next-line (value)
  (end-of-line)
  (newline)
  (insert (format ";; -> %s" value)))

(defun rkn-nrepl-eval-newline-comment-print-handler (buffer)
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (rkn-print-results-on-next-line value)))
                               '()
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (rkn-print-results-on-next-line value)))
                               '()))

(defun rkn-nrepl-interactive-eval-print (form)
  "Evaluate the given FORM and print the value in the current
  buffer on the next line as a comment."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (rkn-nrepl-eval-newline-comment-print-handler buffer)
                       nrepl-buffer-ns)))

(defun rkn-eval-expression-at-point-to-comment ()
  (interactive)
  (let ((form (cider-last-expression)))
    (rkn-nrepl-interactive-eval-print form)))

;; From http://blog.jenkster.com/2013/12/a-cider-excursion.html
;; Put [org.clojure/tools.namespace "0.2.4"] in ~/.lein/profiles.clj's
;; :user :dependencies vector
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(add-hook 'cider-mode-hook
          (lambda ()
            (define-key cider-mode-map (kbd "C-M-j") 'rkn-eval-expression-at-point-to-comment)
            (define-key cider-mode-map (kbd "C-M-r") 'cider-namespace-refresh)))


;; Ido-mode customizations
;; -----------------------

;; Make ido-mode display vertically
(setq ido-decorations
      (quote
       ("\n-> "           ; Opening bracket around prospect list
        ""                ; Closing bracket around prospect list
        "\n   "           ; separator between prospects
        "\n   ..."        ; appears at end of truncated list of prospects
        "["               ; opening bracket around common match string
        "]"               ; closing bracket around common match string
        " [No match]"     ; displayed when there is no match
        " [Matched]"      ; displayed if there is a single match
        " [Not readable]" ; current diretory is not readable
        " [Too big]"      ; directory too big
        " [Confirm]")))   ; confirm creation of new file or buffer

;; Navigate ido-mode vertically
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))


;; Magit
;; -----
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

;; Smex
;; ----
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
