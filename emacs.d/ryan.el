;; General
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)                    ; Disable emacs-starter-kits line highlighting

(setq initial-scratch-message nil)                                         ; *scratch* starts empty
(when (locate-library "clojure-mode")                                      ; Set *scratch* to Clojure mode
  (setq initial-major-mode 'clojure-mode))

(projectile-global-mode)                                                   ; Quickly navigate projects using Projectile (C-c p C-h for available commands)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths) ; Projectile shows full relative paths


;; Visual
(load-theme 'twilight t)                                                   ; Load my preferred theme, twilight
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)                        ; Enable rainbow delimiters when programming

(global-linum-mode t)                                                      ; Always show line numbers on left
(setq linum-format "%4d ")                                                 ; Line numbers gutter should be four characters wide

(line-number-mode 1)                                                       ; Mode line shows line numbers
(column-number-mode 1)                                                     ; Mode line shows column numbers

(setq-default tab-width 2)                                                 ; Tab width of 2
(fset 'yes-or-no-p 'y-or-n-p)                                              ; Emacs prompts should accept "y" or "n" instead of the full word


;; Clojure
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist)) ; *.cljs are Clojure files

(defun clojure-generate-etags (project-root)                               ; Attempt at a Clojure etags generating fn
  "Create tags file for clojure project."

  (interactive "DProject Root:")
  (eshell-command
   (format "find %s -name \'*.clj\' -or -name \'*.cljs\' | xargs etags --regex=@$HOME/.emacs.d/clojure.etags -o %s/TAGS" project-root project-root)))


;; nREPL customizations
(setq nrepl-popup-stacktraces nil)                                         ; Don't aggresively popup stacktraces
(setq nrepl-popup-stacktraces-in-repl t)                                   ; Display stacktrace inline

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)          ; Enable eldoc - shows fn argument list in echo area
(add-hook 'nrepl-mode-hook 'paredit-mode)                                  ; Use paredit in *nrepl* buffer

(add-to-list 'same-window-buffer-names "*nrepl*")                          ; Make C-c C-z switch to *nrepl*


;; Ido-mode customizations
(setq ido-decorations                                                      ; Make ido-mode display vertically
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

(add-hook 'ido-setup-hook                                                  ; Navigate ido-mode vertically
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))
