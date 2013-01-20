(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; Colours and Formatting
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/twilight-theme")
(load-theme 'twilight t)

(setq-default tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)


(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(require 'hlinum)
(line-number-mode 1) ; have line numbers and
(column-number-mode 1) ; column numbers in the mode line
(setq linum-format "%4d ")
(global-linum-mode t)

;; scratch
(setq initial-scratch-message nil)
(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

;; Clojure
(setq auto-mode-alist (cons '("\\.dtm$" . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; Rainbows!

;; nREPL customizations
(setq nrepl-popup-stacktraces nil)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'paredit-mode)

;; IDO mode ---------------------------------------------------------
;; Make ido-mode list things vertically
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

;; And let us use standard navagation keys that make sense vertically
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))
