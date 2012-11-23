(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

(setq initial-scratch-message nil)

(setq-default tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)

;; Make the scratch clojure
(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

(setq auto-mode-alist (cons '("\\.dtm$" . clojure-mode) auto-mode-alist))

;; Linum Mode
(line-number-mode 1) ; have line numbers and
(column-number-mode 1) ; column numbers in the mode line
(add-hook 'clojure-mode-hook 'linum-mode)

;; RAINBOWS!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; nREPL customizations
(setq nrepl-popup-stacktraces nil)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'paredit-mode)

(setq x-select-enable-clipboard t
      make-backup-files nil
      auto-save-default nil
      diff-switches "-u -w"
      whitespace-style '(trailing lines space-before-tab
                                  face indentation space-after-tab))
