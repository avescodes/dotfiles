(global-hl-line-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

(setq initial-scratch-message nil)

(add-hook 'clojure-mode-hook
  (lambda ()
    (setq inferior-lisp-program "lein repl")))
