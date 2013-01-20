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
