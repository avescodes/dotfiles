(global-hl-line-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'zenburn t)

(setq initial-scratch-message nil)

;; nREPL customizations
(setq nrepl-popup-stacktraces nil)
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

(line-number-mode 1) ; have line numbers and
(column-number-mode 1) ; column numbers in the mode line
(global-linum-mode 1) ; add line numbers on the left

(setq x-select-enable-clipboard t
      make-backup-files nil
      auto-save-default nil
      diff-switches "-u -w"
      whitespace-style '(trailing lines space-before-tab
                                  face indentation space-after-tab))
(setq-default tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)

(defcustom clj-dir "/Users/ryan/code/github/clojure"
  "Path to Clojure source directory."
  :type 'string
  :group 'cljrepl)
