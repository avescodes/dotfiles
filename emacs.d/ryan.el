(global-hl-line-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

(setq initial-scratch-message nil)

;; nREPL customizations
(setq nrepl-popup-stacktraces nil)
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
