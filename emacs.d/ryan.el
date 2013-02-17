(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; Colours and Formatting
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/twilight-theme")
(load-theme 'twilight t)

(setq-default tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)


(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(require 'hlinum)
(setq linum-format "%4d ")
(global-linum-mode t)

(line-number-mode 1) ; have line numbers and
(column-number-mode 1) ; column numbers in the mode line

;; scratch
(setq initial-scratch-message nil)
(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

;; Clojure
(setq auto-mode-alist (cons '("\\.dtm$" . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; Rainbows!
(projectile-global-mode)

(defun clojure-maven-etags (project-root)
  "Create tags file for clojure project."

  (interactive "DProject Root:")
  (eshell-command
   (format "find %s -name \'*.clj\' -or -name \'*.cljs\' | xargs etags --regex=@$HOME/.emacs.d/clojure.etags -o %s/TAGS" project-root project-root)))


;; nREPL customizations
(setq nrepl-popup-stacktraces nil)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook
          (lambda ()
            (linum-mode -1)
            (column-number-mode -1)
            (line-number-mode -1)))

(global-set-key (kbd "C-c C-j") 'nrepl-jack-in)
(add-to-list 'same-window-buffer-names "*nrepl*") ; Make C-c C-z switch to *nrepl*

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

;; Syntax highlighting customizations
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fundamental-mode-default ((t (:inherit autoface-default))) t)
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))))
 '(paren-face-match ((((class color)) (:inherit nil))))
 '(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red")))))
