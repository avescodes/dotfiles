(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-ruby
                      starter-kit-js
                      starter-kit-bindings
                      clojure-mode
                      nrepl
                      markdown-mode
                      rainbow-delimiters
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fundamental-mode-default ((t (:inherit autoface-default))) t)
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))))
 '(paren-face-match ((((class color)) (:inherit nil))))
 '(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red")))))
