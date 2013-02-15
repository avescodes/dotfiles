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
                      projectile
                      ack-and-a-half
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-safe-themes (quote ("0c8ffb272e65e2ffc49b0d5755d7db137931c5e3ed47890d7a6de07717eaa2e8" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(fci-rule-color "#383838"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fundamental-mode-default ((t (:inherit autoface-default))) t)
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))))
 '(paren-face-match ((((class color)) (:inherit nil))) t)
 '(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red")))))
