(defun color-theme-sunburst ()
  (interactive)
  (color-theme-install
   '(color-theme-sunburst
     ((background-color . "#000000")
      (foreground-color . "#FFFFFF")
      (cursor-color . "#DAD085"))
     (default ((t (nil))))
     (modeline ((t (:background "DarkRed" :foreground "white"
                                :box (:line-width 1 :style released-button)))))
     (font-lock-builtin-face ((t (:foreground "#3E87E3"))))
     (font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))
     (font-lock-constant-face ((t (:foreground "#3387CC"))))
     (font-lock-doc-string-face ((t (:foreground "#65B042"))))
     (font-lock-string-face ((t (:foreground "#99CF50"))))
     (font-lock-function-name-face ((t (:foreground "#89BDFF"))))
     (font-lock-keyword-face ((t (:foreground "#E28964"))))
     (font-lock-type-face ((t (:underline t :foreground "#89BDFF"))))
     (font-lock-variable-name-face ((t (:foreground "#3E87E3"))))
     (font-lock-warning-face ((t (:bold t :foreground "#FD5FF1"
                                        :background "#562D56"))))
     (py-decorators-face ((t (:foreground "#3387CC"))))
     (py-builtins-face ((t (:foreground "#99CF50"))))
     (py-pseudo-keyword-face ((t (:foreground "#3E87E3"))))
     )
   ))

(defalias 'sunburst #'color-theme-sunburst)

(provide 'sunburst)
