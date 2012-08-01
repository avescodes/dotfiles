(global-hl-line-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'zenburn t)

(setq initial-scratch-message nil)

(add-hook 'clojure-mode-hook
  (lambda ()
    (setq inferior-lisp-program "/Users/ryan/bin/lein1 repl")))

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

(defun cljrepl ()
  "Launch a Clojure REPL."
  (interactive)
  (let ((clj-jar (concat clj-dir "/clojure.jar")))
    (if (file-exists-p clj-jar)
        (inferior-lisp (concat "java -cp " clj-jar " clojure.main"))
      (when (yes-or-no-p "clojure.jar not found.  Build?")
        (if (shell-command (concat "cd " clj-dir " && ant"))
            (cljrepl)
          (message "Building Clojure failed."))))))

(defun leinrepl ()
  "Launch a Leiningen REPL for current file's project. Runs only
   if lib exists.  Requires cl."
  (interactive)
  (if (not (executable-find "lein"))
      (message "lein command not found.")
    (labels ((locate-project (file name)
                             ;; adapted from https://github.com/technomancy/emacs-starter-kit/blob/master/dominating-file.el
                             (let* ((file (abbreviate-file-name file))
                                    (stop-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")
                                    (root nil)
                                    (prev-file file)
                                    try)
                               (while (not (or root
                                               (null file)
                                               (string-match stop-dir-regexp file)))
                                 (setq try (file-exists-p (expand-file-name name file)))
                                 (cond (try (setq root file))
                                       ((equal file (setq prev-file file
                                                          file (file-name-directory
                                                                (directory-file-name file))))
                                        (setq file nil))))
                               root)))
      (let ((project-dir (locate-project buffer-file-name "project.clj")))
        (if (file-exists-p (concat project-dir "lib"))
            (inferior-lisp "lein repl")
          (message "lib directory not found.  Have you run lein deps?"))))))
