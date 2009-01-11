;;; inf-jess.el --- Inferior Jess mode.

;; Copyright (C) 1999 by David E. Young.

;; Author: David E. Young <david.young@fnc.fujitsu.com>
;; Keywords: languages, jess

;; This is version 1.0 of 5 January 2000.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307  USA

;;; Commentary:

;; An inferior mode for running a Jess interpreter within Emacs.

;; The mechanism used to invoke Jess can be customized by setting
;; various hooks and variables. In fact, you *must* set the variable
;; `inferior-jess-program' to something meaningful. For example, from
;; within your '.emacs' you might have something like:
;;
;;  (add-hook 'inferior-jess-load-hook
;;            #'(lambda ()
;;                (setq inferior-jess-program
;;                      #'(lambda ()
;;                          '("-classpath" "/usr/local/jess/jess.jar"
;;                            "jess.Main")))))
;;
;; inf-jess will evaluate `inferior-jess-program' to create the Java
;; VM's argument list, then run the interpreter using the VM specified
;; by `inferior-jess-vm'. Alternatively, if you typically invoke Jess
;; via a shell script do something like:
;;
;;   (add-hook 'inferior-jess-load-hook
;;             #'(lambda ()
;;                 (setq inferior-jess-program
;;                       "/usr/local/bin/jess")))
;;
;; In this case, inf-jess will ignore `inferior-jess-vm' and use the
;; supplied shell script to run the interpreter.

;; See the documentation on `inf-jess-mode' for additional
;; information.

;; Please send enhancement requests and bug reports directly to me
;; <david.young@fnc.fujitsu.com>. If you make a change yourself, or
;; adapt this program for use with another language (CLIPS perhaps)
;; I'd appreciate hearing about it.

;; To do:
;;   * Consider defining user-customizable variables with 'defcustom'.
;;   * Consider eliminating the variable `inferior-jess-vm'; get the
;;     name of the Java executable by evaluating (first inferior-jess-program)
;;     instead.

(require 'comint)
(require 'jess-mode)

(defvar inferior-jess-mode-map nil)

(defvar inferior-jess-mode-hook nil
  "*Hooks for customising Jess mode.")

(defvar inferior-jess-load-hook nil
  "*Hooks run after this module is loaded.")

(defvar inferior-jess-buffer nil
  "The current inferior Jess process buffer.")

(defvar inferior-jess-program nil
  "*Defines a program name or function used to construct an inferior
Jess process.
If this variable evaluates to a string, it is interpreted as a
'self-contained' executable (eg. shell script) that requires no
arguments. If this variable's value is a function, it should evaluate
to a list of arguments which are handed to the Java virtual machine as
defined by `inferior-jess-vm'.")

(defvar inferior-jess-vm "java"
  "*Defines the virtual machine used to run an inferior Jess process.")

(defvar source-modes '(inferior-jess-mode)
  "*Used to determine whether or not a buffer contains Jess source code.")

(defvar previous-dir/file nil
  "Records the last directory and file used in loading. Holds a dotted
pair of the form `(DIRECTORY . FILE)' describing the last
`load-file' command.")

(when (not inferior-jess-mode-map)
  (setq inferior-jess-mode-map
    (copy-keymap comint-mode-map)))

(define-key inferior-jess-mode-map "\C-ci" 'inf-jess-reset-engine) ; initialize engine
(define-key inferior-jess-mode-map "\C-cl" 'inf-jess-load-file)
(define-key inferior-jess-mode-map "\C-cg" 'inf-jess-run-engine) ; "go"...
(define-key inferior-jess-mode-map "\C-cf" 'inf-jess-get-facts)
(define-key inferior-jess-mode-map "\C-cr" 'inf-jess-get-rules)

;; These keys augment 'jess-mode-map' with behavior specific to an
;; inferior Jess process...

(when (not (null jess-mode-map))
  (define-key jess-mode-map "\C-x\C-e" 'inf-jess-eval-last-sexp)  ; GNU convention
  (define-key jess-mode-map "\M-\C-x" 'inf-jess-eval-deffunction) ; GNU convention
  (define-key jess-mode-map "\C-ca" 'inf-jess-get-agenda)
  (define-key jess-mode-map "\C-cb" 'inf-jess-eval-buffer)
  (define-key jess-mode-map "\C-ce" 'inf-jess-eval-region)
  (define-key jess-mode-map "\C-cf" 'inf-jess-get-facts)
  (define-key jess-mode-map "\C-cg" 'inf-jess-run-engine)
  (define-key jess-mode-map "\C-ci" 'inf-jess-reset-engine)
  (define-key jess-mode-map "\C-cr" 'inf-jess-get-rules)
  (define-key jess-mode-map "\C-ct" 'inf-jess-eval-deftemplate)
  (let ((map (lookup-key jess-mode-map [menu-bar jess]))
        (rete (make-sparse-keymap)))
    (define-key map [separator-rete] '("--"))
    (define-key map [rete] (cons "Rete" rete))
    (define-key rete [agenda] '("Agenda" . inf-jess-get-agenda))
    (define-key rete [facts] '("Facts" . inf-jess-get-facts))
    (define-key rete [rules] '("Rules" . inf-jess-get-rules))
    (define-key rete [reset] '("Reset" . inf-jess-reset-engine))
    (define-key map [separator-eval] '("--"))
    (define-key map [eval-buffer] '("Evaluate Buffer" . inf-jess-eval-buffer))
    (define-key map [eval-region] '("Evaluate Region" . inf-jess-eval-region))
    (define-key map [eval-sexp] '("Evaluate Last S-expression" . inf-jess-eval-last-sexp))))

(defun inferior-jess-mode ()
  "Major mode for interacting with an inferior Jess process.
Runs a Jess interpreter as a subprocess of Emacs, with Jess I/O
through an Emacs buffer.  Variable `inferior-jess-program'
controls how the Jess interpreter is run.

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-jess-buffer'.

\\{inferior-jess-mode-map}

Customisation: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-jess-mode-hook' (in that order).

You can send text to the inferior Jess process from other buffers containing
Jess source.  
    switch-to-inferior-jess switches the current buffer to the Jess process buffer.
    jess-eval-region sends the current region to the Jess process.

    Prefixing the jess-eval-region command with a
    \\[universal-argument] causes a switch to the Jess process buffer
    after sending the text.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Jess; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq major-mode 'inferior-jess-mode)
  (setq mode-name "Inferior Jess")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-jess-mode-map)
  (setq comint-input-sentinel 'ignore)
  (run-hooks 'inferior-jess-mode-hook))

(defun run-jess (&optional image)
  "Run an inferior Jess process, with input and output via buffer
`*jess*'. If there is a process already running in `*jess*', just
switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-jess-program').  Runs the hooks from
`inferior-jess-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list
                (and current-prefix-arg
                     (read-string "Run Jess like this: "))))
  (when (not (comint-check-proc "*jess*"))
    (let* ((image
            (or image inferior-jess-program))
           (buffer
            (cond ((stringp image)
                   (make-comint "jess" image))
                  ((functionp image)
                   (apply 'make-comint
                          "jess" inferior-jess-vm nil
                          (funcall image)))
                  (t
                   (error "Variable `inferior-jess-program' must be either stringp or functionp")))))
      (set-buffer buffer)
      (inferior-jess-mode)))
  (setq inferior-jess-buffer "*jess*")
  (switch-to-buffer inferior-jess-buffer))

(defun test-run-jess ()
  (interactive)
  (setq inferior-jess-program
        #'(lambda ()
            '("-classpath"
              "/files/devel/jem/classes/jess.jar"
              "jess.Main")))
  (run-jess))
                      
(defun inf-jess-send-request(req)
  (let ((proc (inferior-jess-process)))
    (comint-send-string proc (concat req "\n"))))

(defun inf-jess-load-file (fname)
  "Load a Jess source file into the inferior Jess process."
  (interactive
   (comint-get-source "Load Jess file: "
                      previous-dir/file
                      source-modes t))
  (comint-check-source fname)
  (setq previous-dir/file
    (cons (file-name-directory fname)
          (file-name-nondirectory fname)))
  (inf-jess-send-request (format "(batch %s)" fname))
  (switch-to-inferior-jess t))

(defun inf-jess-get-facts ()
  "Retrieve the fact list from the inferior Jess process."
  (interactive)
  (inf-jess-send-request "(facts)"))

(defun inf-jess-get-rules ()
  "Retrieve the rule list from the inferior Jess process."
  (interactive)
  (inf-jess-send-request "(rules)"))

(defun inf-jess-get-agenda ()
  "Retrieve the agenda from the inferior Jess process."
  (interactive)
  (inf-jess-send-request "(agenda)"))

(defun inf-jess-reset-engine ()
  "Reset the inference engine running in the inferior Jess process."
  (interactive)
  (inf-jess-send-request "(reset)"))

(defun inf-jess-run-engine ()
  "Run the inference engine in the inferior Jess process."
  (interactive)
  (inf-jess-send-request "(run)"))

(defun inferior-jess-process ()
  (let ((proc (get-buffer-process
               (if (eq major-mode 'inferior-jess-mode)
                   (current-buffer)
                 inferior-jess-buffer))))
    (or proc
        (error "No Jess sub-process; see variable `inferior-jess-buffer'"))))

(defun switch-to-inferior-jess (eob-p)
  "Switch to the inferior Jess process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-jess-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inferior-jess-buffer t))))
        (pop-to-buffer inferior-jess-buffer))
    (run-jess inferior-jess-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inf-jess-eval-region (start end &optional and-go)
  "Send the current region to the inferior Jess process.
Prefix argument forces switch to Jess buffer afterwards."
  (interactive "r\nP")
  (let ((proc (inferior-jess-process)))
    (comint-send-region proc start end)
    (comint-send-string proc "\n")
    (if and-go
        (switch-to-inferior-jess t))))

(defun inf-jess-eval-buffer (&optional and-go)
  "Send the entire buffer to the Inferior Jess process.
Prefix argument means switch to the Jess buffer afterwards."
  (interactive "P")
  (inf-jess-eval-region (point-min) (point-max) and-go))

(defun inf-jess-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Jess process.
Prefix argument means switch to the Jess buffer afterwards."
  (interactive "P")
  (inf-jess-eval-region
   (save-excursion
     (backward-sexp) (point))
   (point) and-go))

(defun inf-jess-eval-form (&optional and-go)
  "Send the current form to the inferior Jess process.
Prefix argument means switch to the Jess buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((end (point)))
      (beginning-of-defun)
      (inf-jess-eval-region (point) end)))
  (if and-go
      (switch-to-inferior-jess t)))

(defun inf-jess-eval-deffunction (&optional and-go)
  "Send the current deffunction to the inferior Jess process.
Prefix argument means switch to the Jess buffer afterwards."
  (interactive "P")
  (inf-jess-eval-form and-go))

(defun inf-jess-eval-defrule (&optional and-go)
  "Send the current defrule to the inferior Jess process.
Prefix argument means switch to the Jess buffer afterwards."
  (interactive "P")
  (inf-jess-eval-form and-go))

(defun inf-jess-eval-deftemplate (&optional and-go)
  "Send the current deftemplate to the inferior Jess process.
Prefix argument means switch to the Jess buffer afterwards."
  (interactive "P")
  (inf-jess-eval-form and-go))

(run-hooks 'inferior-jess-load-hook)

(provide 'inf-jess)

;;; inf-jess.el ends here
