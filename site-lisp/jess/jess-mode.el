;;; jess-mode.el --- Jess editing mode.

;; Copyright (C) 1999 by David E. Young.

;; Author: David E. Young <david.young@fnc.fujitsu.com>
;; Keywords: languages, jess

;; This is version 1.1 of 14 September 2000.

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

;; Some parts of this program were borrowed from Franz Inc's Emacs
;; Lisp Interface (ELI) to Allegro Common Lisp. The following
;; copyright fulfills my obligation in that respect:

;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;;; Commentary:

;; The major mode for editing Jess code. Very similar to CLIPS, which
;; in turn is a close relative of Lisp. Much of the code was borrowed
;; from the various Lisp-related modes developed for Emacs.

;; For interacting with a Jess interpreter see `run-jess' in
;; the `inf-jess' package.

;; Please send enhancement requests and bug reports directly to me
;; <david.young@fnc.fujitsu.com>. If you make a change yourself, or
;; adapt this program to another environment, I'd appreciate hearing
;; about it.

;; To do:
;;   * Figure out how to get 'font-lock' to recognize the "=>" and
;;     "<-" Jess symbols.
;;   * Figure out how to change the indentation rules for 'else and
;;     other similar keywords.
;;   * Add any more key bindings that make sense.
;;   * Consider defining user-customizable variables with 'defcustom'.

;; Change log:
;;  original, 0.5       --  9 August 1999
;;  patchlevel 1, 0.6   --  23 August 1999
;;    * Corrected font-lock problem.
;;    * Added write-file hook for paren-matching support.
;;  patchlevel 2, 0.7   --  26 August 1999
;;    * Fixed another font-lock problem; embedded keywords now are no
;;      longer incorrectly highlighted. Thanks to Kai Grossjohann for
;;      the solution.
;;  patchlevel 3, 0.8   -- 17 September 1999
;;    * New "Jess" menubar structure.
;;    * Minor enhancement to font-lock (added syntax entry so '-' is
;;      identified as a word constituent).
;;  patchlevel 4, 1.0   -- 5 January 2000
;;    * Font-lock code to identify the 'if' keyword. Thanks to Steven
;;      Tamm here.
;;  patchlevel 5, 1.1   -- 14 September 2000
;;    * Additional keyword highlighting corresponding to latest Jess
;;      releases, including improved handling of (import), (defclass),
;;      (deftemplate) and (deffacts).

;;; Code:

(require 'lisp-mode)

(defvar jess-mode-map nil
  "Standard keymap for jess-mode.")

(defvar jess-mode-hook nil
  "*Hooks for customising Jess mode.")

(defvar jess-mode-syntax-table nil
  "The value of which is the syntax table for Jess mode.")

(defvar jess-check-unbalanced-parentheses-when-saving t
  "*If non-nil, check for unbalanced parentheses before writing the file.
If the value is T, then ask whether or not the file should be written ``as
is'' if there are too many or few parens--answering no leaves the point at
the place of error.  If the value is 'warn, then a warning is issued and
the file is written.")

;; (when (not jess-mode-map)
;;   (let ((map (make-sparse-keymap "Jess")))
;;     (setq jess-mode-map
;;           (nconc (make-sparse-keymap) shared-lisp-mode-map))
;;     (define-key jess-mode-map [menu-bar] (make-sparse-keymap))
;;     (define-key jess-mode-map [menu-bar jess]
;;       (cons "Jess" map))
;;     (define-key map [comment-region] '("Comment Out Region" . comment-region))
;;     (define-key map [indent-region] '("Indent Region" . indent-region))
;;     (define-key map [indent-line] '("Indent Line" . lisp-indent-line))))

(if (not jess-mode-syntax-table)
    (let ((i 0))
      (setq jess-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
        (modify-syntax-entry i "_   " jess-mode-syntax-table)
        (setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
        (modify-syntax-entry i "_   " jess-mode-syntax-table)
        (setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
        (modify-syntax-entry i "_   " jess-mode-syntax-table)
        (setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
        (modify-syntax-entry i "_   " jess-mode-syntax-table)
        (setq i (1+ i)))
      (modify-syntax-entry ?  "    " jess-mode-syntax-table)
      (modify-syntax-entry ?\t "    " jess-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " jess-mode-syntax-table)
      (modify-syntax-entry ?\f ">   " jess-mode-syntax-table)
      (modify-syntax-entry ?\; "<   " jess-mode-syntax-table)
      (modify-syntax-entry ?` "'   " jess-mode-syntax-table)
      (modify-syntax-entry ?' "'   " jess-mode-syntax-table)
      (modify-syntax-entry ?, "'   " jess-mode-syntax-table)
      (modify-syntax-entry ?. "'   " jess-mode-syntax-table)
      (modify-syntax-entry ?# "'   " jess-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " jess-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " jess-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " jess-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " jess-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " jess-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " jess-mode-syntax-table)
      (modify-syntax-entry ?*   "w   " jess-mode-syntax-table)
      ;; The next syntax entry doesn't work with these forms:
      ;;  `,.foo
      ;;  #.foo
      ;; but it works better with variables with .'s in them
      (modify-syntax-entry ?. "w   " jess-mode-syntax-table)
                                        ;      (modify-syntax-entry ?- "w   " jess-mode-syntax-table)
      (modify-syntax-entry ?\| "_   " jess-mode-syntax-table)
      (modify-syntax-entry ?\[ "_   " jess-mode-syntax-table)
      (modify-syntax-entry ?\] "_   " jess-mode-syntax-table)))

(defconst jess-font-lock-keywords-1
  (eval-when-compile
    (let ((jess-types
           (regexp-opt
            '("deftemplate" "deffacts")))
          (jess-constructs
           (regexp-opt
            '("deffunction" "defrule" "defadvice" "defglobal"
              "defmethod" "definstance" "defquery")))
          (jess-identifier
           (let ((letter "a-zA-Z_$\-\300-\326\330-\366\370-\377")
                 (digit "0-9"))
             (concat "\\<\\([" letter "][" letter digit "]*\\)\\>"))))
      `(
        ("[ \t]+\\(crlf\\|nil\\|=>\\|<-\\)[ \t]?"
         (1 font-lock-keyword-face)) ; special keywords
        ("\\<\\(import\\)\\>[ \t]*\\(\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-constant-face nil t)
         ("\\=\\.\\(\\*\\|\\sw+\\)" nil nil
          (1 font-lock-constant-face nil t)))
        ("\\<\\(defclass\\)\\>[ \t]*\\(\\sw+\\)?[ \t]*\\(\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-type-face nil t)
         (3 font-lock-constant-face nil t))
        (,(concat "\\<" jess-types "\\>\\s *" jess-identifier)
         (,(+ 1 (regexp-opt-depth jess-types)) font-lock-type-face))
        (,(concat "\\<" jess-constructs "\\>\\s *" jess-identifier)
         (,(+ 1 (regexp-opt-depth jess-constructs)) font-lock-function-name-face))
        (,(concat "\\<\\(" jess-types "\\)\\>") . 'font-lock-keyword-face)
        (,(concat "\\<\\(" jess-constructs "\\)\\>") . 'font-lock-keyword-face)
        ("(\\(if\\|case\\)\\>"
         (jess-font-lock-if-case
          (goto-char (match-end 1))
          (goto-char (match-end 0))
          (1 font-lock-keyword-face nil t)
          (2 font-lock-keyword-face nil t))))))
  "Subdued expressions to highlight in Jess modes.")

(defconst jess-font-lock-keywords-2
  (append jess-font-lock-keywords-1
          (eval-when-compile
            `(
              (,(concat "("
                        (regexp-opt
                         '("slot" "multislot" "type" "default" "default-dynamic"
                           "extends" "range" "while" "progn" "progn$"
                           "not" "or" "switch" "case" "and" "reset" 
                           "assert" "test" "declare" "salience" "return" "bind"
                           "retract" "explicit" "unique" "clear"
                           "node-index-hash" "halt" "run"
                           "exists" "variables" "try" "catch"
                           "throw" "=>" "undefadvice"
                           "undefinstance" "undefrule") t)
                        "\\>") . font-lock-keyword-face)
              ("\\<\\&\\sw+\\>" . font-lock-keyword-face))))
  "Gaudy expressions to highlight in Jess modes.")

(defvar jess-font-lock-keywords jess-font-lock-keywords-2
  "Default expressions to highlight in Jess modes.")

(defun jess-font-lock-if-case (limit)
  "Utility function that determines if a matching 'then' appears after
a given 'if' or 'case' function.  This ensures that if is a keyword
only if it is matched with a given then.  This function may be
modified to color 'if' some really gaudy color to hilite the
fact that it doesn't have a matching then."
  ;; Keep it local
  (save-match-data
    ;; in case they screwed up the parens
    (condition-case nil
        ;; if there is a left-paren or a symbol/variable
        (if (looking-at "\\s-*\\((\\|\\sw*\\)")
            (progn (if (match-end 1);; if left parent
                       (goto-char (scan-sexps (point) 1))
                     (goto-char (match-end 2)));; if symbol, go to end
                   ;; if we are looking at a then...
                   (if (looking-at "\\s-*\\(then\\)\\>")
                       (goto-char (match-end 0));; go past it
                     ;; for any other case, return no match
                     nil))
          nil)
      (error nil))))

(defun jess-initialize-mode ()
  (set-syntax-table jess-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lisp-indent-region)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lisp-imenu-generic-expression)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(jess-font-lock-keywords
          nil nil (("+-*/.<>=!?$%_&~^:" . "w")) nil))
  (use-local-map jess-mode-map)
  (set-syntax-table jess-mode-syntax-table))

(put 'else 'jess-indent-function 0)

(defun jess-indent-function (ipoint state)
  (message "jess-indent-function")
  (lisp-indent-function ipoint state))

;; Courtesy of Franz Inc...

(defun jess-find-unbalanced-parenthesis ()
  "Verify that parentheses in the current Jess source buffer are balanced.
If they are not, position the point at the first syntax error found."
  (interactive)
  (let ((saved-point (point))
        (lpar (string-to-char "("))
        (rpar (string-to-char ")"))
        (comment-start-char (string-to-char comment-start))
        (cond-symbol-chars "-+:a-zA-Z.0-9"))
    (goto-char (point-min))
    (while (and (not (eobp))
                (let ((done nil)
                      (char nil))
                  (while (and (not (eobp)) (not done))
                    (skip-chars-forward "\f\n\t ")
                    (setq char (char-after (point)))
                    (cond ((eq ?\\ char)
                           (forward-char 2))
                          ((eq comment-start-char char)
                           (end-of-line))
                          ((eq ?\" char)
                           (forward-sexp 1))
                          ((eq ?# char)
                           (forward-char 1)
                           (setq char (char-after (point)))
                           (cond ((or (eq ?+ char) (eq ?- char))
                                  (skip-chars-forward cond-symbol-chars))
                                 (t
                                  (forward-sexp 1))))
                          (t (setq done t))))
                  t))
      (let ((char (char-after (point))))
        (cond ((or (eq char lpar)
                   (eq (char-after (1- (point))) ?\n))
               (condition-case ()
                   (forward-sexp 1)
                 (error (error "Missing )"))))
              ((eq char rpar)
               (error "Extra )"))
              (t
               ;; don't know what it is, but hey, try and forward over it
               (forward-sexp 1)))))
    (goto-char saved-point))
  (if (interactive-p)
      (message "All parentheses appear to be balanced."))
  t)

;; Courtesy of Franz Inc...

(defun jess-check-unbalanced-parentheses-when-saving ()
  "Installed as a local-write-file hook to check for unbalanced parens
before saving file."
  (if jess-check-unbalanced-parentheses-when-saving
      (if (eq 'warn jess-check-unbalanced-parentheses-when-saving)
          (condition-case nil
              (progn
                (jess-find-unbalanced-parenthesis)
                nil)
            (error
             (message "Warning: parens are not balanced in this buffer.")
             (ding)
             (sit-for 2)
             ;; so the file is written:
             nil))
        (condition-case nil
            (progn
              (jess-find-unbalanced-parenthesis)
              nil)
          (error
           ;; save file if user types "yes":
           (not (y-or-n-p "Parens are not balanced.  Save file anyway? ")))))))

(defun jess-mode ()
  "Major mode for editing Jess code.
Editing commands are similar to those of other Lisp-like modes.

In addition, if an inferior Jess process is running, some additional
commands will be defined for evaluating expressions and controlling
the interpreter. The status of the process will also be displayed in
the modeline of all Jess buffers.

Commands:
\\{jess-mode-map}
Entry to this mode calls the value of `jess-mode-hook' if that value
is non-nil."
  (interactive)
  (kill-all-local-variables)
  (jess-initialize-mode)
  (setq major-mode 'jess-mode)
  (setq mode-name "Jess")
  (setq local-write-file-hooks
        (cons 'jess-check-unbalanced-parentheses-when-saving
              local-write-file-hooks))
  (run-hooks 'jess-mode-hook))

(defun jess-get-version-string ()
  (interactive)
  )

(provide 'jess-mode)

;;; jess-mode.el ends here
