# File:     ~/.emacs.d/zsh/path.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2009-02-22 18:41:59 CST>

# This file is loaded by zshrc.zsh

setopt ALL_EXPORT

MANPATH=/opt/local/share/man:$MANPATH
PATH="/usr/local/bin:/usr/local/mysql/bin:/Users/jaffer/bin/:$PATH"
PATH="$HOME/bin:/opt/local/bin:/opt/local/sbin:$PATH"
PATH="$HOME/.emacs.d/clojure:$HOME/.emacs.d/site-lisp/ri:$PATH"
CLJ_DIR="$HOME/.emacs.d/clojure"

CLASSPATH=.:$CLASSPATH
unsetopt ALL_EXPORT
