# File:     ~/.emacs.d/zsh/path.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2009-02-22 18:41:59 CST>

# This file is loaded by zshrc.zsh

setopt ALL_EXPORT

MANPATH=/opt/local/share/man:$MANPATH

# DB bins
PATH=/opt/local/lib/postgresql83/bin/:/usr/local/mysql/bin:$PATH 

# User Dir bins
PATH=$HOME/bin:$HOME/bin/checker:$HOME/opt/scala/bin:$PATH
PATH="$HOME/.emacs.d/clojure:$HOME/.emacs.d/site-lisp/ri:$PATH"

# Port, then homebrew
PATH="/opt/local/bin:/opt/local/sbin:$PATH"
PATH="/usr/local/bin:$PATH"

CLJ_DIR="$HOME/.emacs.d/clojure"

CLASSPATH=.:$CLASSPATH
unsetopt ALL_EXPORT
