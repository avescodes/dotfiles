# File:     ~/.emacs.d/zsh/path.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-14 12:26:16 CST>

# This file is loaded by zshrc.zsh

setopt ALL_EXPORT

MANPATH=/opt/local/share/man:$MANPATH
PATH="/usr/local/mysql/bin:/Users/jaffer/bin/:$PATH:$HOME/opt/jruby/bin"
PATH="$HOME/bin:/opt/local/bin:/opt/local/sbin:$PATH"
PATH="$HOME/.emacs.d/clojure:$PATH"
CLJ_DIR="$HOME/.emacs.d/clojure"

CLASSPATH=.:$CLASSPATH
unsetopt ALL_EXPORT
