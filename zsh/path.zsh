# File:     ~/.emacs.d/zsh/path.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2009-02-22 18:41:59 CST>

# This file is loaded by zshrc.zsh

setopt ALL_EXPORT

MANPATH=/opt/local/share/man:$MANPATH

# DB bins
PATH=/usr/local/mysql/bin:$PATH 

# User Dir bins
PATH=$HOME/bin:$HOME/bin/checker:$PATH

# Port, then homebrew
PATH="/usr/local/bin:$PATH"

CLASSPATH=.:$CLASSPATH
unsetopt ALL_EXPORT
