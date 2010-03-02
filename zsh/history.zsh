# File:     ~/.emacs.d/zsh/history.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-15 08:45:31 CST>

# This file is loaded by zshrc.zsh

setopt ALL_EXPORT

HISTFILE=~/.history
HISTSIZE=10500
SAVEHIST=10000
SHARE_HISTORY=1
EXTENDED_HISTORY=1
HIST_EXPIRE_DUPS_FIRST=1

# Grep the history with 'h'
h () { history 0 | grep $1 }


setopt \
    appendhistory \
    autocd \
    extendedglob \
    prompt_subst \
    auto_pushd \
    pushd_silent \
    correct

unsetopt ALL_EXPORT
