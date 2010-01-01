# File:     ~/.zshrc
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2009-01-29 15:47:03 CST>

export NAME="Ryan Neufeld"
export EMAIL="neufelry@gmail.com"

if [ -f /sw/bin/init.sh ]; then # OS X
  . /sw/bin/init.sh
fi

if [ -f /usr/share/gentoo/mc/mc.gentoo ]; then # gentoo linux
  . /usr/share/mc/mc.gentoo
fi

export GIT_AUTHOR_NAME=$NAME
export GIT_COMMITTER_NAME=$NAME
export GIT_AUTHOR_EMAIL=$EMAIL
export GIT_COMMITTER_EMAIL=$EMAIL
export RUBYOPT=""
export EDITOR="vim"
export BROWSER="w3m"
export PAGER="less"
export SHELL="/bin/zsh"
export VIM=~/.vim
setopt CORRECT

bindkey -e

. ~/.config/zsh/git.zsh
. ~/.config/zsh/history.zsh
. ~/.config/zsh/path.zsh
. ~/.config/zsh/completions.zsh
. ~/.config/zsh/functions.zsh
. ~/.config/zsh/prompts.zsh
. ~/.config/zsh/aliases.zsh
. ~/.config/zsh/rake_completions.zsh

# zsh-git setup
