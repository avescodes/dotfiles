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

. ~/.emacs.d/zsh/git.zsh
. ~/.emacs.d/zsh/history.zsh
. ~/.emacs.d/zsh/path.zsh
. ~/.emacs.d/zsh/completions.zsh
. ~/.emacs.d/zsh/functions.zsh
. ~/.emacs.d/zsh/prompts.zsh
. ~/.emacs.d/zsh/aliases.zsh
. ~/.emacs.d/zsh/rake_completions.zsh

# zsh-git setup
