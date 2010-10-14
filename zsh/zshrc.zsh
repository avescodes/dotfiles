# File:     ~/.zshrc
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2009-01-29 15:47:03 CST>

export NAME="Ryan Neufeld"

if [ -f /sw/bin/init.sh ]; then # OS X
  . /sw/bin/init.sh
fi

if [ -f /usr/share/gentoo/mc/mc.gentoo ]; then # gentoo linux
  . /usr/share/mc/mc.gentoo
fi

export GIT_AUTHOR_NAME=$NAME
export GIT_COMMITTER_NAME=$NAME
export RUBYOPT=""
export EDITOR="pico"
export BROWSER="w3m"
export PAGER="less"
export SHELL="/bin/zsh"
export VIM=~/.vim
setopt CORRECT

bindkey -e

autoload -Uz colors
colors

. ~/.config/zsh/path.zsh
. ~/.config/zsh/history.zsh

. ~/.config/zsh/git.zsh
. ~/.config/zsh/prompts.zsh
. ~/.config/zsh/titles.zsh
. ~/.config/zsh/completions.zsh
. ~/.config/zsh/functions.zsh
. ~/.config/zsh/aliases.zsh
# j
#. ~/.config/zsh/rake_completions.zsh

function og {
  scp -r $1 og:~/h
  echo "http://hammerofcode.com/$1" | pbcopy
}

function project_precmd() {
  if [ -z $1 ]; then
    export PROJECT_ROOT=$(cd $(project_precmd .); pwd -P)
  else
    if [[ -d $1/.git || -f $1/Rakefile || -f $1/Makefile ]]; then
      echo $1
    else 
      if [[ $(cd $1; pwd -P) == / ]]; then
        echo .
      else 
        echo $(project_precmd $1/..)
      fi
    fi
  fi
}

precmd_functions+=(project_precmd)

local WORDCHARS=${WORDCHARS//\//}

if [[ -s $HOME/.rvm/scripts/rvm ]] ; then source $HOME/.rvm/scripts/rvm ; fi
