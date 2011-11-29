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
export EDITOR='mvim -f --nomru -c "au VimLeave * !open -a Terminal"'
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

precmd_functions+=(project_precmd)

local WORDCHARS=${WORDCHARS//\//}

 [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.

