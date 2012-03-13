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
export EDITOR=vim
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
. ~/.config/zsh/relevance.zsh
# j
#. ~/.config/zsh/rake_completions.zsh

function rn {
  scp -r $1 prgmr:~/r
  echo "http://ryanneufeld.ca/$1" | pbcopy
}

precmd_functions+=(project_precmd)

local WORDCHARS=${WORDCHARS//\//}

# Ruby stuff
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=1000000000
export RUBY_HEAP_FREE_MIN=500000
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

