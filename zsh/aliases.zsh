# File:     ~/.emacs.d/zsh/aliases.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-15 08:44:47 CST>

# This file is loaded by zshrc.zsh

alias b="popd"

alias gitc="git add .;git commit -a -m"
alias gitco="git checkout"
alias gitl="git log"
alias gitd="git diff"
alias gitbl="git branch -l"
alias gitbd="git branch -d"
alias gitpl="git pull"
alias gitpu="git push"
alias gitm="git merge"

alias slime='emacs -e slime'
alias sl='ls'
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias j='jobs'
alias l='ls'
if ls -F --color=auto >&/dev/null; then
  alias ls="ls --color=auto -F"
else
  alias ls="ls -GF"
fi
alias cl="clear;ls"
alias ll="ls -l"
alias l.='ls -d .[^.]*'
alias lsd='ls -ld *(-/DN)'
alias md='mkdir -p'
alias rd='rmdir'
alias cd..='cd ..'
alias ..='cd ..'
alias po='popd'
alias pu='pushd'
alias tsl="tail -f /var/log/system.log"
alias df="df -hT"
alias em="emacs -nw"
alias sc="screen"
alias scr="screen -r"
alias su="su -s /bin/zsh"

alias m="mate"
alias e="emacs"
alias status="git status"
alias pull="git pull"
alias rb="ruby"
alias rd="ruby -d"

alias ssh-atlantis='ssh quick.thruhere.net -L 5900:localhost:5900'

if [[ ! ( -x `which seq` ) && ( -x `which gseq` ) ]]; then
  alias seq=`which gseq`
fi

