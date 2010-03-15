function linux() { [[ `uname -s` = "Linux"  ]] }
function mac()   { [[ `uname -s` = "Darwin" ]] }

alias r="ruby"

alias e="vim"

alias gsd="git stash; git pull; git stash apply"
alias gcd="git clean -d"
alias grh="git reset HEAD"
alias gac="git commit -a -m"
alias gbl="git branch -l"
alias gc="git commit"
alias gcm="git commit -m"
alias gf="git diff"
alias gl="git log"
alias gt="git status"
alias gn="git clone"
alias gco="git checkout"
alias gs="git stash"
alias gsa="git stash apply"
alias gsp="git stash pop"
alias gx="open -a gitx ."
alias gb="git branch"
alias gd="git pull"
alias gu="git push"
alias gwc="git whatchanged"
alias gap="git add -p"
alias gri="git rebase -i"
alias gsl="git stash list"
alias gcob="git checkout -b"
alias g="git merge"

alias git="hub"

alias rs="rake spec"
alias rdm="rake db:migrate"
alias rdmr="rake db:migrate:redo"

alias bi="bundle install"
alias bp="bundle pack"

find-rakefile () {
  if [ -f $1/Rakefile ]; then
    echo $1
  else 
    echo $(find-rakefile $1/..)
  fi
}

rails-script () {
  rfpath=$(find-rakefile .)
  target=$1; shift
  if [ -f $rfpath/script/rails ]; then 
    $rfpath/script/rails $target $argv
  else
    $rfpath/script/$target $argv
  fi
}

sc () { rails-script "console"  $argv }
ss () { rails-script "server"   $argv }
sg () { rails-script "generate" $argv }
sr () { rails-script "runner"   $argv }
sgm() { rails-script "generate" "migration" $argv }
sgs() { rails-script "generate" "scaffold"  $argv }
sgr() { rails-script "generate" "resource"  $argv }

alias cd..='cd ..'
alias ..='cd ..'
alias u='cd ..'
alias uu='cd ../..'
alias uuu='cd ../../..'
alias uuuu='cd ../../../..'

alias tarx="tar xf"
alias tarc="tar czf"

alias chx="chmod +x"
alias cmmi="./configure && make && sudo make install"
alias slime='emacs -e slime'

alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'

alias g='grep'
alias t='tail'
alias h='head'
alias L='less'

alias fdg="find . | grep"

alias l='ls'
alias sl='ls'
if mac; then
  alias ls="ls -GF" # for OS X
else
  alias ls="ls --color=auto -F"
fi
alias cl="clear;ls"
alias ll="ls -l"
alias l.='ls -d .[^.]*'
alias lsd='ls -ld *(-/DN)'

alias b="popd"

alias md='mkdir -p'
alias rd='rmdir'
alias df="df -hT"
alias scs="screen"
alias scr="screen -r"
alias su="su -s /bin/zsh"

if linux; then
  alias sx="startx"
  alias tsl="tail -f /var/log/syslog"
fi

alias nzb="hellanzb.py"
