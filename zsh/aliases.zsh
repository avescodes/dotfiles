function mac()   { [[ `uname -s` = "Darwin" ]] }

alias e="vim"
alias jsc="/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc"

alias cd..='cd ..'
alias ..='cd ..'

alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'

alias sl='ls'

if mac; then
  if command -v gls > /dev/null 2>&1; then
    alias ls='gls --color=auto -F'
  else
    alias ls="ls -GF"
  fi
else
  alias ls="ls --color=auto -F"
fi

alias ll="ls -l"

alias md='mkdir -p'
alias rd='rmdir'
alias su="su -s /bin/zsh"

function rn {
  scp -r $1 prgmr:~/r
  echo "http://ryanneufeld.ca/$1" | pbcopy
}

alias find="noglob find"

alias g=hub
alias be="bundle exec"
alias last_migration='vim `ls db/migrate/*.rb | sort -r | head -n 1`'
alias gst='git st'
alias gba='git br -ra'

