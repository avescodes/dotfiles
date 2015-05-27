function mac()   { [[ `uname -s` = "Darwin" ]] }

alias e="vim"

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
  alias jsc="/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc"

  alias truecrypt='/Applications/TrueCrypt.app/Contents/MacOS/Truecrypt --text'
else
  alias ls="ls --color=auto -F"
fi

alias ll="ls -l"

alias md='mkdir -p'
alias rd='rmdir'
alias su="su -s /bin/zsh"

alias find="noglob find"
alias search='find . | xargs grep -I --no-messages --colour --line-number'

alias g=hub
alias be="bundle exec"

alias gst='git st'
alias gba='git br -ra'

alias ack=ag
alias ack='noglob ack'
alias siege='noglob siege'
alias http='noglob http'
