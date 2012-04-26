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
  alias ls="ls -GF" # for OS X
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

# TrueCrypt
function decrypt {
  /Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt -t -k "" --protect-hidden=no ~/relevance.tc ~/relevance
}
alias tc="decrypt"
