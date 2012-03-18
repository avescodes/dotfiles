export CDPATH=.:~/Development/work/relevance

# TrueCrypt
function decrypt {
  /Applications/TrueCrypt.app/Contents/MacOS/TrueCrypt -t -k "" --protect-hidden=no ~/relevance.tc ~/relevance
}
alias tc="decrypt"

# To get "pair"
export PATH=~/Development/work/relevance/etc/scripts:$PATH

emulate sh

source ~/Development/work/relevance/etc/bash/aliases.sh
source ~/Development/work/relevance/etc/bash/git.sh
source ~/Development/work/relevance/etc/bash/git_prompt.sh

emulate zsh
