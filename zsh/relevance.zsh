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
source ~/Development/work/relevance/etc/bash/rails.sh
source ~/Development/work/relevance/etc/bash/ruby.sh
# Do not want.
# source ~/Development/work/relevance/etc/bash/ssh_autocompletion.sh
# Don't need yet?
# source ~/Development/work/Relevance/etc/bash/project_aliases.sh
# Have via zsh source
# /Users/jared/Dev/etc/git-completion.bash
emulate zsh
