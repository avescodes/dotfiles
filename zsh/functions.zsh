# File:     ~/.emacs.d/zsh/functions.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-13 00:09:41 CST>

mdc()    { mkdir -p "$1" && cd "$1" }
setenv() { export $1=$2 }
sdate()  { date +%Y.%m.%d }
pc()     { awk "{print \$$1}" }
