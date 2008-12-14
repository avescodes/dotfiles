# File:     ~/.emacs.d/zsh/functions.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-13 00:09:41 CST>

# This file is loaded by zshrc.zsh

set_cp() { export CLASSPATH

mdc()    { mkdir -p "$1" && cd "$1" }
setenv() { export $1=$2 }
sdate()  { date +%Y.%m.%d }
pc()     { awk "{print \$$1}" }

# Open manpage with Preview.app
# Uses ps2pdf conversion because it's faster
if [[ $OSTYPE[1,6] == "darwin" ]]; then
  function manp () {
    man -t $* | ps2pdf - - | open -f -a Preview
  }
fi

function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}

  # Truncate command, and join lines.
  a=$(print -Pn "%40>...>$a" | tr -d "\n")

  s="%39>...>$a:$3"

  case $TERM in
  screen)
    print -Pn "\ek$s\e\\"      # screen title (in ^H")
    ;;
  xterm*|rxvt)
    print -Pn "\e]2;$2 | $a:$3\a" # plain xterm title
    ;;
  esac
}

# precmd is called just before the prompt is printed
function precmd()  { title "zsh" "$USER@%m" "%35<...<%~" }

# preexec is called just before any command line is executed
function preexec() { title "$1"  "$USER@%m" "%55<...<%~" }
