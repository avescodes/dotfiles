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

# function precmd()  { 
#     title "zsh" "$USER@%m" "%35<...<%~" 
#     j --add "$(pwd -P)"
# }

# preexec is called just before any command line is executed
function preexec() { title "$1"  "$USER@%m" "%55<...<%~" }
