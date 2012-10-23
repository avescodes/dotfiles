function git-user-initials {
  initials="`git config --get user.initials`"
  if [[ $initials =~ ".+" ]]; then
    echo "($initials) "
  else
    echo ""
  fi
}

function build-prompt() {
    case "$?" in 
        0) statcolor="%{$fg[green]%}" ;;
        *) statcolor="%{$fg[red]%}"   ;;
    esac 
    git=$(git-prompt)
    pair=$(git-user-initials)

    echo "%{$fg[cyan]%}%2c $git $pair$statcolor▸%{$reset_color%} "
}

function build-right-prompt() {
  if [ -n $TMUX ]; then
    echo ""
  else
    echo "%{$fg[cyan]%}%n %{$reset_color%}@ %{$fg_bold[green]%}%U%m%u %{$reset_color%}(%T)"
  fi
}
PS1='`build-prompt`'

RPS1='`build-right-prompt`'

[ $TERM = "eterm-color" ] && setopt singlelinezle
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ ' && RPS1=''
