function build-prompt() {
    case "$?" in 
        0) statcolor="%{$fg[green]%}" ;;
        *) statcolor="%{$fg[red]%}"   ;;
    esac 
    git=$(git-prompt)
    ruby=$(rvm-prompt u g)
    
    echo "%{$fg[cyan]%}%~ $git%{$fg[red]%}$ruby $statcolor▸%{$reset_color%} "
}
PS1='`build-prompt`'

RPS1="%{$fg[cyan]%}%n %{$reset_color%}@ %{$fg_bold[green]%}%U%m%u %{$reset_color%}(%T)"

[ $TERM = "eterm-color" ] && setopt singlelinezle
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ ' && RPS1=''
