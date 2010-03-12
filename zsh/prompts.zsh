function build-prompt() {
    case "$?" in 
        0) statcolor="%{$fg[green]%}" ;;
        *) statcolor="%{$fg[red]%}"   ;;
    esac 
    git=$(git-prompt)
    ruby=$(rvm-prompt)
    
    echo "%{$fg[cyan]%}%~ $git%{$fg[red]%}$ruby $statcolor▸%{$reset_color%} "
}
PS1='`build-prompt`'

[ $TERM = "eterm-color" ] && setopt singlelinezle
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
