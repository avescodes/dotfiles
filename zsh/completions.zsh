# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Include known hosts, requires "HashKnownHosts no" in .ssh/config -http://www.sourceguru.net/ssh-host-completion-zsh-stylee/ 
zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'
autoload -U compinit
compinit
