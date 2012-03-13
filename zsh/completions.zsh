# File:     ~/.emacs.d/zsh/completions.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-13 00:01:27 CST>

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Include known hosts, requires "HashKnownHosts no" in .ssh/config -http://www.sourceguru.net/ssh-host-completion-zsh-stylee/ 
zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'
autoload -U compinit
compinit


