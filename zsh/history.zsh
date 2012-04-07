export HISTFILE=~/.history
export HISTSIZE=10500
export SAVEHIST=10000
export SHARE_HISTORY=1
export EXTENDED_HISTORY=1
export HIST_EXPIRE_DUPS_FIRST=1

setopt \
    appendhistory \
    autocd \
    extendedglob \
    prompt_subst \
    auto_pushd \
    pushd_silent \
    correct
