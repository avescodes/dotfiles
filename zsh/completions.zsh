# File:     ~/.emacs.d/zsh/completions.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-13 00:01:27 CST>

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

autoload -U compinit
compinit
