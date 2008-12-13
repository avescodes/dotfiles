# File:     ~/.emacs.d/zsh/completions.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-13 00:01:27 CST>

# This file is loaded by zshrc.zsh

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
autoload -U compinit
compinit

# Files to ignore during completion
fignore=(DS_Store $fignore)

# cache completions
zstyle ':completion::complete:*' use-cache 1
