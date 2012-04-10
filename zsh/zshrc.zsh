setopt ALL_EXPORT

autoload -Uz colors && colors
bindkey -e # emacs style key bindings
local WORDCHARS=${WORDCHARS//\//}
setopt CORRECT

EDITOR=vim
BROWSER="w3m"
PAGER="less"
SHELL="/bin/zsh"

PATH=$HOME/bin:/usr/local/bin:$PATH:/usr/X11/bin
MANPATH=/usr/local/share/man:$MANPATH
CLASSPATH=.:$CLASSPATH

unsetopt ALL_EXPORT

# Load nested configs
for f in $(find ~/.config/zsh -name \*.zsh | grep -v zshrc.zsh); do
  . $f
done
