autoload -Uz colors && colors
bindkey -e # emacs style key bindings
local WORDCHARS=${WORDCHARS//\//}
setopt CORRECT

setopt ALL_EXPORT

EDITOR=vim
BROWSER="w3m"
PAGER="less"
SHELL="/bin/zsh"

PATH=$HOME/bin:/usr/local/bin:$PATH:/usr/X11/bin:/Applications/Xcode.app/Contents/Developer/usr/bin/
MANPATH=/usr/local/share/man:$MANPATH
CLASSPATH=.:$CLASSPATH

GREP_OPTIONS='--color=auto'
GREP_COLOR='3;33'

unsetopt ALL_EXPORT

# Load nested configs
for f in $(find ~/.config/zsh -name \*.zsh | grep -v zshrc.zsh); do
  . $f
done
