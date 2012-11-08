autoload -Uz colors && colors
bindkey -e # emacs style key bindings
local WORDCHARS=${WORDCHARS//\//}
setopt CORRECT

setopt ALL_EXPORT

EDITOR=vim
PAGER="less"
SHELL="/bin/zsh"

PATH=.bundle/binstubs:/usr/local/bin:$HOME/bin:$PATH:/usr/X11/bin:/Applications/Xcode.app/Contents/Developer/usr/bin
MANPATH=/usr/local/share/man:$MANPATH
CLASSPATH=.:$CLASSPATH

GREP_OPTIONS='--color=auto'
GREP_COLOR='3;33'

cdpath=(. ~ ~/relevance ~/code)

unsetopt ALL_EXPORT

# Load nested configs
for f in $(find ~/.config/zsh -name \*.zsh | grep -v zshrc.zsh); do
  . $f
done
