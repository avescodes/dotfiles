# File:     ~/.zshrc
# Author:   Burke Libbey <burke@burkelibbey.org>
# Modified: <2008-11-30 09:58:03 CST>

export NAME="Burke Libbey"
export EMAIL="burke@burkelibbey.org"

# {{{ Environment ###########################################################

if [ -f /sw/bin/init.sh ]; then # OS X
  . /sw/bin/init.sh
fi

if [ -f /usr/share/gentoo/mc/mc.gentoo ]; then # gentoo linux
  . /usr/share/mc/mc.gentoo
fi

export IXP_ADDRESS=unix!/tmp/ns.$USER.127.0.0.1_0/wmii
export GIT_AUTHOR_NAME=$NAME
export GIT_COMMITTER_NAME=$NAME
export GIT_AUTHOR_EMAIL=$EMAIL
export GIT_COMMITTER_EMAIL=$EMAIL
export RUBYOPT=""
export EDITOR="emacs"
export BROWSER="w3m"
export PAGER="less"

export PATH="~/opt/clojure-extra/sh-script:/opt/local/bin:/usr/bin:$PATH:$HOME/bin:/usr/local/bin"

setopt CORRECT

# }}}

# {{{ Miscellaneous #########################################################

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000
setopt appendhistory autocd extendedglob

# Emacs editing
bindkey -e

# Compatibility with TRAMP
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

# }}}

# {{{ Prompts ###############################################################

# Colorize red for root, green for normal users. It's Gentoolicious!
if [[ $USER == "root" ]]; then
  COLOR="%{[0m[01;31m%}"
else
  COLOR="%{[0m[01;32m%}"
fi

# prompt (if running screen, show window #)
if [[ $WINDOW != "" ]]; then
    export PS1="$COLOR$WINDOW:%{[01;34m%}%~ $COLOR%#%{[0m%} "
else
    export PS1="%{[01;34m%}%~ $COLOR%#%{[0m%} "
fi


RPS1="%n@%{[0m[01;31m%}%m%{[0m%} (%T)"


# If we're using a dumb terminal (ie. emacs), assume we don't want colour.
if [[ $TERM == "dumb" ]]; then
    export PS1="%~ %# "
fi

# }}}

# {{{ Titles ################################################################

function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}

  # Truncate command, and join lines.
  a=$(print -Pn "%40>...>$a" | tr -d "\n")

  s="%39>...>$a:$3"

  case $TERM in
  screen)
    print -Pn "\ek$s\e\\"      # screen title (in ^H")
    ;;
  xterm*|rxvt)
    print -Pn "\e]2;$2 | $a:$3\a" # plain xterm title
    ;;
  esac
}

# precmd is called just before the prompt is printed
function precmd()  { title "zsh" "$USER@%m" "%35<...<%~" }

# preexec is called just before any command line is executed
function preexec() { title "$1"  "$USER@%m" "%55<...<%~" }

# }}}

# {{{ Completion ############################################################

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

autoload -U compinit
compinit

# cache completions
zstyle ':completion::complete:*' use-cache 1

# }}}

# {{{ Functions #############################################################

mdc()    { mkdir -p "$1" && cd "$1" }
setenv() { export $1=$2 }
sdate()  { date +%Y.%m.%d }
pc()     { awk "{print \$$1}" }

# }}}

# {{{ Aliases ###############################################################

alias gitc="git add .;git commit -a -m"
alias gitco="git checkout"
alias gitl="git log"
alias gitd="git diff"
alias gitbl="git branch -l"
alias gitbd="git branch -d"
alias gitpl="git pull"
alias gitpu="git push"
alias gitm="git merge"
alias hamachi='sudo hamachi -c /etc/hamachi'
alias sx="startx"
alias slime='emacs -e slime'
alias conkeror='/opt/conkeror/conkeror'
alias rdesktop='rdesktop -g 1680x1024'
#alias sx='xinit /home/burke/.xinitrc'
alias sl='ls'
alias psql='psql -U postgres'
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias j='jobs'
alias l='ls'
if ls -F --color=auto >&/dev/null; then
  alias ls="ls --color=auto -F"
else
  alias ls="ls -GF"
fi
alias cl="clear;ls"
alias ll="ls -l"
alias l.='ls -d .[^.]*'
alias lsd='ls -ld *(-/DN)'
alias md='mkdir -p'
alias rd='rmdir'
alias cd..='cd ..'
alias ..='cd ..'
alias po='popd'
alias pu='pushd'
alias tsl="tail -f /var/log/syslog"
alias df="df -hT"
alias em="emacs -nw"
alias sc="screen"
alias scr="screen -r"
alias su="su -s /bin/zsh"

# }}}



