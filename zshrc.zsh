# File:     ~/.zshrc
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2008-12-12 10:05:05 CST>


# TOC:
#  - Environment
#  - Misc.
#  - History
#  - Titles
#  - Completion
#  - Functions
#  - Aliases
#  - Git functions
#  - Prompts

export NAME="Ryan Neufeld"
export EMAIL="neufelry@gmail.com"

#############################################################################
# Environment ###############################################################
#############################################################################

if [ -f /sw/bin/init.sh ]; then # OS X
  . /sw/bin/init.sh
fi

if [ -f /usr/share/gentoo/mc/mc.gentoo ]; then # gentoo linux
  . /usr/share/mc/mc.gentoo
fi


export GIT_AUTHOR_NAME=$NAME
export GIT_COMMITTER_NAME=$NAME
export GIT_AUTHOR_EMAIL=$EMAIL
export GIT_COMMITTER_EMAIL=$EMAIL
export RUBYOPT=""
export EDITOR="emacs"
export BROWSER="w3m"
export PAGER="less"
export SHELL="/opt/local/bin/zsh"
setopt CORRECT

#############################################################################
# Miscellaneous #############################################################
#############################################################################

# Emacs editing
bindkey -e

#############################################################################
# History ###################################################################
#############################################################################

setopt ALL_EXPORT

HISTFILE=~/.history
HISTSIZE=10500
SAVEHIST=10000
SHARE_HISTORY=1
EXTENDED_HISTORY=1
HIST_EXPIRE_DUPS_FIRST=1
 
# Grep the history with 'h'
h () { history 0 | grep $1 }

setopt appendhistory autocd extendedglob

CLJ_DIR=$HOME/.emacs.d/clojure
CLJ_CONTRIB=$CLJ_DIR/contrib/clojure-contrib.jar
JLINE=$CLJ_DIR/jline.jar
CLJ=$CLJ_DIR/clojure.jar
CLJ_SELF=$CLJ_DIR/self.clj
CLASSPATH="$CLJ_DIR:$CLJ_CONTRIB:$JLINE:$CLJ:$CLJ_SELF:.:$CLASSPATH"

# Path setting
MANPATH=/opt/local/share/man:$MANPATH
PATH="/usr/local/mysql/bin:/Users/jaffer/bin/:$PATH:$HOME/opt/jruby/bin"
PATH="$HOME/bin:/opt/local/bin:/opt/local/sbin:$PATH"
PATH="$CLJ_DIR:$PATH"


unsetopt ALL_EXPORT

#############################################################################
# Titles ####################################################################
#############################################################################

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

#############################################################################
# Completion ################################################################
#############################################################################

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

#############################################################################
# Functions #################################################################
#############################################################################

mdc()    { mkdir -p "$1" && cd "$1" }
setenv() { export $1=$2 }
sdate()  { date +%Y.%m.%d }
pc()     { awk "{print \$$1}" }

# Open manpage with Preview.app
# Uses ps2pdf conversion because it's faster
if [[ $OSTYPE[1,6] == "darwin" ]]; then
  function manp () {
    man -t $* | ps2pdf - - | open -f -a Preview
  }
fi

#############################################################################
# Aliases ###################################################################
#############################################################################

alias gitc="git add .;git commit -a -m"
alias gitco="git checkout"
alias gitl="git log"
alias gitd="git diff"
alias gitbl="git branch -l"
alias gitbd="git branch -d"
alias gitpl="git pull"
alias gitpu="git push"
alias gitm="git merge"

alias slime='emacs -e slime'
alias sl='ls'
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
alias tsl="tail -f /var/log/system.log"
alias df="df -hT"
alias em="emacs -nw"
alias sc="screen"
alias scr="screen -r"
alias su="su -s /bin/zsh"

alias m="mate"
alias e="emacs"
alias status="git status"
alias pull="git pull"
alias rb="ruby"
alias rd="ruby -d"

alias ssh-atlantis='ssh quick.thruhere.net -L 5900:localhost:5900'

if [[ ! ( -x `which seq` ) && ( -x `which gseq` ) ]]; then
  alias seq=`which gseq`
fi
# }}}

#############################################################################
# Git Functions #############################################################
#############################################################################
if [[ -x `which git` ]]; then
	function git-branch-name () {
		git branch 2> /dev/null | grep '^\*' | sed 's/^\*\ //'
	}
	function git-dirty () {
		git status 2> /dev/null | grep "nothing to commit (working directory clean)"
		echo $?
	}
	function gsrb () {
		branch=$(git-branch-name) 
		git checkout master
		git svn rebase
		git checkout "${branch}"
		git rebase master
	}
	function git-prompt() {
		gstatus=$(git status 2> /dev/null)
		branch=$(echo $gstatus | head -n 1 | sed 's/^# On branch //')
		dirty=$(echo $gstatus | sed 's/^#.*$//' | tail -n 2 | grep 'nothing to commit (working directory clean)'; echo $?)
		if [[ x$branch != x ]]; then
			dirty_color=$fg[cyan]
			if [[ $dirty = 1 ]] { dirty_color=$fg[magenta] }
			[ x$branch != x ] && echo " %{$dirty_color%}$branch%{$reset_color%} "
		fi
	}
	function git-scoreboard () {
		git log | grep Author | sort | uniq -ci | sort -r
	}
	function github-init () {
		git config branch.$(git-branch-name).remote origin
		git config branch.$(git-branch-name).merge refs/heads/$(git-branch-name)
	}
	
	function github-url () {
		git config remote.origin.url | sed -En 's/git(@|:\/\/)github.com(:|\/)(.+)\/(.+).git/https:\/\/github.com\/\3\/\4/p'
	}
	
	# Seems to be the best OS X jump-to-github alias from http://tinyurl.com/2mtncf
	function github-go () {
		open $(github-url)
	}
	
	function nhgk () {
		nohup gitk --all &
	}
fi

#############################################################################
# Prompts ###################################################################
#############################################################################
setopt ALL_EXPORT

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
(( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"
PS1="$PR_RED%2c $PR_YELLOW%(!.#.$)$PR_NO_COLOR "
RPS1="$PR_RED%n$PR_NO_COLOR@$PR_LIGHT_RED%U%m%u $PR_YELLOW(%T)$PR_NO_COLOR"
#(`git-prompt`)

# If we're on a remote host, prefix PS1 with the first letter of the hostname.
#if [[ $SSH_CLIENT != "" ]]; then
  HNC="$(hostname | tr '[a-z]' '[A-Z]') "
#fi

# If we're using a dumb terminal (ie. emacs), assume we don't want colour.
if [[ $TERM == "dumb" ]]; then
      PS1="%~ %# "
fi
# Compatibility with TRAMP
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

unsetopt ALL_EXPORT

