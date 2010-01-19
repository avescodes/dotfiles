# File:     ~/.emacs.d/zsh/prompts.zsh
# Author:   Ryan Neufeld <neufelry@gmail.com>
# Modified: <2009-05-22 21:19:27 CDT>

# This file is loaded by zshrc.zsh

setopt ALL_EXPORT
setopt prompt_subst

# ZSH Git setup - thanks http://www.jukie.net/~bart/blog/zsh-git-branch2
typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions
export __CURRENT_GIT_BRANCH=
export __CURRENT_GIT_VARS_INVALID=1

zsh_git_invalidate_vars() {
  export __CURRENT_GIT_VARS_INVALID=1
}
zsh_git_compute_vars() {
  export __CURRENT_GIT_BRANCH="$(git-prompt)"
  export __CURRENT_GIT_VARS_INVALID=
}

chpwd_functions+='zsh_git_chpwd_update_vars'
zsh_git_chpwd_update_vars() {
  zsh_git_invalidate_vars
}

preexec_functions+='zsh_git_preexec_update_vars'
zsh_git_preexec_update_vars() {
  case "$(history $HISTCMD)" in 
    *git*) zsh_git_invalidate_vars ;;
  esac
 }


get_git_prompt_info() {
  test -n "$__CURRENT_GIT_VARS_INVALID" && zsh_git_compute_vars
  echo $__CURRENT_GIT_BRANCH
}




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
#PROMPT="$PR_RED%2c $(get_git_prompt_info)$PR_YELLOW% ▸ $PR_NO_COLOR"
export PROMPT='$PR_RED%2c $(get_git_prompt_info)$PR_YELLOW▸$PR_NO_COLOR '
RPS1="$PR_RED%n$PR_NO_COLOR@$PR_LIGHT_RED%U%m%u $PR_YELLOW(%T)$PR_NO_COLOR"
#(`git-prompt`)

# If we're on a remote host, prefix PS1 with the first letter of the hostname.
#if [[ $SSH_CLIENT != "" ]]; then
  HNC="$(hostname | tr '[a-z]' '[A-Z]') "
#fi

# If we're using a dumb terminal (ie. emacs), assume we don't want colour.
if [[ $TERM == "dumb" ]]; then
      PS1="%~ %# "
      RPS1=""
fi
# Compatibility with TRAMP
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ ' && RPS1=''

# emacs ansi-term sends "eterm-color".
if [[ $TERM == "eterm-color" ]]; then
  export PS1="%{[01;34m%}%C $COLOR%#%{[0m%} "
  setopt singlelinezle
fi

unsetopt ALL_EXPORT
