if [ "$(command -v git)" ]; then
  function __pair-initials () {
    author_email=${${${GIT_AUTHOR_EMAIL#pair+}%@thinkrelevance.com}//+/\/}
    if [[ -n "$author_email" ]]; then
      echo " ($author_email)"
    fi
  }

  function __branch-name () {
    git branch 2> /dev/null | grep '^\*' | sed 's/^\*\ //'
  }

  function __dirty () {
    git status 2> /dev/null | grep "nothing to commit.*working directory clean"
    echo $?
  }

  function __remote-is-dotfiles () {
      git config --get remote.origin.url 2> /dev/null | grep 'dotfiles.git'
      echo $?
  }

  function git-prompt() {
    branch=$(__branch-name)
    dirty=$(__dirty)
    dotfiles=$(__remote-is-dotfiles)
    dirty_color=$fg[green]
    pair=$(__pair-initials)
    if [[ $dirty == 1 ]]       { dirty_color=$fg[magenta] }
    if [[ $branch == master ]] { branch=✪ }
    if [[ $dotfiles != 1 ]]    { branch=✖ }
    if [[ x$branch == x ]]     { branch=''; dirty_color=$fg[white] }
    echo "%{$dirty_color%}$branch%{$reset_color%}$pair"
  }

  function __scoreboard () {
    git log | grep Author | sort | uniq -ci | sort -r
  }
fi

