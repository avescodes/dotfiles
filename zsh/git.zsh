if [[ -x `which git` ]]; then
	function git-branch-name () {
		git branch 2> /dev/null | grep '^\*' | sed 's/^\*\ //'
	}
	function git-dirty () {
		git status 2> /dev/null | grep "nothing to commit (working directory clean)"
		echo $?
	}

  function git-remote-is-dotfiles () {
      git config --get remote.origin.url 2> /dev/null | grep 'dotfiles.git'
      echo $?
  }

	function git-prompt() {
		  branch=$(git-branch-name)
		  dirty=$(git-dirty)
      dotfiles=$(git-remote-is-dotfiles)
			dirty_color=$fg[green]
			if [[ $dirty == 1 ]]       { dirty_color=$fg[magenta] }
      if [[ $branch == master ]] { branch=✪ }
      if [[ $branch == wookie ]] { branch=♨ }
      if [[ $branch == lazer ]]  { branch=⚡ }
			if [[ $dotfiles != 1 ]]    { branch=✖ }
      if [[ x$branch == x ]]     { branch=◻; dirty_color=$fg[white] }
      echo "%{$dirty_color%}$branch%{$reset_color%} "
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
	function ghg () {
		  open $(github-url)
	}
	
	function nhgk () {
		  nohup gitk --all &
	}
fi
