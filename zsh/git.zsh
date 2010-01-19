if [[ -x `which git` ]]; then
	function git-branch-name () {
		git branch 2> /dev/null | grep '^\*' | sed 's/^\*\ //'
	}
	function git-dirty () {
		git status 2> /dev/null | grep "nothing to commit (working directory clean)"
		echo $?
	}
# 	function gsrb () {
# 		branch=$(git-branch-name) 
# 		git checkout master
# 		git svn rebase
# 		git checkout "${branch}"
# 		git rebase master
# 	}
	function git-prompt() {
		gstatus=$(git status 2> /dev/null)
		branch=$(echo $gstatus | head -n 1 | sed 's/^# On branch //')
		dirty=$(echo $gstatus | sed 's/^#.*$//' | tail -n 2 | grep 'nothing to commit (working directory clean)'; echo $?)
		if [[ x$branch != x ]]; then
			dirty_color=$fg[cyan]
			if [[ $dirty = 1 ]] { dirty_color=$fg[magenta] }
			[ x$branch != x ] && echo "%{$reset_color%}{%{$dirty_color%}$branch%{$reset_color%}} "
		fi
	}
	function git-scoreboard () {
		git log | grep Author | sort | uniq -ci | sort -r
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
