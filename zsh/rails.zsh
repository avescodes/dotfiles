find-rakefile () {
  if [ -f $1/Rakefile ]; then
    echo $1
  else 
    echo $(find-rakefile $1/..)
  fi
}

rails-script () {
  rfpath=$(find-rakefile .)
  target=$1; shift
  if [ -f $rfpath/script/rails ]; then 
    $rfpath/script/rails $target $argv
  else
    $rfpath/script/$target $argv
  fi
}

sc () { rails-script "console"  $argv }
ss () { rails-script "server"   $argv }
sg () { rails-script "generate" $argv }
sr () { rails-script "runner"   $argv }
sgm() { rails-script "generate" "migration" $argv }
sgs() { rails-script "generate" "scaffold"  $argv }
sgr() { rails-script "generate" "resource"  $argv }


