export GEMEDITOR=$EDITOR

function ruby_gc_tuning () {
  export RUBY_HEAP_MIN_SLOTS=800000
  export RUBY_HEAP_FREE_MIN=100000
  export RUBY_HEAP_SLOTS_INCREMENT=300000
  export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
  export RUBY_GC_MALLOC_LIMIT=79000000
}
alias rake="noglob rake"

# Rails
alias sc="rails console"
alias ss="rails server"
alias sg="rails generate"
alias sgm="rails generate migration"
