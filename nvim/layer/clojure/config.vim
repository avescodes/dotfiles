let g:rainbow_active = 1
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,defcomponent,deftask,set-env!,task-options!'
let g:clojure_maxlines = 1000
setlocal lispwords+=go-loop,try-n-times,fdef

command! BootCljs Piggieback (adzerk.boot-cljs-repl/repl-env)

function! s:ReplDoc(symbol)
  exec "Eval (clojure.repl/doc " a:symbol ")"
endfunction

