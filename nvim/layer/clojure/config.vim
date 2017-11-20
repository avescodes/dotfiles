let g:rainbow_active = 1
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,defcomponent,deftask,set-env!,task-options!'
let g:clojure_maxlines = 1000
let g:clojure_fold = 1
setlocal lispwords+=go-loop,try-n-times,fdef

let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'

command! BootCljs :Piggieback (adzerk.boot-cljs-repl/repl-env)

function! s:ReplDoc(symbol)
  exec "Eval (clojure.repl/doc " a:symbol ")"
endfunction

nnoremap <silent> RK :call <SID>ReplDoc(expand('<cword>'))<CR>
nnoremap <leader>r :Require<CR>
nnoremap <leader>t :RunTests<CR>
nnoremap <leader>T :RunAllTests<CR>

nnoremap <F6> :Require<CR>
nnoremap <F5> :RunTests<CR>
