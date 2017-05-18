let g:rainbow_active = 1
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,defcomponent,deftask,set-env!,task-options!'
let g:clojure_maxlines = 1000
setlocal lispwords+=go-loop,try-n-times,fdef

let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'

if !exists('g:test_terminal_id')
  let g:test_terminal_id = -2
endif

function! s:GetOrMakeTerm()
  if g:test_terminal_id <= 0 || jobwait([g:test_terminal_id], 0)[0] <= -2
    botright new
    resize 7
    set wfh
    let g:test_terminal_id = termopen('boot repl -c')
  endif
  return g:test_terminal_id
endfunction

command! -nargs=0 Repl :call s:GetOrMakeTerm()

function! s:ReplDoc(symbol)
  exec "Eval (clojure.repl/doc " a:symbol ")"
endfunction

nnoremap <silent> RK :call <SID>ReplDoc(expand('<cword>'))<CR>
nnoremap <leader>r :Require<CR>

