let g:rainbow_active = 1
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,defcomponent,deftask,set-env!,task-options!'
let g:clojure_maxlines = 1000
setlocal lispwords+=go-loop,try-n-times,fdef

let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'

let g:sexp_enable_insert_mode_mappings = 0

function! s:SwapParinferMode()
  if g:parinfer_mode == "off"
    let g:parinfer_mode = "indent"
  elseif g:parinfer_mode == "indent"
    let g:parinfer_mode = "paren"
  elseif g:parinfer_mode == "paren"
    let g:parinfer_mode = "indent"
  endif
endfunction

command! -nargs=0 SwapParinfer :call s:SwapParinferMode()
nnoremap <buffer> <localleader>sp :call <SID>SwapParinferMode()<CR>

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

function! s:RunAllMyTests(myarg)
  " call jobsend(s:GetOrMakeTerm(), ["(require 'dev) (dev/run-all-my-tests)", ""])
  call jobsend(s:GetOrMakeTerm(), ["(require 'eftest.runner) (eftest.runner/run-tests (eftest.runner/find-tests " . a:myarg . ") {:multithread? false})", ""])
endfunction

command! -nargs=* RunDevTests :call s:RunAllMyTests(<q-args>)

function! s:ReplDoc(symbol)
  exec "Eval (clojure.repl/doc " a:symbol ")"
endfunction

nnoremap <silent> RK :call <SID>ReplDoc(expand('<cword>'))<CR>
nnoremap <leader>r :Require<CR>

function! FindSymbol(symbol, ns, curr_file, pos)
  let info = get(fireplace#message({'op': 'info', 'symbol': a:symbol, 'ns': a:ns}), 0, {})

  let response = fireplace#message({'op': 'find-symbol', 'dir': getcwd(), 'file': a:curr_file, 'ns': get(info, 'ns', a:ns), 'name': get(info, 'name', a:symbol), 'line': a:pos[1], 'column': a:pos[2], 'serialization-format': 'bencode'})

  let occurences = []

  " echo response
  for x in response
    if has_key(x, 'occurrence')
      call add(occurences, x['occurrence'])
    endif
  endfor

  return occurences
endfunction

command! FindSymbol :echo FindSymbol(expand('<cword>'), fireplace#ns(), expand('%:p'), getpos('.'))

function! s:unite_find_symbol(args, context)
  let candidates = []
  let occurrences = FindSymbol(a:context.source__symbol,
                             \ a:context.source__ns,
                             \ a:context.source__curr_file,
                             \ a:context.source__pos)

  for o in occurrences
    call add(candidates, {
          \ 'word': printf('%s:%s %s', fnamemodify(o['file'], ':~:.'),
          \                o['line-beg'] . (o['col-beg'] != 0 ? ':'.o['col-beg'] : ''),
          \                split(o['match'], '\n')[0]),
          \ 'action__path': o['file'],
          \ 'action__line': o['line-beg'],
          \ 'action__col': o['col-beg']
          \ })
  endfor

  return candidates
endfunction

function! s:unite_find_symbol_init(args, context)
  let a:context.source__symbol = expand('<cword>')
  let a:context.source__ns = fireplace#ns()
  let a:context.source__curr_file = expand('%:p')
  let a:context.source__pos = getpos('.')
endfunction

function! s:unite_find_symbol_syntax(args, context)
  let info = fireplace#message({'op': 'info', 'symbol': a:context.source__symbol, 'ns': a:context.source__ns})[0]
  syntax case ignore
  syntax match uniteSource__FSHeader /[^:]*: \d\+: \(\d\+: \)\?/ contained
        \ containedin=uniteSource__FS
  syntax match uniteSource__FSFile /[^:]*: / contained
        \ containedin=uniteSource__FSHeader
        \ nextgroup=uniteSource__FSLineNR
  syntax match uniteSource__FSLineNR /\d\+: / contained
        \ containedin=uniteSource__FSHeader
        \ nextgroup=uniteSource__FSPattern
  " execute 'syntax match uniteSource__FSPattern /'
  "       \ . substitute(info['name'], '\([/\\]\)', '\\\1', 'g')
  "       \ . '/ contained containedin=uniteSource__FS'
  syntax match uniteSource__FSSeparator /:/ contained conceal
        \ containedin=uniteSource__FSFile,uniteSource__FSLineNR
  highlight default link uniteSource__FSFile Comment
  highlight default link uniteSource__FSLineNr LineNR
  execute 'highlight default link uniteSource__FSPattern'
        \ get(a:context, 'custom_grep_search_word_highlight',
        \ g:unite_source_grep_search_word_highlight)
endfunction

call unite#define_source({
      \ 'name': 'clj_find_symbol',
      \ 'gather_candidates': function('s:unite_find_symbol'),
      \ 'hooks': {'on_init': function('s:unite_find_symbol_init'),
                \ 'on_syntax': function('s:unite_find_symbol_syntax')},
      \ 'syntax' : 'uniteSource__FS',
      \ 'default_kind': 'jump_list',
      \ 'description': 'Find usages of symbol for Clojure using nrepl'
      \ })
"
"" Tagbar + Clojure
let g:tagbar_type_clojure = {
    \ 'ctagstype' : 'clojure',
    \ 'kinds'     : [
        \ 'n:ns',
        \ 'd:def',
        \ 'c:defonce',
        \ 'o:defprotocol',
        \ 'f:defn',
        \ 'p:defn-',
        \ 'm:defmacro',
        \ 'i:definline',
        \ 'a:defmulti',
        \ 'b:defmethod',
        \ 's:defstruct',
        \ 'v:intern',
    \ ]
\ }


" These make more sense to me
nmap <buffer> gs <Plug>FireplaceDjump
nmap <buffer> gvs <Plug>FireplaceDsplit

" TODO: Hammock an argument for input?
function! RunRepl(cmd)
  tabnew
  if executable('rlwrap')
    call termopen('rlwrap ' . a:cmd)
  else
    call termopen(a:cmd)
  endif
  set syntax=clojure
  tabprevious
endfunction

function! GradleRepl(cmd)
  if a:0 > 0 && a:1 != ''
    call RunRepl('gradle '.join(a:000, ' '))
  else
    call RunRepl('gradle :switch:nrepl')
  endif
endfunction

function! BootRepl(...)
  if a:0 > 0 && a:1 != ''
    call RunRepl('boot '.join(a:000, ' '))
  else
    call RunRepl('boot dev repl')
  endif
endfunction

" TODO: Take an optional arg for alternative tasks
command! -nargs=* -buffer Boot :exe BootRepl(<q-args>)
command! -nargs=* -buffer Gradle :exe GradleRepl(<q-args>)
command! -buffer ReplConnect :FireplaceConnect('nrepl://localhost:7888')
command! -nargs=* -buffer CljsRepl :call RunRepl("boot repl -c --port 3001")
command! -buffer Lein :call RunRepl("lein repl")
command! -buffer Figwheel :call RunRepl("lein figwheel")")

" TODO: if/else this,and warn
command! -buffer Cljsbuild :Dispatch lein cljsbuild once

if !exists('b:dev_ns')
  let b:dev_ns = 'dev'
endif

function! s:CiderRefresh()
  " TODO: Collect reloading key from all messages, just in case
  let refresh = fireplace#message({'op': 'refresh'})
  echon 'reloading: (' join(refresh[0]['reloading'], ' ') ')'
endfunction

" Stuart Sierra's reloaded workflow
nnoremap <buffer> <localleader>go :call fireplace#echo_session_eval('(go)', {'ns': b:dev_ns})<CR>
nnoremap <buffer> <localleader>rs :call fireplace#echo_session_eval('(reset)', {'ns': b:dev_ns})<CR>
nnoremap <buffer> <localleader>ra :call fireplace#echo_session_eval('(reset-all)', {'ns': b:dev_ns})<CR>
nnoremap <buffer> <localleader>ff :call fireplace#echo_session_eval('(clojure.tools.namespace.repl/refresh)', {'ns': 'user'})<CR>
nnoremap <buffer> <localleader>fa :call fireplace#echo_session_eval('(clojure.tools.namespace.repl/refresh-all)', {'ns': 'user'})<CR>
nnoremap <buffer><silent> <localleader>cf :call <SID>CiderRefresh()<CR>

function! s:EvalIn(args)
  let sargs = split(a:args)
  call fireplace#echo_session_eval(join(sargs[1:-1]), {'ns': sargs[0]})
endfunction

command! -buffer -nargs=* EvalIn :exe s:EvalIn(<q-args>)

if expand('%:t') == 'build.boot'
  let b:fireplace_ns = 'boot.user'
endif

setlocal foldmethod=syntax
