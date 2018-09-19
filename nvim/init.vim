" Plugins to explore:
"
" - vim-test
" - UltiSnips // other snippet plugin
" - vim-dispatch
"
call plug#begin('~/.local/share/nvim/plugged')

" Editor Style
Plug 'nanotech/jellybeans.vim' " Preferred theme
Plug 'vim-airline/vim-airline' " Status line
Plug 'vim-airline/vim-airline-themes' " Themes for airline

" General Editing
Plug 'godlygeek/tabular', {'on': 'Tabularize'}  " Align blocks of text
Plug 'jiangmiao/auto-pairs' " Automatically complete pairs
Plug 'mileszs/ack.vim' " Search with :Ack
Plug 'tpope/vim-commentary' " Comment out with gc.* commands
Plug 'tpope/vim-endwise' " Add end and other language-aware completions for fn syntax
Plug 'tpope/vim-repeat' " Support repeat for plugins
Plug 'tpope/vim-surround' " Manage delimiters (ys)
Plug 'tpope/vim-projectionist' " Move to related files easily using :E*

" Better Defaults
Plug 'tpope/vim-rsi' " Readline style input
Plug 'markonm/traces.vim' " Show search results
Plug 'dominikduda/vim_current_word' " Highlight current word boundaries
Plug 'mhinz/vim-startify' " Startup screen
Plug 'ntpeters/vim-better-whitespace' " Highlight whitespace, provide :StripWhitespace
Plug 'haya14busa/is.vim' " Better incremental search
Plug 'google/vim-searchindex' " Show search counter

" Language Support
Plug 'jceb/vim-orgmode', {'for': 'org'} " .org support
Plug 'tpope/vim-markdown', {'for': 'markdown'} " .md support

"" Lisps
Plug 'guns/vim-sexp', { 'for': 'clojure' } " S-expression text-objects and manipulations
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure' } " Better bindings for vim-sexp
Plug 'luochen1990/rainbow', { 'for': 'clojure' } " Rainbow parentheses

" Clojure plugins
Plug 'guns/vim-clojure-static', { 'for': 'clojure' } " Clojure runtime files
Plug 'tpope/vim-salve', { 'for': 'clojure' } " Static lein/boot support (:Connect, :E*)
Plug 'tpope/vim-fireplace', {'for': 'clojure'} " Clojure/ClojureScript REPL support
" Plug 'guns/vim-clojure-highlight'
Plug 'snoe/clj-refactor.nvim', { 'for': 'clojure', 'do': ':UpdateRemotePlugins'} " Clojure Refactoring support

"" Elixir
Plug 'c-brenn/phoenix.vim', {'for': 'elixir'} "Phoenix support (jump/gf, :E* projections, server)
Plug 'elixir-editors/vim-elixir', {'for': 'elixir'} " Elixir language support
Plug 'slashmili/alchemist.vim', {'for': 'elixir'} " Completion, doc and mix support


" Utilities
Plug 'gcmt/taboo.vim' " Vim :tabe manager
Plug 'janko-m/vim-test' " :Test runners hooked into multiple languages
Plug 'tpope/vim-eunuch' " Unix utilities
Plug 'tpope/vim-dispatch'  " Async compiler execution (used by vim-salve)
Plug 'radenling/vim-dispatch-neovim' " vim-dispatch support for Neovim
Plug 'svermeulen/vim-easyclip'
Plug 'liuchengxu/vim-which-key'

"" Navigation Utilities
Plug 'scrooloose/nerdtree', {'on': ['NERDTree','NERDTreeToggle']} " Tree-based directory viewer

"" Git Utilities
Plug 'tpope/vim-fugitive' " Git wrapper (:G* family)
Plug 'tommcdo/vim-fugitive-blame-ext' " Git blame ext to show commit msg
Plug 'airblade/vim-gitgutter' " Git status in gutter

" Vim Plugin Editing
Plug 'tpope/vim-scriptease' " Various plugin development utilities

" Neovim Plugins
Plug 'w0rp/ale' " Async linting engine.
Plug 'kassio/neoterm', {'on': ['T']} " :term helpers (e.g. T, Tmap, TREPLSend*)

" Search & Completions
" Plug 'cloudhead/neovim-fuzzy'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' } " Async completions

call plug#end()

" Editor Style  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

colorscheme jellybeans
set background=dark

set laststatus=2  " always show the status bar

let g:airline_powerline_fonts = 1
let g:airline_theme='badcat'
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#branch#enabled=1
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#neomake#enabled=0

" Editor Config """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set directory=/tmp/
set encoding=utf-8
set showcmd                     " display incomplete commands

set list
set listchars=""
set listchars+=tab:▸\ 
set listchars+=trail:·
set listchars+=extends:>
set listchars+=precedes:<

" set wildignore+=target,*.jar,*.class,ivyrepo
" set wildignore+=*.so,*.swp,*.zip,*.un~,.DS_Store,.gitkeep,*/vendor/*,.*,*/coverage/*,*.pyc

set foldlevelstart=99
set number                         " Show numbers gutter
set numberwidth=3                  " Numbers gutter 3 cols wide
set ruler       " show the cursor position all the time
set cursorline
set scrolloff=3
set scrolljump=8
set shortmess=atI
set lazyredraw

set comments+=fb:*              " Make bullet lists reflow nicely

"" Whitespace
set nowrap                      " don't wrap lines
set tabstop=2 shiftwidth=2      " a tab is two spaces (or set this to 4)
set expandtab                   " use spaces, not tabs (optional)
set backspace=indent,eol,start  " backspace through everything in insert mode

""" Strip all trailing whitespace, but not this vim files
augroup striptrailing
  autocmd!
  autocmd BufWritePre * if &ft != "vim" | :%s/\s\+$//e
augroup END

"" Searching
set hlsearch                    " highlight matches
set incsearch                   " incremental searching
set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter
set gdefault                    " Always assume /g on substitutions

"" Splits
set splitbelow
set splitright

set mouse=a " Enable mouse events (scrolling), particularly over tmux+iTerm2

if v:version >= 703
  set undofile
  let &undodir=&directory
endif

" Clojure """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" See also after/ftplugin/clojure.vim

let g:rainbow_active = 1
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,defcomponent,deftask,set-env!,task-options!,s/fdef'
let g:clojure_maxlines = 1000
let g:clojure_fold = 1
setlocal lispwords+=go-loop,try-n-times,fdef

let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^s/fdef']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']
setlocal lispwords+=go-loop,try-n-times,fdef

augroup cljautopairs
  autocmd!
  au FileType clojure let b:AutoPairs = {'(':')','{':'}',"'":"'",'"':'"', '[':']'}
augroup END

" Elixir """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup elixir
  autocmd!
  autocmd FileType elixir
    \ let b:endwise_addition = 'end' |
    \ let b:endwise_words = ''
      \ . 'def,'
      \ . 'defmodule,'
      \ . 'case,'
      \ . 'cond,'
      \ . 'bc,'
      \ . 'lc,'
      \ . 'inlist,'
      \ . 'inbits,'
      \ . 'if,'
      \ . 'unless,'
      \ . 'try,'
      \ . 'receive,'
      \ . 'function,'
      \ . 'fn'
      \ |
    \ let b:endwise_pattern = ''
      \ . '^\s*\zs\%('
        \ . 'def\|'
        \ . 'defmodule\|'
        \ . 'case\|'
        \ . 'cond\|'
        \ . 'bc\|'
        \ . 'lc\|'
        \ . 'inlist\|'
        \ . 'inbits\|'
        \ . 'if\|'
        \ . 'unless\|'
        \ . 'try\|'
        \ . 'receive\|'
        \ . 'function\|'
        \ . 'fn'
      \ . '\)\>\%(.*[^:]\<end\>\)\@!'
      \ |
    \ let b:endwise_syngroups = ''
      \ . 'elixirDefine,'
      \ . 'elixirModuleDefine,'
      \ . 'elixirKeyword'
augroup END

" Status Line """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup numbering
  autocmd!
  autocmd FocusLost * :set number
  autocmd FocusGained * :set relativenumber
  autocmd InsertEnter * :set number
  autocmd InsertLeave * :set relativenumber
  autocmd CursorMoved * :set relativenumber
augroup END

" Remember last location in file, but not for commit messages.
" see :help last-position-jump
augroup lastposnjump
  autocmd!
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g`\"" | endif
augroup END

" tnoremap <Esc> <C-\><C-n>
let g:neoterm_autoscroll = 1

" Search & Completion """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:deoplete#enable_at_startup = 1

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

if executable('rg')
  let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
  let g:ackprg = 'rg --vimgrep --no-heading'
  set grepprg=rg\ --vimgrep
  command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
endif

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" Ctrl-N and Ctrl-P navigate history
let g:fzf_history_dir = '~/.local/share/fzf-history'

let g:fzf_command_prefix = 'Fzf' 

let g:fzf_layout = { 'down': '~30%' }

command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)


let g:neoterm_default_mod = 'rightbelow'
let g:neoterm_automap_keys = ',tt'

let g:gitgutter_map_keys = 0

command! -nargs=* Wrap set wrap linebreak nolist

" Keybindings """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TODO: Document & clean

let mapleader = " "
let maplocalleader = " "

" Unlearn Ctrl-c
inoremap <c-c> <Nop>


" Use tab to navigate tabs
nnoremap <Tab> :tabnext<CR>
nnoremap <S-Tab> :tabprev<CR>

" Picked from https://github.com/tpope/vim-unimpaired
" Quickfix
nnoremap ]q :cnext<cr>zz
nnoremap [q :cprev<cr>zz

" Location
nnoremap ]l :lnext<cr>zz
nnoremap [l :lprev<cr>zz

" Buffers
nnoremap ]b :bnext<cr>
nnoremap [b :bprev<cr>

" Tabs
nnoremap ]t :tabn<cr>
nnoremap [t :tabp<cr>

" Git Changes
nmap [c <Plug>GitGutterPrevHunk
nmap ]c <Plug>GitGutterNextHunk

"" easier navigation between split windows
nmap <c-j> <c-w>j
nmap <c-k> <c-w>k
nmap <c-h> <c-w>h
nmap <c-l> <c-w>l
tnoremap <c-h> <C-\><C-N><C-w>h
tnoremap <c-j> <C-\><C-N><C-w>j
tnoremap <c-k> <C-\><C-N><C-w>k
tnoremap <c-l> <C-\><C-N><C-w>l
inoremap <c-h> <C-\><C-N><C-w>h
inoremap <c-j> <C-\><C-N><C-w>j
inoremap <c-k> <C-\><C-N><C-w>k
inoremap <c-l> <C-\><C-N><C-w>l
nnoremap <c-h> <C-w>h
nnoremap <c-j> <C-w>j
nnoremap <c-k> <C-w>k
nnoremap <c-l> <C-w>l

"" Reselect visual block after indent/outdent - vimbits.com/bits/20
vnoremap < <gv
vnoremap > >gv

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Run tests quickly
vnoremap <silent> <f2> :TREPLSendSelection<cr>
nnoremap <silent> <f3> :TREPLSendLine<cr>
nnoremap <silent> <f4> :TREPLSendFile<cr>


"" Leader Tree """""""""""""""""""""""""""""""""""
let g:which_key_map = {}

let g:which_key_map['?'] = [ 'FzfMaps', 'show-keybindings' ]
let g:which_key_map[';'] = [ '<Plug>NERDCommenterToggle','commenter' ]

nnoremap <silent> <leader>1 :1wincmd w<CR>
nnoremap <silent> <leader>2 :2wincmd w<CR>
nnoremap <silent> <leader>3 :3wincmd w<CR>
nnoremap <silent> <leader>4 :4wincmd w<CR>
nnoremap <silent> <leader>5 :5wincmd w<CR>
nnoremap <silent> <leader>6 :6wincmd w<CR>
nnoremap <silent> <leader>7 :7wincmd w<CR>
nnoremap <silent> <leader>8 :8wincmd w<CR>
nnoremap <silent> <leader>9 :9wincmd w<CR>

let g:which_key_map.1 = 'which_key_ignore'
let g:which_key_map.2 = 'which_key_ignore'
let g:which_key_map.3 = 'which_key_ignore'
let g:which_key_map.4 = 'which_key_ignore'
let g:which_key_map.5 = 'which_key_ignore'
let g:which_key_map.6 = 'which_key_ignore'
let g:which_key_map.7 = 'which_key_ignore'
let g:which_key_map.7 = 'which_key_ignore'
let g:which_key_map.8 = 'which_key_ignore'
let g:which_key_map.9 = 'which_key_ignore'

" tab
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" a - align commands
let g:which_key_map['a'] = {
      \ 'name' : '+align',
      \ }

nmap <Leader>a&     :Tabularize /&<CR>
vmap <Leader>a&     :Tabularize /&<CR>
nmap <Leader>a=     :Tabularize /^[^=]*\zs=<CR>
vmap <Leader>a=     :Tabularize /^[^=]*\zs=<CR>
nmap <Leader>a=>    :Tabularize /=><CR>
vmap <Leader>a=>    :Tabularize /=><CR>
nmap <Leader>a:     :Tabularize /:<CR>
vmap <Leader>a:     :Tabularize /:<CR>
nmap <Leader>a::    :Tabularize /:\zs<CR>
vmap <Leader>a::    :Tabularize /:\zs<CR>
nmap <Leader>a,     :Tabularize /,<CR>
vmap <Leader>a,     :Tabularize /,<CR>
nmap <Leader>a,,    :Tabularize /,\zs<CR>
vmap <Leader>a,,    :Tabularize /,\zs<CR>
nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
vmap <Leader>a<Bar> :Tabularize /<Bar><CR>

" b - buffer commands
let g:which_key_map['b'] = {
      \ 'name' : '+buffers',
      \ '1' :  'buffer-1'        ,
      \ '2' :  'buffer-2'        ,
      \ '3' :  'buffer-3'        ,
      \ '4' :  'buffer-4'        ,
      \ '5' :  'buffer-5'        ,
      \ '6' :  'buffer-6'        ,
      \ '7' :  'buffer-7'        ,
      \ '8' :  'buffer-8'        ,
      \ '9' :  'buffer-9'        ,
      \ 'd' :  'delete-buffer'   ,
      \ 'f' :  'first-buffer'    ,
      \ 'h' :  'home-buffer'     ,
      \ 'k' :  'kill-buffer'     ,
      \ 'l' :  'last-buffer'     ,
      \ 'n' :  'next-buffer'     ,
      \ 'p' :  'previous-buffer' ,
      \ 'b' :  'fzf-buffer'      ,
      \ '?' : 'fzf-buffer' ,
      \ }
nnoremap <silent><Leader>bh :Startify<CR>

" c - quickfix window
let g:which_key_map['c'] = {
      \ 'name' : '+quickfix',
      \ 'c' : 'close',
      \ 'n' : 'next-error'     ,
      \ 'p' : 'previous-error' ,
      \ }
nnoremap <silent> <leader>cc :cclose<CR>
nnoremap <silent> <leader>cn :cnext<CR>
nnoremap <silent> <leader>cp :cprev<CR>

" f - fzf commands
let g:which_key_map['f'] = {
      \ 'name' : '+fzf',
      \ }
nnoremap <silent> <Leader>f/ :FzfAg<CR>
nnoremap <silent> <Leader>fl :FzfBLines<CR>    " Lines in the current buffer
nnoremap <silent> <Leader>fb :FzfBuffers<CR>   " Open buffers
nnoremap <silent> <Leader>fC :FzfColors<CR>    " Color schemes
nnoremap <silent> <Leader>fc :FzfCommands<CR>  " Commands
nnoremap <silent> <Leader>ff :FzfFiles<CR>     " Files (similar to :FZF
nnoremap <silent> <Leader>fgc :FzfCommits<CR>  " Git commits (requires [fugitive.vim][f
nnoremap <silent> <Leader>fgC :FzfBCommits<CR> " Git commits for the current buffer
nnoremap <silent> <Leader>fgf :FzfGFiles<CR>   " Git files (git ls-files
nnoremap <silent> <Leader>fgs :FzfGFiles?<CR>  " Git files (git status
nnoremap <silent> <Leader>fh :FzfHelptags<CR>  " Help tags <sup id="a1" >[1](#helptags)</sup>
nnoremap <silent> <Leader>fH/ :FzfHistory/<CR> " Search history
nnoremap <silent> <Leader>fH: :FzfHistory:<CR> " Command history
nnoremap <silent> <Leader>fHf :FzfHistory<CR>  " v:oldfiles and open buffers
nnoremap <silent> <Leader>fl :FzfLines<CR>     " Lines in loaded buffers
nnoremap <silent> <Leader>fm :FzfMaps<CR>      " Normal mode mappings
nnoremap <silent> <Leader>fm :FzfMarks<CR>     " Marks
nnoremap <silent> <Leader>fs :FzfSnippets<CR>  " Snippets ([UltiSnips][us
nnoremap <silent> <Leader>ft :FzfTags<CR>      " Tags in the project (ctags -R
nnoremap <silent> <Leader>fT :FzfBTags<CR>     " Tags in the current buffer
nnoremap <silent> <Leader>fw :FzfWindows<CR>   " Windows

" m - marks
let g:which_key_map['m'] = {
      \ 'name' : 'mark',
      \ }
nnoremap <Leader>m m

" p
let g:which_key_map['p'] = {
      \ 'name' : 'find-file-in-project',
      \ }
nnoremap <silent> <Leader>p :FZF<CR>

" t - vim-test mappings
let g:which_key_map['t'] = {
      \ 'name' : '+test',
      \ }
nmap <silent> <leader>tt :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ts :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tv :TestVisit<CR>

" T - neoterm commands
let g:which_key_map['T'] = {
      \ 'name' : '+terminal',
      \ }
nnoremap <silent> <leader>Ts :TREPLSendSelection<CR>
nnoremap <silent> <leader>Tf :TREPLSendFile<CR>
nnoremap <silent> <leader>Td :TREPLSendLine<cr>
nnoremap <silent> <leader>Th :call neoterm#close()<cr>
nnoremap <silent> <leader>Tl :call neoterm#clear()<cr>
nnoremap <silent> <leader>Tx :call neoterm#kill()<cr>

" g - Git commands
let g:which_key_map['g'] = {
      \ 'name' : '+git',
      \ 's': ['Gstatus', 'git-status'],
      \ 'c': ['Gcommit', 'commit'],
      \ 'p': ['Gpush', 'git-push'],
      \ 'f': ['Gpull', 'git-pull'],
      \ 'W': ['Gwrite', 'git-write'],
      \ 'd': ['Gdiff', 'git-diff'],
      \ 'D': ['Gvdiff', 'git-vdiff'],
      \ 'b': ['Gblame', 'git-blame'],
\ }

" w - window management
let g:which_key_map['w'] = {
      \ 'name' : '+windows' ,
      \ 'w' : ['<C-W>w'     , 'other-window']          ,
      \ 'd' : ['<C-W>c'     , 'delete-window']         ,
      \ '-' : ['<C-W>s'     , 'split-window-below']    ,
      \ '|' : ['<C-W>v'     , 'split-window-right']    ,
      \ '2' : ['<C-W>v'     , 'layout-double-columns'] ,
      \ 'h' : ['<C-W>h'     , 'window-left']           ,
      \ 'j' : ['<C-W>j'     , 'window-below']          ,
      \ 'l' : ['<C-W>l'     , 'window-right']          ,
      \ 'k' : ['<C-W>k'     , 'window-up']             ,
      \ 'H' : ['<C-W>5<'    , 'expand-window-left']    ,
      \ 'J' : ['resize +5'  , 'expand-window-below']   ,
      \ 'L' : ['<C-W>5>'    , 'expand-window-right']   ,
      \ 'K' : ['resize -5'  , 'expand-window-up']      ,
      \ '=' : ['<C-W>='     , 'balance-window']        ,
      \ 's' : ['<C-W>s'     , 'split-window-below']    ,
      \ 'v' : ['<C-W>v'     , 'split-window-below']    ,
      \ '?' : ['Windows'    , 'fzf-window']            ,
      \ }

let g:which_key_map['.'] = {
      \ 'name' : '+vim',
      \ 's' : ['StripWhitespace', 'strip-whitespace'],
      \ 'l' : ['set list!', 'Toggle List'],
      \ 'w' : ['set wrap!', 'Toggle Soft Wrap'],
      \ }

" Enable WhichKey
call which_key#register('<Space>', "g:which_key_map")
nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
set timeoutlen=200
