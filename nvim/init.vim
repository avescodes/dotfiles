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
Plug 'ConradIrwin/vim-bracketed-paste' " Paste support for iTerm2 paste
Plug 'godlygeek/tabular' " Align blocks of text
Plug 'jiangmiao/auto-pairs' " Automatically complete pairs
Plug 'mileszs/ack.vim' " Search with :Ack
Plug 'tpope/vim-commentary' " Comment out with gc.* commands
Plug 'tpope/vim-endwise' " Add end and other language-aware completions for fn syntax
Plug 'tpope/vim-repeat' " Support repeat for plugins
Plug 'tpope/vim-speeddating' " Extended C-a/x incrementor/decrementors.
Plug 'tpope/vim-surround' " Manage delimiters (ys)
Plug 'tpope/vim-projectionist' " Move to related files easily using :E*

" Language Support
Plug 'dotcloud/docker', { 'rtp': 'contrib/syntax/vim' } " Dockerfile support
Plug 'jceb/vim-orgmode' " .org support
Plug 'tpope/vim-markdown' " .md support
Plug 'udalov/kotlin-vim' " Kotlin lang

"" Lisps
Plug 'guns/vim-sexp' " S-expression text-objects and manipulations
Plug 'tpope/vim-sexp-mappings-for-regular-people' " Better bindings for vim-sexp
Plug 'luochen1990/rainbow' " Rainbow parentheses

" Clojure plugins
Plug 'guns/vim-clojure-static' " Clojure runtime files
Plug 'tpope/vim-salve' " Static lein/boot support (:Connect, :E*)
Plug 'tpope/vim-fireplace' " Clojure/ClojureScript REPL support
" Plug 'guns/vim-clojure-highlight'
Plug 'snoe/clj-refactor.nvim', {'do': ':UpdateRemotePlugins'} " Clojure Refactoring support

"" Elixir
Plug 'c-brenn/phoenix.vim' "Phoenix support (jump/gf, :E* projections, server)
Plug 'elixir-editors/vim-elixir' " Elixir language support
Plug 'slashmili/alchemist.vim' " Completion, doc and mix support
""" TODO: Trial elixir.nvim + related plugins
" Plug 'awetzel/elixir.nvim', { 'do': 'yes \| ./install.sh' }
" Plug 'thinca/vim-ref' 


" Utilities
Plug 'gcmt/taboo.vim' " Vim :tabe manager
Plug 'janko-m/vim-test' " :Test runners hooked into multiple languages
Plug 'tpope/vim-eunuch' " Unix utilities
Plug 'tpope/vim-dispatch'  " Async compiler execution (used by vim-salve)
Plug 'radenling/vim-dispatch-neovim' " vim-dispatch support for Neovim

"" Navigation Utilities
Plug 'majutsushi/tagbar' " Visualize/jump to ctags. (TODO: need Clojure tag config)
Plug 'scrooloose/nerdtree' " Tree-based directory viewer

"" Git Utilities
Plug 'tpope/vim-fugitive' " Git wrapper (:G* family)
Plug 'tommcdo/vim-fugitive-blame-ext' " Git blame ext to show commit msg
Plug 'airblade/vim-gitgutter' " Git status in gutter
Plug 'junegunn/gv.vim' " Git commit viewer (GV[!?])

" Vim Plugin Editing
Plug 'tpope/vim-scriptease' " Various plugin development utilities

" Neovim Plugins
Plug 'benekastah/neomake', { 'on': ['Neomake'] } " Used for async make. Drop?
Plug 'w0rp/ale' " Async linting engine. Use w/ joker 
Plug 'kassio/neoterm' " :term helpers (e.g. T, Tmap, TREPLSend*)

" Search & Completions
Plug 'cloudhead/neovim-fuzzy'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' } " Async completions
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer'} " TRIAL: Code completion engine
" Plug 'clojure-vim/async-clj-omni' " TODO: Diagnose freezing of UI input
" Plug 'roxma/nvim-completion-manager' " TODO: Reevaluate against deoplete + async-clj-omni support

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

command! BootCljs :Piggieback (adzerk.boot-cljs-repl/repl-env)

function! s:ReplDoc(symbol)
  exec "Eval (clojure.repl/doc " a:symbol ")"
endfunction

augroup cljautopairs
  autocmd!
  au FileType clojure let b:AutoPairs = {'(':')','{':'}',"'":"'",'"':'"', '[':']'}
augroup END

" Elixir """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>

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

function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

" nnoremap <C-n> :call NumberToggle()<cr>

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

" Neovim Plugins """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup neomake
  autocmd!
  autocmd BufWritePost,BufEnter *.scss Neomake
augroup END

" See also ftdetect/kotlin_neoterm.vim

" Search & Completion """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:deoplete#enable_at_startup = 1

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" Keybindings """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TODO: Document & clean

let mapleader = ","
let maplocalleader = ","

"" No help please
nmap <F1> <Esc>
nmap <F1> <nop>

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

"" Add some nice short cuts for tab swapping
nnoremap <silent> <C-n> :tabnext<CR>
nnoremap <silent> <C-p> :tabprevious<CR>

"" Tag traversal
nnoremap <silent> <Leader>b :TagbarToggle<CR>
nmap <F7> :TagbarOpen<CR>

"" Reselect visual block after indent/outdent - vimbits.com/bits/20
vnoremap < <gv
vnoremap > >gv

"" Rename current file
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction

map <leader>mv :call RenameFile()<cr>

command! KillWhitespace :normal :%s/ *$//g<cr><c-o><cr>
map <leader>ws :call KillWhitespace

"" Profiling Support
nnoremap <silent> <leader>DD :exe ":profile start profile.log"<cr>:exe ":profile func *"<cr>:exe ":profile file *"<cr>
nnoremap <silent> <leader>DP :exe ":profile pause"<cr>
nnoremap <silent> <leader>DC :exe ":profile continue"<cr>
nnoremap <silent> <leader>DQ :exe ":profile pause"<cr>:noautocmd qall!<cr>

"" Search Assistance
nnoremap <silent> <Leader>p        :FuzzyOpen<CR>
nnoremap <silent> <Leader>g        :FuzzyGrep<CR>

function! FzyCommand(choice_command, vim_command)
  try
    let output = system(a:choice_command . " | fzy ")
  catch /Vim:Interrupt/
    " Swallow errors from ^C, allow redraw! below
  endtry
  redraw!
  if v:shell_error == 0 && !empty(output)
    exec a:vim_command . ' ' . output
  endif
endfunction

nnoremap <leader>e :call FzyCommand("ag . --silent -l -g ''", ":e")<cr>
nnoremap <leader>v :call FzyCommand("ag . --silent -l -g ''", ":vs")<cr>
nnoremap <leader>s :call FzyCommand("ag . --silent -l -g ''", ":sp")<cr>

nnoremap <silent> <leader>c :ccl<CR>

nnoremap <silent> <Leader>ts        :TREPLSendSelection<CR>
nnoremap <silent> <Leader>tf        :TREPLSendFile<CR>

let g:neoterm_default_mod = 'rightbelow'
let g:neoterm_automap_keys = ',tt'

nnoremap <silent> <f10> :TREPLSendFile<cr>
nnoremap <silent> <f9> :TREPLSendLine<cr>
vnoremap <silent> <f9> :TREPLSendSelection<cr>

"" neoterm

" hide/close terminal
nnoremap <silent> ,th :call neoterm#close()<cr>
" clear terminal
nnoremap <silent> ,tl :call neoterm#clear()<cr>
" kills the current job (send a <c-c>)
nnoremap <silent> ,tc :call neoterm#kill()<cr>

"" Git
" See also after/ftplugin/git{commit,rebase}.vim

let g:gitgutter_map_keys = 0

command! -nargs=+ Tg :T git <args>
nnoremap <Leader>Gv :GV
nnoremap <Leader>Gs :Gstatus<CR>
nnoremap <Leader>Gc :Gcommit<CR>
nnoremap <Leader>Gp :Gpush<CR>
nnoremap <Leader>Gf :Gpull<CR>
nnoremap <Leader>GW :Gwrite<CR>:Gcommit<CR>
nnoremap <Leader>Gd :Gdiff<CR>
nnoremap <Leader>GD :Gvdiff<CR>
nnoremap <Leader>Gb :Gblame<CR>

nmap [c <Plug>GitGutterPrevHunk
nmap ]c <Plug>GitGutterNextHunk
