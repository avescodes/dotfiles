
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Bundles
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.config/nvim/plugged')


" Aesthetics
Plug 'nanotech/jellybeans.vim'
" Plug 'bling/vim-airline'
Plug 'chriskempson/base16-vim'

" Syntaxon
Plug 'tpope/vim-git'
Plug 'tpope/vim-markdown'
Plug 'dotcloud/docker', {
  \ 'rtp': 'contrib/syntax/vim'
  \ }
Plug 'wting/rust.vim', { 'for': 'rust' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
" Plug 'Raimondi/delimitMate'

" Clojure
" Plug 'vim-scripts/vim-niji'
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
Plug 'guns/vim-sexp', { 'for': 'clojure' }
Plug 'tpope/vim-dispatch', { 'for': 'clojure' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'tpope/vim-leiningen', { 'for': 'clojure' }
Plug 'rkneufeld/vim-boot', { 'for': 'clojure' }
Plug 'tpope/vim-scriptease', { 'for': 'clojure' }
Plug 'tpope/vim-projectionist', { 'for': 'clojure' }
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure' }

" Extras
Plug 'ConradIrwin/vim-bracketed-paste'
" Plug 'TailMinusF'
Plug 'godlygeek/tabular', { 'on': 'Tabularize' }
Plug 'majutsushi/tagbar', { 'on': 'Tagbar' }
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-eunuch', { 'on': [ 'Rename', 'Move', 'SudoWrite' ] }
Plug 'jceb/vim-orgmode'
Plug 'AnsiEsc.vim'

" Neovim
Plug 'benekastah/neomake', { 'on': ['Neomake'] }
Plug 'Shougo/deoplete.nvim'
Plug 'junegunn/fzf.vim'

set rtp+=/usr/local/opt/fzf

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General config
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

syntax enable

set directory=/tmp/
set encoding=utf-8
set background=dark
colorscheme jellybeans
set showcmd                     " display incomplete commands
set list
set listchars=""
set listchars+=tab:▸\ 
set listchars+=trail:·
set listchars+=extends:>
set listchars+=precedes:<

set wildignore+=target,*.jar,*.class,ivyrepo
set wildignore+=*.so,*.swp,*.zip,*.un~,.DS_Store,.gitkeep,*/vendor/*,.*,*/coverage/*,*.pyc

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

" Strip all trailing whitespace, but not this vim files
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

" Splits
set splitbelow
set splitright

set mouse=a " Enable mouse events (scrolling), particularly over tmux+iTerm2

if v:version >= 703
  set undofile
  let &undodir=&directory
endif

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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Status line
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set laststatus=2  " always show the status bar

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Extras Customization
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" vim-airline
" let g:airline_powerline_fonts=1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#branch#enabled=1
let g:airline#extensions#tabline#enabled=1

"" vim-clojure-static
let g:clojure_maxlines = 1000 " Search more lines for identation (correctness over speed)
let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^dom\/.*']

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

" Remember last location in file, but not for commit messages.
" see :help last-position-jump
augroup lastposnjump
  autocmd!
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g`\"" | endif
augroup END

" Treat JSON files like JavaScript
augroup filetypes
  autocmd!
  autocmd BufRead,BufNewFile {Vagrantfile,Guardfile,Gemfile,Rakefile,Capfile,*.rake,config.ru} set filetype=ruby
  autocmd BufRead,BufNewFile {COMMIT_EDITMSG}                                                  set filetype=gitcommit
  autocmd BufRead,BufNewFile {*.json}                                                          set filetype=javascript
  autocmd BufRead,BufNewFile {*.cljs,*.hl,*.edn,*.cljx,*.boot}                                 setlocal filetype=clojure
  autocmd BufRead,BufNewFile {*.gradle}                                                        setlocal filetype=groovy
  autocmd FileType           python                                                            setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4
  autocmd FileType           rust                                                              setlocal tabstop=4 expandtab shiftwidth=4 softtabstop=4
augroup END


" delimitMate
" let loaded_delimitMate = 1
augroup clojure-delimitMate
  autocmd!
  autocmd FileType clojure let b:loaded_delimitMate = 0
augroup END

" Keybindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let mapleader = ","
let maplocalleader = ","

nmap <F1> <nop>

" No help please
nmap <F1> <Esc>

" easier navigation between split windows
nmap <c-j> <c-w>j
nmap <c-k> <c-w>k
nmap <BS> <C-w>h
nmap <c-l> <c-w>l

"Add some nice short cuts for tab swapping
nnoremap <silent> <C-n> :tabnext<CR>
nnoremap <silent> <C-p> :tabprevious<CR>

"Tag traversal
nnoremap <silent> <Leader>b :TagbarToggle<CR>
nmap <F7> :TagbarOpen j<CR>

" Reselect visual block after indent/outdent - vimbits.com/bits/20
vnoremap < <gv
vnoremap > >gv

" Rename current file
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

nnoremap <leader>r :Require<CR>
nnoremap <leader>e :Eval (clojure.repl/pst *e)<CR>
nnoremap <leader>q <Plug>FireplacePrompt

" Profiling Support
nnoremap <silent> <leader>DD :exe ":profile start profile.log"<cr>:exe ":profile func *"<cr>:exe ":profile file *"<cr>
nnoremap <silent> <leader>DP :exe ":profile pause"<cr>
nnoremap <silent> <leader>DC :exe ":profile continue"<cr>
nnoremap <silent> <leader>DQ :exe ":profile pause"<cr>:noautocmd qall!<cr>

" vim-orgmode tweaks
augroup org-disable-list
  autocmd!
  autocmd FileType org :set nolist
augroup END

let g:org_heading_shade_leading_stars=0
let g:org_indent=0

" tnoremap <Esc> <C-\><C-n>

" if has('nvim')
"   let $FZF_DEFAULT_OPTS .= ' --inline-info'
" endif

nnoremap <silent> <Leader><Leader> :Files<CR>
nnoremap <silent> <Leader>p        :Files<CR>
nnoremap <silent> <Leader>f        :Files<CR>
nnoremap <silent> <Leader>C        :Colors<CR>
nnoremap <silent> <Leader>b        :Buffers<CR>
nnoremap <silent> <Leader>ag       :Ag <C-R><C-W><CR>

nnoremap p :File<cr>
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Writing
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Bundle 'reedes/vim-pencil'
" Bundle 'reedes/vim-lexical'
" Bundle 'reedes/vim-wheel'
" Bundle 'reedes/vim-wordy'
" Bundle 'reedes/vim-litecorrect'
" Bundle 'kana/vim-textobj-user'
" Bundle 'reedes/vim-textobj-sentence'

" let g:pencil#wrapModeDefault = 'soft'   " or 'soft'

" augroup pencil
"   autocmd!
"   autocmd FileType markdown call pencil#init()
"   " autocmd FileType text call pencil#init()
" augroup END

" augroup lexical
"   autocmd!
"   autocmd FileType markdown call lexical#init()
"   " autocmd FileType text call lexical#init({ 'spell': 0 })
" augroup END

" augroup litecorrect
"   autocmd!
"   autocmd FileType markdown call litecorrect#init()
"   " autocmd FileType text call litecorrect#init()
" augroup END

" augroup textobj_sentence
"   autocmd!
"   autocmd FileType markdown call textobj#sentence#init()
"   " autocmd FileType text call textobj#sentence#init()
" augroup END
