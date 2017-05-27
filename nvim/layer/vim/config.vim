"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General config
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

syntax enable

colorscheme jellybeans
set background=dark

set directory=/tmp/
set encoding=utf-8
set showcmd                     " display incomplete commands
set list
set listchars=""
set listchars+=tab:▸\ 
set listchars+=trail:·
set listchars+=extends:>
set listchars+=precedes:<

set wildignore+=target,*.jar,*.class,ivyrepo
set wildignore+=*.so,*.swp,*.zip,*.un~,.DS_Store,.gitkeep,*/vendor/*,.*,*/coverage/*,*.pyc

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

" Remember last location in file, but not for commit messages.
" see :help last-position-jump
augroup lastposnjump
  autocmd!
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g`\"" | endif
augroup END

let g:deoplete#enable_at_startup = 0

tnoremap <Esc> <C-\><C-n>

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
