Plug 'AnsiEsc.vim'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'benekastah/neomake', { 'on': ['Neomake'] }
Plug 'godlygeek/tabular', { 'on': 'Tabularize' }
Plug 'majutsushi/tagbar', { 'on': 'Tagbar' }
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch', { 'on': [ 'Rename', 'Move', 'SudoWrite' ] }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'

Plug 'junegunn/fzf.vim'
set rtp+=/usr/local/opt/fzf


" Syntaxon
Plug 'tpope/vim-markdown'
Plug 'dotcloud/docker', {
  \ 'rtp': 'contrib/syntax/vim'
  \ }
Plug 'wting/rust.vim', { 'for': 'rust' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
