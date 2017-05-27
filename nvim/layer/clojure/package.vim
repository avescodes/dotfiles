function! RefactorDoRemote(arg)
  UpdateRemotePlugins
endfunction

" Lispy plugins
Plug 'guns/vim-sexp' " Attempt to strip down?
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'luochen1990/rainbow'

" Clojure plugins
Plug 'guns/vim-clojure-static'
Plug 'tpope/vim-salve'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fireplace'

" Plug 'guns/vim-clojure-highlight'

Plug 'neovim/node-host', {'do': 'npm install'}
Plug 'snoe/clj-refactor.nvim', {'do': function('RefactorDoRemote')}

Plug 'clojure-vim/async-clj-omni'
