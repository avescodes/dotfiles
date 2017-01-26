function! RefactorDoRemote(arg)
  UpdateRemotePlugins
endfunction

" Lispy plugins
Plug 'guns/vim-sexp' " Attempt to strip down?
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'luochen1990/rainbow'

" Clojure plugins
Plug 'tpope/vim-fireplace'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-clojure-highlight'

Plug 'neovim/node-host', {'do': 'npm install'}
" Plug 'snoe/nvim-parinfer.js', {'do': function('RefactorDoRemote')}
Plug 'snoe/clj-refactor.nvim', {'do': function('RefactorDoRemote')}

" TODO: Move to completion layer?
" Plug 'SevereOverfl0w/async-clj-omni'
Plug 'clojure-vim/async-clj-omni'
Plug 'SevereOverfl0w/unite-clojure'
