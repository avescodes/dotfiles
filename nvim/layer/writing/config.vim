" vim-orgmode tweaks
augroup org-disable-list
  autocmd!
  autocmd FileType org :set nolist
augroup END

let g:org_heading_shade_leading_stars=0
let g:org_indent=0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Writing
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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
