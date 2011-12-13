" Default Window size
set winwidth=85
let g:halfsize = 86
let g:fullsize = 171
set lines=50
let &columns = g:halfsize

" Font
set guifont=Pragmata_TT:h15.00
set antialias! " Turn off AA

" No audible bell
set vb

set guioptions-=T " no toolbar
set guioptions-=R " RLrl are right- and left-hand scrollbar options for the gui
set guioptions-=L
set guioptions-=r
set guioptions-=l
set guioptions+=c " Use console dialogs

if has("gui_macvim")
  " Fullscreen takes up entire screen
  set fuoptions=maxhorz,maxvert
end
