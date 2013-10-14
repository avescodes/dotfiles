" Default Window size
set winwidth=85
let g:halfsize = 86
let g:fullsize = 171
set lines=50
let &columns = g:halfsize

" Font
set guifont=Essential\ PragmataPro\ for\ Powerline:h18.00
set antialias

" No audible bell
set vb

set guioptions-=T " no toolbar
set guioptions-=R " RLrl are right- and left-hand scrollbar options for the gui
set guioptions-=L
set guioptions-=r
set guioptions-=l
set guioptions+=c " Use console dialogs

if has("mac")
  set fuoptions=maxhorz,maxvert

  " Unmap Apple+S to remap to Esc, then :w<CR>
  " https://github.com/spicycode/Vimlander-2-The-Quickening/blob/master/keybindings.gui.vim
  macmenu &File.Save key=<nop>
  imap <D-s> <Esc>:w<CR>
  map <D-s> :w<CR>
end


