# rkneufeld's vimfiles

Heavily influenced by Mislav's post "[Vim: Revisited](http://mislav.uniqpath.com/2011/12/vim-revisited/)" and accompanying repo <https://github.com/mislav/vimfiles>

## Installation

Prerequisites: ruby, git.

1. Move your existing configuration somewhere else:  
   `mv .vim* .gvim* my_backup`
2. Clone this repo into ".vim":  
   `git clone https://github.com/mislav/vimfiles ~/.vim`
3. Go into ".vim" and run "rake":  
   `cd ~/.vim && rake`

This will install "~/.vimrc" and "~/.gvimrc" symlinks that point to
files inside the ".vim" directory.

