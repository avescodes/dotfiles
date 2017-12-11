augroup kotlin_neoterm
  au!
  au FileType kotlin
        \ if executable('kotlinc-jvm') |
        \   call neoterm#repl#set('kotlinc-jvm') |
        \ end
augroup END
