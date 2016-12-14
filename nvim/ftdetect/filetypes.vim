
" Treat JSON files like JavaScript
augroup filetypes
  autocmd!
  autocmd BufRead,BufNewFile {Vagrantfile,Guardfile,Gemfile,Rakefile,Capfile,*.rake,config.ru} set filetype=ruby
  autocmd BufRead,BufNewFile {COMMIT_EDITMSG}                                                  set filetype=gitcommit
  autocmd BufRead,BufNewFile {*.json}                                                          set filetype=javascript
  autocmd BufRead,BufNewFile {*.clj,*.cljc,*.cljs,*.hl,*.edn,*.cljx,*.boot}                    setlocal filetype=clojure
  autocmd BufRead,BufNewFile {*.gradle}                                                        setlocal filetype=groovy
  autocmd FileType           python                                                            setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4
  autocmd FileType           rust                                                              setlocal tabstop=4 expandtab shiftwidth=4 softtabstop=4
augroup END



