let g:deoplete#enable_at_startup = 1

let g:deoplete#sources = {}
let g:deoplete#sources._=['buffer', 'ultisnips', 'file', 'dictionary']
let g:deoplete#sources.clojure=['async_clj', 'file', 'ultisnips', 'dictionary']

let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'

command! AsyncCljDebug call deoplete#custom#set('async_clj', 'debug_enabled', 1) | call deoplete#enable_logging("DEBUG", "/tmp/deopletelog")
