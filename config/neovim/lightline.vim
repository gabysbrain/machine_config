" pretty icons for lightline-lsp
let g:lightline#lsp#indicator_hints = "\uf002"
let g:lightline#lsp#indicator_infos = "\uf129"
let g:lightline#lsp#indicator_warnings = "\uf071"
let g:lightline#lsp#indicator_errors = "\uf05e"
let g:lightline#lsp#indicator_ok = "\uf00c"

" lightline config
set laststatus=2
set showtabline=2
let g:lightline = { 
  \ 'colorscheme': 'nord', 
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'gitbranch', 'readonly', 'filename', 'modified' ],
  \             [ 'lspstatus' ] ],
  \   'right': [ [ 'lsp_errors', 'lsp_warnings', 'lsp_infos', 'lsp_hints', 'lsp_ok' ],
  \              [ 'lineinfo' ],
  \              [ 'fileformat', 'fileencoding', 'filetype' ] ]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'FugitiveHead',
  \ },
  \ 'tabline': {
  \   'left': [[ 'buffers' ]], 
  \   'right': []
  \ },
  \ 'component_expand': {
  \   'buffers': 'lightline#bufferline#buffers',
  \   'lsp_hints': 'lightline#lsp#hints',
  \   'lsp_infos': 'lightline#lsp#infos',
  \   'lsp_warnings': 'lightline#lsp#warnings',
  \   'lsp_errors': 'lightline#lsp#errors',
  \   'lsp_ok': 'lightline#lsp#ok'
  \ },
  \ 'component_type': {
  \   'buffers': 'tabsel',
  \   'lsp_hints': 'right',
  \   'lsp_infos': 'right',
  \   'lsp_warnings': 'warning',
  \   'lsp_errors': 'error',
  \   'lsp_ok': 'right'
  \ }
  \ }

