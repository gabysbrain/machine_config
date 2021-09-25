" lightline config
set laststatus=2
set showtabline=2
let g:lightline = { 
  \ 'colorscheme': 'nord', 
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'FugitiveHead'
  \ },
  \ 'tabline': {
  \   'left': [[ 'buffers' ]], 
  \   'right': []
  \ },
  \ 'component_expand': {
  \   'buffers': 'lightline#bufferline#buffers'
  \ },
  \ 'component_type': {
  \   'buffers': 'tabsel'
  \ }
  \ }

