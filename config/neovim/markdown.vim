" vim markdown config
"let g:vim_markdown_folding_disabled = 1
set conceallevel=0
let g:vim_markdown_folding_level = 1
let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0

" align markdown tables
au FileType markdown vmap <Leader><Bslash> :EasyAlign*<Bar><Enter>

