
lua << EOF
require('lint').linters_by_ft = {
  javascript = {'eslint',},
  nix = {'nix',},
}
EOF

au BufWritePost <buffer> lua require('lint').try_lint()

" for python run black and isort
au BufWritePost *.py silent! !black <afile>
au BufWritePost *.py silent! !isort <afile>

