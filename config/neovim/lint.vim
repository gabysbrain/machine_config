
lua << EOF
require('lint').linters_by_ft = {
  markdown = {'vale',},
  javascript = {'eslint',},
}
EOF

au BufWritePost <buffer> lua require('lint').try_lint()

