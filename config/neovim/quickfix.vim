
" quickfix and local list stuff

" quickfix list open/close next/prev
nnoremap <C-q> :call togglequickfix#ToggleQuickfix()<CR>
nnoremap <C-k> :cnext<CR>zz
nnoremap <C-j> :cprev<CR>zz

" local list things
nnoremap <leader>q :call togglequickfix#ToggleLocation()<CR>
nnoremap <leader>k :lnext<CR>zz
nnoremap <leader>j :lprev<CR>zz

