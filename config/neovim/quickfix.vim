
" quickfix and local list stuff

" quickfix list open/close next/prev
nnoremap <C-q> :call togglequickfix#ToggleQuickfix()<CR>
nnoremap <C-k> :cnext<CR>zz
nnoremap <C-j> :cprev<CR>zz

" local list things
nnoremap <localleader>q :call togglequickfix#ToggleLocation()<CR>
nnoremap <localleader>k :lnext<CR>zz
nnoremap <localleader>j :lprev<CR>zz

