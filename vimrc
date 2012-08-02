
" 100% pure vim
:set nocompatible

" use pathogen for all the plugins
:call pathogen#infect()

" turn on syntax highlighting
:syntax enable
:filetype plugin on

" quickly edit/reload the vimrc file
" from http://nvie.com/posts/how-i-boosted-my-vim/
:nmap <silent> <leader>ev :e $MYVIMRC<CR>
:nmap <silent> <leader>sv :so $MYVIMRC<CR>

:set ai
:set tabstop=4
:set shiftwidth=4
:set expandtab
:set showmode
:set textwidth=78
:set formatoptions-=t
:colorscheme proton

" disable the f1 help key
:nmap <F1> <nop>

" hide buffers instead of closing them
:set hidden

:set backupdir=/var/tmp/
:set directory=/var/tmp/

" setup for vim r plugin
:let vimrplugin_term = "screen"
:let vimrplugin_screenplugin = 0
:let vimrplugin_conqueplugin = 0
:let vimrplugin_applescript = 1
:let vimrplugin_underscore = 0

" marked preview screen
:nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>

