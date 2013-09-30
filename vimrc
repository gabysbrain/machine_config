
" 100% pure vim
:set nocompatible

" use pathogen for all the plugins
:call pathogen#infect()

" set up vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" github plugins
Bundle 'msanders/snipmate.vim'
Bundle 'tristen/vim-sparkup'
Bundle 'vim-scripts/LustyJuggler'
Bundle 'thinca/vim-localrc'
Bundle 'jcf/vim-latex'
Bundle 'vim-scripts/Processing'
Bundle 'rosstimson/scala-vim-support'
Bundle 'vim-scripts/Vim-R-plugin'
Bundle 'rking/ag.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdcommenter'
Bundle 'torsneyt/snipmate-snippets'
Bundle 'torsneyt/vim-multimarkdown'
Bundle 'torsneyt/sbt-vim'

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
:set wrap
:colorscheme proton

" use standard regex for searching
nnoremap / /\v
vnoremap / /\v

" other search options
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

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

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
:set grepprg=grep\ -nH\ $*

" Make vim recognize latex
:let g:tex_flavor='latex'

:let g:snippets_dir='~/.vim/bundle/snipmate-snippets/snippets'

