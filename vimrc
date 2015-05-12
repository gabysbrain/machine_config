
" 100% pure vim
set nocompatible
filetype off

" set up neobundle
set runtimepath+=~/.vim/bundle/neobundle.vim/
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'

" My plugins
NeoBundle 'othree/html5.vim'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'bling/vim-airline'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'weynhamz/vim-plugin-minibufexpl'
NeoBundle 'jnurmine/Zenburn'

" done with plugins
call neobundle#end()
filetype plugin indent on " required

" prompt to install missing plugins
NeoBundleCheck

" appearance
"set guifont=Anonymous\ Pro:h12
set guifont=Anonymice\ Powerline:h12
colors zenburn

" quickly edit/reload the vimrc file
" from http://nvie.com/posts/how-i-boosted-my-vim/
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" keep backup files and stuff outside of the working dir
set backupdir=/var/tmp/
set directory=/var/tmp/

" always show airline
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline_detect_whitespace=0
let g:airline_enable_bufferline=1

" NerdTree config
nmap <leader>n :NERDTreeToggle<CR>
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q

" disable the f1 help key
nmap <F1> <nop>

set expandtab
set tabstop=2
set shiftwidth=2
set textwidth=78
"set formatoptions+=t
"set wrap linebreak nolist " softwrap lines
set nowrap

set number

