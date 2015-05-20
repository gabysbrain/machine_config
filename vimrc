
" 100% pure vim
set nocompatible
filetype off

" set up neobundle
set runtimepath+=~/.vim/bundle/neobundle.vim/
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'

" My plugins
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'bling/vim-airline'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'jnurmine/Zenburn'

" done with plugins
call neobundle#end()
filetype plugin indent on " required

" prompt to install missing plugins
NeoBundleCheck

" appearance
set guifont=Anonymice\ Powerline:h12
colors zenburn
set noshowmode
execute "set colorcolumn=" . join(range(81,335), ',')

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
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#bufferline#enabled = 1
if has("gui_running") " tabline takes up too much space on the console
  let g:airline#extensions#tabline#enabled = 1
endif

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

" F5 to switch buffers fast
nnoremap <F5> :buffers<CR>:buffer<Space>

" ctrl-P config
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
\}
let g:ctrlp_working_path_mode = 'r'
nmap <leader>p :CtrlP<cr>

" Buffergator config
" let g:buffergator_viewport_split_policy = 'R'

" I want my own keymappings...
"let g:buffergator_suppress_keymaps = 1

" Looper buffers
"let g:buffergator_mru_cycle_loop = 1

" Go to the previous buffer open
"nmap <leader>jj :BuffergatorMruCyclePrev<cr>

" Go to the next buffer open
"nmap <leader>kk :BuffergatorMruCycleNext<cr>

" View the entire list of buffers open
"nmap <leader>bl :BuffergatorOpen<cr>

