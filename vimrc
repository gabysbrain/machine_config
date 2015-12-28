
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
NeoBundle 'guicolorscheme.vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'bling/vim-airline'
NeoBundle 'jeetsukumaran/vim-buffergator'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tfnico/vim-gradle'
NeoBundle 'derekwyatt/vim-scala'
NeoBundle 'jnurmine/Zenburn'
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }

" Haskell stuff
NeoBundle 'neovimhaskell/haskell-vim'
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'bitc/vim-hdevtools'
NeoBundle 'eagletmt/neco-ghc'
NeoBundle 'Twinside/vim-hoogle'

" done with plugins
call neobundle#end()
filetype plugin indent on " required

" prompt to install missing plugins
NeoBundleCheck

" appearance
set guifont=Anonymice\ Powerline:h12
colors zenburn
set noshowmode
set cursorline " color current line
syntax on " syntax highlighting
" set the gutter color
" TODO: look into matchadd though to only highlight extended lines
execute "set colorcolumn=" . join(range(79,335), ',') 

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
set wrap linebreak nolist " softwrap lines

" show numbers in margin
set number

" F5 to switch buffers fast
nnoremap <F5> :buffers<CR>:buffer<Space>

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

" hdevtools config
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsInfo<CR>

" syntastic config
let g:syntastic_always_populate_loc_list=1
let g:syntastic_disabled_filetypes=['haskell']

" Marked integration
nmap <leader>m :silent !open -a 'Marked 2.app' '%:p'<CR>

