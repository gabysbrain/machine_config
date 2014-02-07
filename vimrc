
" 100% pure vim
:set nocompatible

" use pathogen for all the plugins
:call pathogen#infect()

" set up vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" github plugins
"Bundle 'vim-scripts/wc.vim--jcline'
Bundle 'vimwiki/vimwiki'
Bundle 'gmarik/vundle'
Bundle 'kien/ctrlp.vim'
Bundle 'majutsushi/tagbar'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'thinca/vim-localrc'
Bundle 'rosstimson/scala-vim-support'
Bundle 'vim-scripts/Vim-R-plugin'
Bundle 'rking/ag.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'SirVer/ultisnips'
Bundle 'slim-template/vim-slim.git'
Bundle 'gabysbrain/vim-multimarkdown'
Bundle 'gabysbrain/sbt-vim'
Bundle 'gabysbrain/wc.vim--jcline'

" These plugins need more exploration
"Bundle 'jcf/vim-latex'

set guifont=Anonymous\ Pro:h12

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

" fugitive key mapping
:nmap <leader>gs  :Gstatus<CR>
:nmap <leader>gc  :Gcommit<CR>
:nmap <leader>gmv :Gmove<CR>

" NerdTree config
:nmap <leader>n :NERDTreeToggle<CR>
":autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
":autocmd vimenter * if !argc() | NERDTree | endif

" VimWiki config
:let g:vimwiki_list = [{'path': '~/Dropbox/vimwiki/',
                       \ 'syntax': 'markdown', 'ext': '.md'}]


" ctags config
:set tags=./tags;

" Tagbar config
:nmap <leader>b :TagbarToggle<CR>
:let g:tagbar_type_scala = {
    \ 'ctagstype' : 'Scala',
    \ 'kinds'     : [
        \ 'p:packages:1',
        \ 'V:values',
        \ 'v:variables',
        \ 'T:types',
        \ 't:traits',
        \ 'o:objects',
        \ 'a:aclasses',
        \ 'c:classes',
        \ 'r:cclasses',
        \ 'm:methods'
    \ ]
\ }

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
:set grepprg=grep\ -nH\ $*

" Make vim recognize latex
:let g:tex_flavor='latex'

let g:UltiSnipsSnippetDirectories=["UltiSnips", "mysnippets"]
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" save/restore folds
au BufWinLeave * mkview
au BufWinEnter * silent loadview

