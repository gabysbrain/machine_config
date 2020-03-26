{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.vim_configurable.customize {
  name = "vim";

  vimrcConfig.customRC = ''
    " 100% pure vim
    set nocompatible
    filetype off

    filetype plugin indent on " required
    
    " appearance
    set guifont=Anonymice\ Nerd\ Font\ 11
    colors zenburn
    set noshowmode
    set cursorline " color current line
    syntax on " syntax highlighting
    " set the gutter color
    " TODO: look into matchadd though to only highlight extended lines
    execute "set colorcolumn=" . join(range(79,335), ',') 

    " keep backup files and stuff outside of the working dir
    set backupdir=/var/tmp/
    set directory=/var/tmp/

    " always show airline
    set laststatus=2
    let g:airline_powerline_fonts = 1
    let g:airline#extensions#whitespace#enabled = 0
    let g:airline#extensions#bufferline#enabled = 1
    let g:airline#extensions#ale#enabled = 1
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
    set autoindent

    " show numbers in margin
    set number

    " F5 to switch buffers fast
    nnoremap <F5> :buffers<CR>:buffer<Space>

    " syntastic config
    let g:syntastic_always_populate_loc_list=1
    let g:syntastic_disabled_filetypes=['haskell']

    " ctags
    nnoremap <silent> <Leader>, :TagbarToggle<CR>
    nnoremap <leader>. :CtrlPTag<cr>
    " haskell ctags
    let g:tagbar_type_haskell = {
        \ 'ctagsbin'  : 'hasktags',
        \ 'ctagsargs' : '-x -c -o-',
        \ 'kinds'     : [
            \  'm:modules:0:1',
            \  'd:data: 0:1',
            \  'd_gadt: data gadt:0:1',
            \  't:type names:0:1',
            \  'nt:new types:0:1',
            \  'c:classes:0:1',
            \  'cons:constructors:1:1',
            \  'c_gadt:constructor gadt:1:1',
            \  'c_a:constructor accessors:1:1',
            \  'ft:function types:1:1',
            \  'fi:function implementations:0:1',
            \  'o:others:0:1'
        \ ],
        \ 'sro'        : '.',
        \ 'kind2scope' : {
            \ 'm' : 'module',
            \ 'c' : 'class',
            \ 'd' : 'data',
            \ 't' : 'type'
        \ },
        \ 'scope2kind' : {
            \ 'module' : 'm',
            \ 'class'  : 'c',
            \ 'data'   : 'd',
            \ 'type'   : 't'
        \ }
    \ }
    
    " vim-json config
    let g:vim_json_syntax_conceal = 0
    
    " tablular
    vmap a= :Tabularize /=<CR>
    vmap a; :Tabularize /::<CR>
    vmap a- :Tabularize /-><CR>

    " vim markdown config
    let g:vim_markdown_folding_disabled = 1

    " align markdown tables
    au FileType markdown vmap <Leader><Bslash> :EasyAlign*<Bar><Enter>

    " haskell stuff
    let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
    let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
    let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
    let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
    let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
    let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
    let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

    " vim wiki config
    let g:vimwiki_global_ext = 0
    let g:vimwiki_list = [{'path': '~/Sync/wiki/',
                          \ 'syntax': 'markdown', 'ext': '.md'}]
  '';

  vimrcConfig.vam.knownPlugins = pkgs.vimPlugins;
  vimrcConfig.vam.pluginDictionaries = [
    { names = [
      #"Syntastic"
      "vim-markdown"
      "Tagbar"
      "Tabular"
      "vim-buffergator"
      "The_NERD_Commenter"
      "The_NERD_tree"
      "fugitive"
      "julia-vim"
      "vim-gitgutter"
      "Supertab"
      "ctrlp"
      "vim-addon-nix"
      "zenburn"
      "vim-airline"
      "vim-airline-themes"
      "vim-obsession"
      "vimwiki"
      "vimproc"
      "haskell-vim"
      "vim-easy-align"
      "vim-stylish-haskell"
    ]; }
  ];
}
#    " My plugins
#    NeoBundle 'vim-pandoc/vim-criticmarkup'
#    NeoBundle 'chrisbra/csv.vim'


