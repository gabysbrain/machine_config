pkgs: {
  allowUnfree = true;
  allowBroken = true;

  packageOverrides = pkgs : with pkgs; rec {
    my_vim = vim_configurable.customize {
      name = "vim";

      vimrcConfig.customRC = ''
        " 100% pure vim
        set nocompatible
        filetype off

        filetype plugin indent on " required
        
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
        
        " supertab for haskell
        " http://www.stephendiehl.com/posts/vim_2016.html
        let g:SuperTabDefaultCompletionType = '<c-x><c-o>'
        
        if has("gui_running")
          imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
        else " no gui
          if has("unix")
            inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
          endif
        endif

        let g:haskellmode_completion_ghc = 1
        autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

        " tablular
        let g:haskell_tabular = 1

        vmap a= :Tabularize /=<CR>
        vmap a; :Tabularize /::<CR>
        vmap a- :Tabularize /-><CR>

        " vim markdown config
        let g:vim_markdown_folding_disabled = 1
      '';

      vimrcConfig.vam.knownPlugins = pkgs.vimPlugins;
      vimrcConfig.vam.pluginDictionaries = [
        { names = [
	    "Syntastic"
	    "Tagbar"
	    "ctrlp"
	    "vim-addon-nix"
      "zenburn"
	]; }
      ];
#        " My plugins
#        NeoBundle 'godlygeek/tabular'
#        "NeoBundle 'plasticboy/vim-markdown'
#        NeoBundle 'vim-pandoc/vim-criticmarkup'
#        NeoBundle 'chrisbra/csv.vim'
#        NeoBundle 'ervandew/supertab'
#        NeoBundle 'kien/ctrlp.vim'
#        NeoBundle 'mattn/emmet-vim'
#        NeoBundle 'guicolorscheme.vim'
#        NeoBundle 'othree/html5.vim'
#        NeoBundle 'scrooloose/nerdcommenter'
#        NeoBundle 'scrooloose/nerdtree'
#        NeoBundle 'raichoo/purescript-vim'
#        NeoBundle 'scrooloose/syntastic'
#        NeoBundle 'majutsushi/tagbar'
#        NeoBundle 'vim-airline/vim-airline'
#        NeoBundle 'vim-airline/vim-airline-themes'
#        NeoBundle 'jeetsukumaran/vim-buffergator'
#        NeoBundle 'tpope/vim-fugitive'
#        NeoBundle 'airblade/vim-gitgutter'
#        NeoBundle 'tfnico/vim-gradle'
#        NeoBundle 'elzr/vim-json'
#        NeoBundle 'derekwyatt/vim-scala'
#        NeoBundle 'jnurmine/Zenburn'
#        NeoBundle 'godlygeek/tabular'
#        NeoBundle 'Shougo/vimproc.vim', {
#        \ 'build' : {
#        \     'windows' : 'tools\\update-dll-mingw',
#        \     'cygwin' : 'make -f make_cygwin.mak',
#        \     'mac' : 'make -f make_mac.mak',
#        \     'linux' : 'make',
#        \     'unix' : 'gmake',
#        \    },
#        \ }

#        " Haskell stuff
#        NeoBundle 'eagletmt/ghcmod-vim'
#        NeoBundle 'eagletmt/neco-ghc'

    };
    all = pkgs.buildEnv {
      name = "all";
      paths = [
        my_vim
        git
        dropbox
        oh-my-zsh
      ];
    };
  };
}
