{pkgs, lib, ...}:

let
  unstable = pkgs.unstable;

  customPlugins.vim-criticmarkup = pkgs.vimUtils.buildVimPlugin {
    name = "vim-criticmarkup";
    src = pkgs.fetchFromGitHub {
      owner = "vim-pandoc";
      repo = "vim-criticmarkup";
      rev = "d15dc134eb177a170c79f6377f81eb02a9d20b02";
      sha256 = "1la51jyjprjp7cvm6mfjs5365m2kfn02cqh599j8ciylr5arjcyq";
    };
  };
  customPlugins.vim-convert-color-to = pkgs.vimUtils.buildVimPlugin {
    name = "vim-convert-color-to";
    src = pkgs.fetchFromGitHub {
      owner = "amadeus";
      repo = "vim-convert-color-to";
      rev = "5eb519e33e697606dcace57009e7308761261b46";
      sha256 = "0z6n82zdm219q1bblmis1473ciq31v3dwhmwkl4sld8ahff0cqc3";
    };
  };
  customPlugins.vim-toggle-quickfix = pkgs.vimUtils.buildVimPlugin {
    name = "vim-toggle-quickfix";
    src = pkgs.fetchFromGitHub {
      owner = "drmingdrmer";
      repo = "vim-toggle-quickfix";
      rev = "04ca155dc8d8bd6b9e882b916ba827fa80f7b576";
      sha256 = "1jd0j8n1h964yg2lbgzdhg4c2j9a71h07qxw3zpbcxv1ack8v0ib";
    };
  };
  customPlugins.telescope-bibtex = pkgs.vimUtils.buildVimPlugin {
    name = "telescope-bibtex";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-telescope";
      repo = "telescope-bibtex.nvim";
      rev = "cd2640e74657f154b50e2278a279ad02ba523e97";
      sha256 = "0AOEhmeeXbNc2Ge+J+/o6OBUEudyKv5HmZgpcqWu8As=";
    };
  };
in 
{
  programs.neovim = {
    enable = true;
    #package = pkgs.unstable.neovim-unwrapped;
    package = pkgs.neovim-unwrapped;
    extraConfig = ''
      ${builtins.readFile ./vimrc}

      " universal packages
      lua << EOF
      ${builtins.readFile ./lualine.lua}
      ${builtins.readFile ./barbar.lua}
      ${builtins.readFile ./todos.lua}
      EOF
      ${builtins.readFile ./nerdtree.vim}
      ${builtins.readFile ./slime.vim}
      ${builtins.readFile ./telescope.vim}
      ${builtins.readFile ./quickfix.vim}
      ${builtins.readFile ./vimdiff.vim}

      lua << EOF
      ${builtins.readFile ./completion.lua}
      ${builtins.readFile ./treesitter.lua}
      ${builtins.readFile ./lsp.lua}
      ${builtins.readFile ./conform.lua}
      EOF

      " languages
      ${builtins.readFile ./html.vim}
      ${builtins.readFile ./json.vim}
      ${builtins.readFile ./markdown.vim}

      ${builtins.readFile ./tex.vim}

      ${builtins.readFile ./haskell.vim}
      ${builtins.readFile ./python.vim}

      " journaling
      lua << EOF
      ${builtins.readFile ./zk.lua}
      EOF

      runtime scratch.vim
      lua require('scratch')
    '';
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      # themes
      nord-vim

      # completion
      blink-cmp
      #luasnip

      # can also use nvim-treesitter.withAllGrammars and be done
      (nvim-treesitter.withPlugins (
        plugins: with plugins; [
          bash
          nix
          python
          go
          haskell
          json
          markdown
          lua
          sql
          julia
          vim
        ]
      ))
      polyglot
      customPlugins.vim-criticmarkup
      lualine-nvim
      barbar-nvim
      Tagbar
      Tabular
      vim-buffergator
      The_NERD_Commenter
      The_NERD_tree
      telescope-nvim
      telescope-fzf-native-nvim
      customPlugins.telescope-bibtex
      nvim-lspconfig
      fugitive
      todo-comments-nvim
      julia-vim
      vim-go
      nvim-jqx # json
      customPlugins.vim-convert-color-to
      customPlugins.vim-toggle-quickfix
      vim-signify
      Supertab
      vim-addon-nix
      vim-obsession
      vim-unimpaired
      vimproc
      vim-easy-align
      vim-slime
      vimtex
      typescript-vim
      vim-jsx-typescript
      bracey-vim
      lsp-progress-nvim
      conform-nvim

      telekasten-nvim
      calendar-vim
    ];
  };
  home.packages = with pkgs; [
    neovim-remote

    # language servers
    java-language-server
    gopls
    lua-language-server
    nodePackages.typescript-language-server
    nodePackages.eslint
    nodePackages.vscode-langservers-extracted
    nil # nix
    haskell-language-server
    rust-analyzer
    texlab
    pyright # python 1
    ruff # python 2
    # julia ts is installed as julia module

    ripgrep # telekasten.nvim uses this
  ];
  home.activation = {
    neovimScratchFiles = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if [ ! -f $HOME/.config/nvim/scratch.vim ]; then
        $DRY_RUN_CMD touch $HOME/.config/nvim/scratch.vim
      fi
      if [ ! -f $HOME/.config/nvim/lua/scratch.lua ]; then
        $DRY_RUN_CMD mkdir -p $HOME/.config/nvim/lua
        $DRY_RUN_CMD touch $HOME/.config/nvim/lua/scratch.lua
      fi
    '';
  };
}

