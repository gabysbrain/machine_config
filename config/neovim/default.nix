{pkgs, lib, ...}:

let
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
  customPlugins.nvim-jqx = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-jqx";
    src = pkgs.fetchFromGitHub {
      owner = "gennaro-tedesco";
      repo = "nvim-jqx";
      rev = "master";
      sha256 = "1dh4yb6rr593nx8kbhskpbb50l211b9z47rvhxd1n07d31bc0lmc";
    };
  };
in 
{
  programs.neovim = {
    enable = true;
    extraConfig = ''
      ${builtins.readFile ./vimrc}

      " universal packages
      lua << EOF
      ${builtins.readFile ./lualine.lua}
      ${builtins.readFile ./barbar.lua}
      EOF
      ${builtins.readFile ./nerdtree.vim}
      ${builtins.readFile ./slime.vim}
      ${builtins.readFile ./telescope.vim}

      lua << EOF
      ${builtins.readFile ./lsp.lua}
      EOF

      " languages
      ${builtins.readFile ./html.vim}
      ${builtins.readFile ./json.vim}
      ${builtins.readFile ./markdown.vim}

      ${builtins.readFile ./tex.vim}

      ${builtins.readFile ./haskell.vim}
      ${builtins.readFile ./python.vim}

      runtime scratch.vim
      lua require('scratch')
    '';
    viAlias = true;
    vimAlias = true;
    #vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      # themes
      nord-vim

      polyglot
      customPlugins.vim-criticmarkup
      lualine-nvim
      barbar-nvim
      pkgs.vimPlugins.Tagbar
      pkgs.vimPlugins.Tabular
      pkgs.vimPlugins.vim-buffergator
      pkgs.vimPlugins.The_NERD_Commenter
      pkgs.vimPlugins.The_NERD_tree
      pkgs.vimPlugins.telescope-nvim
      pkgs.vimPlugins.nvim-lspconfig
      pkgs.vimPlugins.fugitive
      pkgs.vimPlugins.vim-lawrencium
      pkgs.vimPlugins.julia-vim
      customPlugins.nvim-jqx # json
      customPlugins.vim-convert-color-to
      pkgs.vimPlugins.vim-signify
      pkgs.vimPlugins.Supertab
      pkgs.vimPlugins.vim-addon-nix
      pkgs.vimPlugins.vim-obsession
      pkgs.vimPlugins.vimproc
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-slime
      pkgs.vimPlugins.vimtex
      pkgs.vimPlugins.typescript-vim
      pkgs.vimPlugins.vim-jsx-typescript
      pkgs.vimPlugins.bracey-vim
      pkgs.vimPlugins.lsp-status-nvim
    ];
  };
  home.packages = with pkgs; [
    neovim-remote

    # language servers
    gopls
    nodePackages.typescript-language-server
    # julia ts is installed as julia module
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

  nixpkgs.overlays = [
    (import ../../overlays/neovim.nix)
  ];
}

