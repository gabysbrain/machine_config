{pkgs,...}:

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
      sha256 = "0kavcpmn066lyajf2zhci5ki3hjm6xv5dqkbh5fknzkl6as4fi4s";
    };
  };
in 
{
  programs.neovim = {
    enable = true;
    extraConfig = ''
      ${builtins.readFile ./vimrc}

      " universal packages
      ${builtins.readFile ./lightline.vim}
      ${builtins.readFile ./nerdtree.vim}
      ${builtins.readFile ./slime.vim}

      " languages
      ${builtins.readFile ./html.vim}
      ${builtins.readFile ./json.vim}
      ${builtins.readFile ./markdown.vim}

      ${builtins.readFile ./tex.vim}

      ${builtins.readFile ./haskell.vim}
      ${builtins.readFile ./python.vim}
    '';
    viAlias = true;
    vimAlias = true;
    #vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      # themes
      nord-vim

      polyglot
      customPlugins.vim-criticmarkup
      pkgs.vimPlugins.Tagbar
      pkgs.vimPlugins.Tabular
      pkgs.vimPlugins.vim-buffergator
      pkgs.vimPlugins.The_NERD_Commenter
      pkgs.vimPlugins.The_NERD_tree
      pkgs.vimPlugins.fugitive
      pkgs.vimPlugins.julia-vim
      customPlugins.nvim-jqx # json
      customPlugins.vim-convert-color-to
      pkgs.vimPlugins.vim-gitgutter
      pkgs.vimPlugins.Supertab
      pkgs.vimPlugins.ctrlp
      pkgs.vimPlugins.vim-addon-nix
      pkgs.vimPlugins.lightline-vim
      pkgs.vimPlugins.lightline-bufferline
      pkgs.vimPlugins.vim-obsession
      pkgs.vimPlugins.vimproc
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-slime
      pkgs.vimPlugins.vimtex
      pkgs.vimPlugins.bracey-vim
    ];
  };
  home.packages = [
    pkgs.neovim-remote
  ];

  nixpkgs.overlays = [
    (import ../../overlays/neovim.nix)
  ];
}

