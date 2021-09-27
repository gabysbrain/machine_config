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
in 
{
  programs.neovim = {
    enable = true;
    extraConfig = builtins.readFile ./vimrc;
    viAlias = true;
    vimAlias = true;
    #vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      # themes
      nord-vim

      polyglot
      pkgs.vimPlugins.ale
      customPlugins.vim-criticmarkup
      pkgs.vimPlugins.Tagbar
      pkgs.vimPlugins.Tabular
      pkgs.vimPlugins.vim-buffergator
      pkgs.vimPlugins.The_NERD_Commenter
      pkgs.vimPlugins.The_NERD_tree
      pkgs.vimPlugins.fugitive
      pkgs.vimPlugins.julia-vim
      pkgs.vimPlugins.vim-gitgutter
      pkgs.vimPlugins.Supertab
      pkgs.vimPlugins.ctrlp
      pkgs.vimPlugins.vim-addon-nix
      pkgs.vimPlugins.lightline-vim
      pkgs.vimPlugins.lightline-bufferline
      pkgs.vimPlugins.vim-obsession
      pkgs.vimPlugins.vimwiki
      pkgs.vimPlugins.vimproc
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-slime
      pkgs.vimPlugins.vimtex
      pkgs.vimPlugins.bracey-vim
      pkgs.vimPlugins.vim-hindent
    ];
  };
  home.packages = [
    pkgs.neovim-remote
  ];

  nixpkgs.overlays = [
    (import ../../overlays/neovim.nix)
  ];
}

