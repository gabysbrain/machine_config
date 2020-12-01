{pkgs,...}:

let
  bracey-vim = (pkgs.callPackage ./bracey-vim {}).package;
  customPlugins.bracey-vim = pkgs.vimUtils.buildVimPlugin {
    name = "bracey-vim";
    src = pkgs.fetchFromGitHub {
      owner = "turbio";
      repo = "bracey.vim";
      rev = "912997c5d0dbf42feaba5a4ee5527159a41ff82f";
      sha256 = "1swsgid5xhgcvw4rqmalcjmkhr7bv00ikfj7b7xsyw0ps9mmcjbw";
    };
    #buildInputs = [
      #nodePkgs.package
    #];
    # FIXME: this is a hack because bracey wants node_modules in its server directory
    postInstall = ''
      ln -s ${bracey-vim}/lib/node_modules/bracey/node_modules $out/node_modules
    '';
  };
  customPlugins.lightline-bufferline = pkgs.vimUtils.buildVimPlugin {
    name = "lightline-bufferline";
    src = pkgs.fetchFromGitHub {
      owner = "mengelbrecht";
      repo = "lightline-bufferline";
      rev = "17683bc5802de7f295f2583a15461e2bc662f98b";
      sha256 = "1rlamxwk2gm9pyxl9vym9w6rhgimzqa2hjghy3qdqwvif6w8ir6l";
    };
  };
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
      customPlugins.lightline-bufferline
      pkgs.vimPlugins.vim-obsession
      pkgs.vimPlugins.vimwiki
      pkgs.vimPlugins.vimproc
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-slime
      pkgs.vimPlugins.vimtex
      customPlugins.bracey-vim
      pkgs.vimPlugins.vim-hindent
    ];
  };
  home.packages = [
    pkgs.neovim-remote
  ];
}

