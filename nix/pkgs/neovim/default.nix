{pkgs,...}:

let
  bracey-vim = (pkgs.callPackage ./bracey-vim {}).package;
  customPlugins.bracey-vim = pkgs.vimUtils.buildVimPlugin {
    name = "bracey-vim";
    src = pkgs.fetchFromGitHub {
      owner = "turbio";
      repo = "bracey.vim";
      rev = "3d234b3c5284ce4373b1de90098c58cc5cfc4dd4";
      sha256 = "10p4p2wyb1ps19b2mhzpfamcibac7n92wlxyjg667snppsm8m7lh";
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
in 
{
  programs.neovim = {
    enable = true;
    extraConfig = builtins.readFile ./vimrc;
    viAlias = true;
    vimAlias = true;
    #vimdiffAlias = true;
    plugins = [
      pkgs.vimPlugins.vim-markdown
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
      pkgs.vimPlugins.purescript-vim
      pkgs.vimPlugins.vim-obsession
      pkgs.vimPlugins.vimwiki
      pkgs.vimPlugins.vimproc
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-slime
      pkgs.vimPlugins.tender-vim
      customPlugins.bracey-vim
      pkgs.vimPlugins.vimtex
    ];
  };
}

