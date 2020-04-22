{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

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
pkgs.vim_configurable.customize {
  name = "vim";

  vimrcConfig.customRC = builtins.readFile ./vimrc;

  vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
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
      "lightline-vim"
      "lightline-bufferline"
      "vim-obsession"
      "vimwiki"
      "vimproc"
      "haskell-vim"
      "vim-easy-align"
      "vim-slime"
      "tender-vim"
      "bracey-vim"
    ]; }
  ];
}


