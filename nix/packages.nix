with import <nixpkgs> {};

let
  myvim = import ./vim.nix;
  gdrive = import ./gdrive.nix;
  zshrc = import ./zsh-config.nix;
  bower = nodePackages.bower;
  ps = haskellPackages.purescript;

in
  { inherit
      powerline-fonts
      anonymousPro

      cabal-install
      ghc
      stack
      nodejs
      ps

      gnumake

      git
      silver-searcher
      mutt
      rxvt_unicode-with-plugins
      myvim
      zshrc
      gdrive
      ;
  }
