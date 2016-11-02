pkgs: {
  allowUnfree = true;
  allowBroken = true;

  packageOverrides = pkgs : with pkgs; rec {
    all = pkgs.buildEnv {
      name = "all";
      paths = [
        powerline-fonts
        git
        oh-my-zsh
        anonymousPro
        cabal-install
        ghc
        gnumake
        nodejs
        nodePackages.bower
        nodePackages.bower2nix
        rxvt_unicode-with-plugins
        haskellPackages.purescript
        stack
      ];
    };
  };
}
