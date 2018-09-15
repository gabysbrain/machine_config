{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # haskell
    cabal-install
    ghc
    stack
    hlint
    #haskellPackages.ghc-mod
    haskellPackages.hdevtools
    #haskellPackages.hfmt

    # node 
    nodejs
    nodePackages.node2nix

    # general
    gnumake
  ];
}
