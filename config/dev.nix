{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cabal-install
    ghc
    stack
    nodejs
    nodePackages.node2nix
    gnumake
  ];
}
