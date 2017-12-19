{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cabal-install
    ghc
    stack
    nodejs
    node2nix
    gnumake
  ];
}
