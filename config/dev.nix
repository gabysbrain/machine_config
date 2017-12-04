{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cabal-install
    ghc
    stack
    nodejs
    gnumake
    texlive.combined.scheme-full
    bibtool
    pandoc
  ];
}
