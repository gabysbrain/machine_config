{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnumake
    texlive.combined.scheme-full
    bibtool
    pandoc
    haskellPackages.pandoc-citeproc
    haskellPackages.pandoc-crossref
    odpdown
    graphviz
  ];

  fonts.fonts = with pkgs; [
    montserrat
    raleway
    oxygenfonts
  ];
}
