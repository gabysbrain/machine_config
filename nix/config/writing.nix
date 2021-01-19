{ config, pkgs, ... }:

let
  pancritic = pkgs.callPackage ../pkgs/pancritic { };
in
{
  environment.systemPackages = with pkgs; [
    gnumake
    texlive.combined.scheme-full
    bibtool
    pandoc
    haskellPackages.pandoc-citeproc
    haskellPackages.pandoc-crossref
    pancritic
    odpdown
    pdftk
    graphviz
  ];

  fonts.fonts = with pkgs; [
    montserrat
    raleway
    oxygenfonts
  ];
}
