{ config, pkgs, ... }:

let
  pancritic = pkgs.callPackage ../pkgs/pancritic { };
in
{
  home.packages = with pkgs; [
    gnumake
    texlive.combined.scheme-full
    bibtool
    pandoc
    #haskellPackages.pandoc-citeproc
    haskellPackages.pandoc-crossref
    pancritic
    #odpdown
    pdftk
    graphviz
    unstable.zotero # TODO: move to a research profile (with latex and stuff from writing)
  ];

  /*
  fonts.fonts = with pkgs; [
    montserrat
    raleway
    oxygenfonts
    google-fonts
  ];
  */
}
