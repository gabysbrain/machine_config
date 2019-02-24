{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnumake
    texlive.combined.scheme-full
    bibtool
    pandoc
    haskellPackages.pandoc-citeproc
    odpdown

    # fonts
    montserrat
    raleway
    oxygenfonts
    (import ../pkgs/remarker/default.nix {}).remarker
  ];
}
