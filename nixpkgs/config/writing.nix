{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnumake
    texlive.combined.scheme-full
    bibtool
    pandoc
    haskellPackages.pandoc-citeproc
    (import ../pkgs/remarker/default.nix {}).remarker
  ];
}
