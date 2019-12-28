{ config, pkgs, ... }:

    # TODO: this hdf5r thing should be an override and use the official depends
let my-hdf5r = pkgs.rPackages.hdf5r.override {
      depends = [pkgs.hdf5_1_8 pkgs.rPackages.R6 pkgs.rPackages.bit64]; #++ pkgs.rPackages.hdf5r.depends;
    };
    rpkgs =  rpkg: with rpkg; [
      bench
      devtools
      dplyr
      geometry
      ggplot2
      gridExtra
      my-hdf5r
      #GPareto
      mco
      pracma
      profvis
      randtoolbox
      rjson
      rgl
      roxygen2
      #tensorflow
      testthat
      tidyr
      usethis
      xml2
    ];
in
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
    jq
    entr
    bat

    chromium

    # R stuff
    rstudio-with-my-packages
    R-with-my-packages

    # Julia stuff
    atom # for juno
    (callPackage ../pkgs/julia.nix {})
  ];

  nixpkgs.overlays = [
    (
      self: super: {
        R-with-my-packages = super.rWrapper.override { 
          packages = rpkgs super.rPackages;
        };
        rstudio-with-my-packages = super.rstudioWrapper.override {
          packages = rpkgs super.rPackages;
        };
      }
    )
  ];

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
}
