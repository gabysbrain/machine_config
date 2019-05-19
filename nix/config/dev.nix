{ config, pkgs, ... }:

let rpkgs =  rpkg: with rpkg; [
      bench
      devtools
      dplyr
      geometry
      ggplot2
      gridExtra
      GPareto
      mco
      profvis
      randtoolbox
      rjson
      rgl
      roxygen2
      #tensorflow
      testthat
      tidyr
      usethis
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

    chromium

    # R stuff
    rstudio-with-my-packages
    R-with-my-packages
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
