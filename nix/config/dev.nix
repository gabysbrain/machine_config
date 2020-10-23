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
      packrat
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
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  users.extraGroups.vboxusers.members = [ "tom" ];

  environment.systemPackages = with pkgs; [
    # git things
    git
    gitAndTools.git-annex
    gitAndTools.git-annex-remote-rclone
    gitAndTools.hub

    # haskell
    cabal-install
    ghc
    stack
    hlint
    #haskellPackages.ghc-mod
    #haskellPackages.hdevtools
    #haskellPackages.hfmt

    # purescript
    purescript
    spago

    # node 
    nodejs
    nodePackages.node2nix

    # general
    gnumake
    cmake
    jq
    entr
    bat

    # deployment
    nixops
    heroku

    chromium

    # R stuff
    rstudio-with-my-packages
    R-with-my-packages

    # Julia stuff
    vscode # for julia-vscode
    (callPackage ../pkgs/julia.nix {})
    (callPackage ../pkgs/julia-vim {})
  ];

  # needed for mysql vscode password saving
  services.gnome3.gnome-keyring.enable = true;

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
