{ config, pkgs, ... }:

    # TODO: this hdf5r thing should be an override and use the official depends
let #my-hdf5r = pkgs.rPackages.hdf5r.override {
      #depends = [pkgs.hdf5_1_8 pkgs.rPackages.R6 pkgs.rPackages.bit64]; #++ pkgs.rPackages.hdf5r.depends;
    #};
    rpkgs =  rpkg: with rpkg; [
      bench
      devtools
      dplyr
      geometry
      ggplot2
      gridExtra
      #my-hdf5r
      hdf5r
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
    juliaPkg = pkgs.unstable.julia-bin;
in
{
  imports = [
    ../config/julia/default.nix
  ];

  home.packages = with pkgs; [
    # git things
    git
    gitAndTools.git-annex
    gitAndTools.git-annex-remote-rclone
    gitAndTools.gh
    glab

    circleci-cli
    go-jira

    # for nix dev
    niv
    cachix
    nixos-shell

    # haskell
    cabal-install
    ghc
    stack
    hlint
    #haskellPackages.ghc-mod
    #haskellPackages.hdevtools
    #haskellPackages.hfmt

    # node 
    nodejs
    nodePackages.node2nix

    # purescript
    purescript
    #spago

    # python
    (python311.withPackages (ps: with ps; [ numpy pandas ]))
    isort
    black
    mypy
    python311Packages.flake8
    ruff

    # go
    go

    # rust
    gcc
    cargo
    rustc

    # databases
    dbeaver

    # general
    gnumake
    cmake
    jq
    entr
    bat
    universal-ctags

    # deployment
    morph
    heroku

    chromium

    # R stuff
    #rstudio-with-my-packages
    #R-with-my-packages

    # Julia stuff
    juliaPkg

    # Problog
    (callPackage ../pkgs/problog {})
  ];

  nixpkgs.overlays = [
    /*
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
    */
  ];

}
