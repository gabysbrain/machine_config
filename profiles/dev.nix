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
in
{
  imports = [
    ../config/julia/default.nix
    ../config/vscode/default.nix
  ];

  # virtualbox
  #virtualisation.virtualbox.host.enable = true;
  #virtualisation.virtualbox.host.enableExtensionPack = true;
  #users.extraGroups.vboxusers.members = [ "tom" ];

  home.packages = with pkgs; [
    # git things
    git
    gitAndTools.git-annex
    gitAndTools.git-annex-remote-rclone
    gitAndTools.gh

    mercurial

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
    spago
    (callPackage ../pkgs/parcel {})."parcel-1.10.x"

    # python
    python3

    # go
    go

    # general
    gnumake
    cmake
    jq
    entr
    bat
    universal-ctags

    # deployment
    nixopsUnstable
    heroku

    chromium

    # R stuff
    #rstudio-with-my-packages
    #R-with-my-packages

    # Julia stuff
    (callPackage ../pkgs/julia.nix {})
    (callPackage ../pkgs/julia-vim {})

    # Problog
    (callPackage ../pkgs/problog {})
  ];

  home.file = {
    ".jira.d/config.yml".text = ''
      endpoint: https://jira.vrvis.at
      user: torsney-weir

      assignee: torsney-weir
      project: LARVAE2

      custom-commands:
        - name: mine
          help: display issues assigned to me
          script: |-
            {{jira}} list --template table --query "resolution=unresolved and assignee=currentuser() ORDER BY priority asc, created"
    '';
  };

  /*
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
  */

}
