{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    (callPackage ../pkgs/agg { })

    # git things
    git
    gh

    # for nix dev
    niv
    #cachix
    nixos-shell

    # node
    nodejs

    # purescript
    purescript
    #spago

    # python
    (python313.withPackages (
      ps: with ps; [
        numpy
        pandas
      ]
    ))
    isort
    black
    mypy
    python313Packages.flake8
    ruff

    # go
    go

    # rust
    gcc
    cargo
    rustc

    # general
    gnumake
    cmake
    jq
    entr
    bat
    universal-ctags

    # deployment
    morph

    # R stuff
    #rstudio-with-my-packages
    #R-with-my-packages
  ];

}
