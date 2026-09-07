{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    (callPackage ../pkgs/agg { })

    # git things
    git
    gh
    forgejo-cli

    # for nix dev
    niv
    #cachix
    nixos-shell

    # node
    nodejs

    # python
    (python313.withPackages (
      ps: with ps; [
        mypy
        ruff
      ]
    ))

    # general
    gnumake
    jq
    entr
    bat

    # R stuff
    #rstudio-with-my-packages
    #R-with-my-packages
  ];

}
