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
    nvd
    nixos-shell

    # node
    nodejs

    # python
    (python314.withPackages (
      ps: with ps; [
        mypy
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
