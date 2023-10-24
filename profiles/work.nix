{ pkgs, config, ... }:

{
  homeage.file.raicoon-envvars.source = ../secrets/raicoon-envvars.age;
  programs = {
    git = {
      includes = [
        { contents = {
            user = {
              name = "Tom Torsney-Weir";
              email = "t.torsney@raicoon.com";
            };
          };
          condition = "gitdir:~/raicoon/";
        }
      ];
    };
    zsh.initExtra = ''
      # work envvars for services
      source "${config.homeage.file.raicoon-envvars.path}"
    '';
  };

  home.packages = with pkgs; [
    slack
    bump2version
    dbeaver
    magic-wormhole
    iredis
    rclone
    teams-for-linux
    thunderbird

    # kubernetes
    openlens
    kubectl
  ];
}
