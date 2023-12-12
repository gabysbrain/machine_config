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
    k9s = {
      enable = true;
      skin = "nord";
      plugin = {
        # Defines a plugin to provide a `ctrl-l` shortcut to
        # tail the logs while in pod view.
        fred = {
          shortCut = "Ctrl-L";
          description = "Pod logs";
          scopes = [ "po" ];
          command = "kubectl";
          background = false;
          args = [
            "logs"
            "-f"
            "$NAME"
            "-n"
            "$NAMESPACE"
            "--context"
            "$CLUSTER"
          ];
        };
      };
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
