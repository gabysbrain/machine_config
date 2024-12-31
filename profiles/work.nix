{ pkgs, config, ... }:

{
  programs = {
    git = {
      includes = [
        { contents = {
            user = {
              name = "Tom Torsney-Weir";
              email = "soy9a@s-mxs.net";
            };
          };
          #condition = "gitdir:~/raicoon/";
        }
      ];
    };
    zsh.shellAliases = {
    };
    zsh.initExtra = ''
      # sshing around
      function essh() {
        conn="soy9a@$1@pathpsmpp01.eb.lan.at"
        ssh $conn ''${@:2}
      }
    '';
    #zsh.initExtra = ''
      ## work envvars for services
      #source "${config.homeage.file.raicoon-envvars.path}"
    #'';
  };

  home.packages = with pkgs; [
    bump2version
  ];

  home.file."${config.xdg.configHome}/pip/pip.conf" = { source = ../config/pip.conf; };
}
