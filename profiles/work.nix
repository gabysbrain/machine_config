{ pkgs, config, ... }:

{
  homeage.file.raicoon-envvars.source = ../secrets/raicoon-envvars.age;
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
}
