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
  };

  home.packages = with pkgs; [
    bump2version
  ];
  home.file."${config.xdg.configHome}/pip/pip.conf" = { source = ../config/pip.conf; };
}
