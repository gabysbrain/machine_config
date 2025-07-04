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
  ];
}
