{ pkgs, ... }:

{
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
  };

  home.packages = with pkgs; [
    slack
  ];
}
