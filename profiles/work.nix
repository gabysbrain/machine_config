{ pkgs, lib, config, ... }:

{
  programs = {
    git = {
      includes = [
        { contents = {
            user = {
              name = "Tom Torsney-Weir";
              email = "thomas.torsney-weir@boehringer-ingelheim.com";
            };
          };
          #condition = "gitdir:~/raicoon/";
        }
      ];
    };
    ssh = {
      matchBlocks = {
        "github.com" = {
          hostname = "github.com";
          identityFile = lib.mkForce "~/.ssh/id_github";
        };
      };
    };
    zsh.shellAliases = {
    };
  };

  home.packages = with pkgs; [
  ];
}
