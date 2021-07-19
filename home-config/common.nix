{ pkgs, ... }:
{
  imports = [
    ../config/zsh.nix
    ../config/tmux.nix
    ../config/lf/default.nix

    ../config/neovim/default.nix
  ];
  programs = {
    git = {
      enable = true;
      lfs.enable = true;
      userName = "Thomas Torsney-Weir";
      userEmail = "torsneyt@gmail.com";
      extraConfig = {
        credential = {
          helper = "store";
        };
      };
    };
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        # TODO need to run `bat cache --build` after custom theme
        theme = "Nord";
      };
    };
    broot = {
      enable = true;
      enableZshIntegration = true;
    };
  };
  home.sessionVariables = {
    EDITOR = "vim";
    OPENER = "mimeo";
    BROWSER = "firefox";
  };
  home.packages = with pkgs; [
    gopass
    gopass-jsonapi
    gnupg

    silver-searcher
  ];
  home.file = {
    ".config/broot/launcher/refused".source = ../config/broot/broot-refused;
  };

  nixpkgs.config.allowUnfree = true;
  programs.home-manager.enable = true;
}
