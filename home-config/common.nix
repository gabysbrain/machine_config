{ pkgs, ... }:
{
  imports = [
    ../config/broot/default.nix
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
        pull.rebase = true;
        merge = {
          tool = "vimdiff";
          conflictstyle = "diff3";
        };
        mergetool.vimdiff.cmd = ''nvim -d "$LOCAL" "$MERGED" "$REMOTE"'';
        init = {
          defaultBranch = "dev";
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

  nixpkgs.config.allowUnfree = true;
  programs.home-manager.enable = true;
}
