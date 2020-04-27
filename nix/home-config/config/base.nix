{ pkgs, ... }:
{
  imports = [
    ./zsh.nix
    ./tmux.nix
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
        theme = "tender";
      };
    };
  };
  home.sessionVariables = {
    EDITOR = "vim";
    BROWSER = "firefox";
  };
  home.packages = [
    # TODO: integrate this into the programs.vim module
    (pkgs.callPackage ../../pkgs/vim {})
    (pkgs.callPackage ../../pkgs/tat {})
  ];
  home.file = {
    ".config/bat/themes/tender.tmTheme".source = ../../../dotfiles/dot-bat/tender.tmTheme;
  };

  #home.stateVersion = "18.09";
  programs.home-manager.enable = true;
}
