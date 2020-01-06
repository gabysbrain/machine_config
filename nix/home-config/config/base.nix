{ pkgs, ... }:
{
  imports = [
    ./zsh.nix
    ./tmux.nix
  ];
  programs = {
    git = {
      enable = true;
      userName = "Thomas Torsney-Weir";
      userEmail = "torsneyt@gmail.com";
    };
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        theme = "zenburn";
      };
    };
  };
  home.sessionVariables = {
    EDITOR = "vim";
    BROWSER = "firefox";
  };
  home.packages = [
    # TODO: integrate this into the programs.vim module
    (pkgs.callPackage ../../pkgs/vim.nix {})
    (pkgs.callPackage ../../pkgs/tat {})
  ];

  #home.stateVersion = "18.09";
  programs.home-manager.enable = true;
}
