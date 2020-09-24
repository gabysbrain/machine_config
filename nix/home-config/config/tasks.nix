{ pkgs, ... }:
{
  home.file = {
    ".taskrc".source = ../../../dotfiles/dot-taskrc;
    ".task/nord.theme".source = ../../../dotfiles/dot-taskwarrior/nord.theme;
  };
  home.packages = [
    # task management stuff
    pkgs.taskwarrior
    pkgs.timewarrior
    (pkgs.callPackage ../../pkgs/tasks {})
  ];
}
