{ pkgs, ... }:
{
  home.file = {
    ".taskrc".source = ./dot-taskrc;
    ".task/baserc".source = ./dot-taskwarrior/baserc;
    ".task/nord.theme".source = ./dot-taskwarrior/nord.theme;
  };
  home.packages = [
    # task management stuff
    pkgs.taskwarrior
    pkgs.timewarrior
    (pkgs.callPackage ../../pkgs/tasks {})
    (pkgs.callPackage ../../pkgs/weekly-review {})
  ];
}
