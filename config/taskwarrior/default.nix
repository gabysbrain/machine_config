{ pkgs, ... }:
{
  home.file = {
    ".taskrc".text = ''
        ${builtins.readFile ./baserc}

        # color theme
        ${builtins.readFile ./nord.theme}
    '';
  };
  home.packages = [
    # task management stuff
    pkgs.taskwarrior
    pkgs.timewarrior
    (pkgs.callPackage ../../pkgs/tasks {})
    (pkgs.callPackage ../../pkgs/weekly-review {})
  ];
}
