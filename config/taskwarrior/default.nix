{ pkgs, lib, ... }:
{
  nixpkgs.overlays = [
    # taskw 1.3.0 has a bug with recent taskwarrior
    # see https://github.com/ralphbean/taskw/pull/141
    (import ../../overlays/taskw.nix)
  ];

  home.file = {
    ".taskrc".text = ''
        ${builtins.readFile ./baserc}

        # bugwarrior udas
        ${builtins.readFile ./bugwarrior.udas}

        # color theme
        ${builtins.readFile ./nord.theme}
    '';
    ".config/bugwarrior/bugwarriorrc".source = ./bugwarriorrc;
  };
  home.packages = [
    # task management stuff
    pkgs.taskwarrior
    pkgs.timewarrior
    pkgs.python38Packages.bugwarrior
    (pkgs.callPackage ../../pkgs/tasks {})
    (pkgs.callPackage ../../pkgs/weekly-review {})
  ];
  systemd.user.services.bugwarrior-pull = {
    #path = [
      #"${pkgs.taskwarrior}/bin"
    #];
    Unit = {
      Description = "sync tasks with external sources using bugwarrior-pull";
    };
    Service = {
      #Environment = "DISPLAY=:0";
      Environment = "PATH=${
        lib.makeBinPath (with pkgs; [ taskwarrior coreutils gnugrep ])
      }";
      ExecStart = "${pkgs.python38Packages.bugwarrior}/bin/bugwarrior-pull";
    };
  };
  systemd.user.timers.bugwarrior-pull = {
    Unit = {
      Description = "run bugwarrior-pull periodically";
    };
    Timer = {
      OnBootSec="3min";
      OnUnitActiveSec="8h";
    };
  };
}
