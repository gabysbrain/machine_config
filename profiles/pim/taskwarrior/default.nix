{ pkgs, lib, nixosConfig, ... }:

{
  nixpkgs.overlays = [
    # taskw 1.3.0 has a bug with recent taskwarrior
    # see https://github.com/ralphbean/taskw/pull/141
    (import ../../../overlays/taskw.nix)
  ];

  # FIXME: need to get the system hostname to do the check in this module
  home.file = {
    ".taskrc".text = ''
        ${builtins.readFile ./baserc}

        # only 1 system should do recurrence
        recurrence=${if (nixosConfig.networking.hostName == "katana") then "on" else "off"}

        # reports
        ${builtins.readFile ./reports}

        # bugwarrior udas
        ${builtins.readFile ./bugwarrior.udas}

        # color theme
        ${builtins.readFile ./nord.theme}
    '';
    ".config/bugwarrior/bugwarriorrc".source = ./bugwarriorrc;
    ".tmuxp/Tasks.yaml".source = ./Tasks.yaml;
  };
  home.packages = with pkgs; [
    # task management stuff
    taskwarrior
    timewarrior
    python38Packages.bugwarrior
    tmuxp
    (callPackage ../../../pkgs/weekly-review {})
  ];
  programs.zsh.initExtra = ''
    # open taskwarrior task in jira
    function twjira {
      readonly twid=''${1:?"A task id must be specified."}
      open `task _get "$twid".jiraurl`
    }

    # taskwarrior shortcuts
    function twa {
      task add $*
    }

    function twd {
      task $* done
    }

    function twrm {
      task $* delete
    }

    function tws {
      task sync
    }

    function twrs {
      readonly datespec=''${1:?"The reschedule data must be specified."}
      # TODO: make sure some task ids are specified
      shift 1
      task $* modify scheduled:"$datespec"
    }
  '';
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
