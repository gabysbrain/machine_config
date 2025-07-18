{ pkgs, lib, nixosConfig, ... }:

let
  bugwarrior-pkg = pkgs.python3.pkgs.bugwarrior;

  # Report definitions
  reports = {
    # next filter
    next = {
      labels = [ "ID" "Project" "Recur" "Start" "Due" "Description" "Urg" ];
      columns = [ "id" "project" "recur" "scheduled.countdown" "due.relative" "description" "urgency" ];
      filter = "urgency>1 status:pending -WAITING";
    };

    # things to do today
    today = {
      description = "Tasks for today";
      columns = [ "id" "project" "scheduled.countdown" "due.relative" "description" "urgency" ];
      sort = [ "due+" "scheduled+" "urgency-" ];
      filter = "(scheduled.before:tomorrow or due.before:yesterday) and status:pending -WAITING";
    };

    # Things to do the next week
    week = {
      description = "Tasks for the next week";
      columns = [ "id" "project" "scheduled.countdown" "due.relative" "description" "urgency" ];
      sort = [ "due+" "scheduled+" "urgency-" ];
      filter = "(scheduled.before:today+7days or due.before:today+7days) and status:pending -WAITING";
    };

    # Tasks that need following up on
    followup = {
      description = "Things for followup/review";
      columns = [ "id" "uuid.short" "description" "project" "status" ];
      filter = "+followup";
    };

    _reviewed = {
      description="Tasksh review report. Adjust the filter to your needs.";
      columns="uuid";
      sort= [ "reviewed+" "modified+" ];
      filter="( reviewed.none: or reviewed.before:now-6days ) and ( +PENDING or +WAITING )";
    };
  };
in
{
  homeage.file.github-bugwarrior.source = ../../../secrets/github-bugwarrior.age;
  programs.taskwarrior = {
    enable = true;

    config = {
      # only 1 machine should do recurrence
      recurrence = nixosConfig.networking.hostName == "katana";

      # sync settings
      taskd = {
        key="~/keys/joukamachi_taskserver/private.key";
        certificate="~/keys/joukamachi_taskserver/public.key";
        ca="~/keys/joukamachi_taskserver/ca.cert";
        server="taskserver.joukamachi.net:53589";
        credentials="hadleyco/tom/32ddf58f-63cd-4991-b5b6-5d33ae9ac492";
        trust="strict";
        #taskd.trust=ignore hostname
      };

      # turn off confirmations
      confirmation = false;
      #recurrence.confirmation = false;

      # setup urgency computation
      urgency = {
        user.tag.next.coefficient  = 15.0; # +next tag
        due.coefficient            = 12.0; # overdue or near due date
        blocking.coefficient       =  0.0; # blocking other tasks
        uda.priority.H.coefficient =  0.0; # high Priority
        uda.priority.M.coefficient =  0.0; # medium Priority
        uda.priority.L.coefficient =  0.0; # low Priority
        scheduled.coefficient      = 10.0; # scheduled tasks
        active.coefficient         = 20.0; # already started tasks
        age.coefficient            =  0.1; # coefficient for age
        annotations.coefficient    =  0.0; # has annotations
        tags.coefficient           =  0.0; # has tags
        project.coefficient        =  0.0; # assigned to any project
        waiting.coefficient        =  0.0; # waiting task
        blocked.coefficient        =  0.0; # blocked by other tasks
      };

      # Task review
      uda.reviewed.type="date";
      uda.reviewed.label="Reviewed";

      # Contexts
      context = {
        home.read="-work";
        work.read="+work";
        work.write="+work";
      };

      report = reports;
    };

    extraConfig = ''
      # bugwarrior udas
      ${builtins.readFile ./bugwarrior.udas}

      # color theme
      ${builtins.readFile ./nord.theme}
    '';
  };

  home.file = {
    ".config/bugwarrior/bugwarriorrc".source = ./bugwarriorrc;
    ".tmuxp/Tasks.yaml".source = ./Tasks.yaml;
  };
  home.packages = with pkgs; [
    # task management stuff
    timewarrior
    bugwarrior-pkg
    tmuxp # for the task view
    (callPackage ../../../pkgs/weekly-review {})
  ];
  programs.zsh.initContent = ''
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

    function twc {
      task context $1
    }

    function twrs {
      readonly datespec=''${1:?"The reschedule data must be specified."}
      # TODO: make sure some task ids are specified
      shift 1
      task $* modify scheduled:"$datespec"
    }
  '';
  systemd.user.services.bugwarrior-pull = {
    Unit = {
      Description = "sync tasks with external sources using bugwarrior-pull";
    };
    Service = {
      Environment = "PATH=${
        lib.makeBinPath (with pkgs; [ taskwarrior coreutils gnugrep ])
      }";
      ExecStart = "${bugwarrior-pkg}/bin/bugwarrior-pull";
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
