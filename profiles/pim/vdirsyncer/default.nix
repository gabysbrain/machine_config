{ pkgs, lib, ... }:

{
  homeage.file.google-vdirsyncer.source = ../../../secrets/google-vdirsyncer.age;
  home.file = {
    ".vdirsyncer/config".source = ./dot-vdirsyncer;
  };

  home.packages = with pkgs; [
     vdirsyncer # to put vdirsyncer in my path
  ];

  systemd.user = {
    services = {
      vdirsyncer = {
        Unit = {
          Description="sync vcard/vcal servers";
        };
        Service = {
          Environment = "PATH=${
            lib.makeBinPath (with pkgs; [ coreutils gnused ])
          }";
          ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync";
        };
      };
    };
    timers = {
      vdirsyncer = {
        Unit = {
          Description="sync vcard/vcal servers";
        };
        Timer = {
          OnBootSec = "2m";
          OnUnitInactiveSec = "15m";
        };
        Install = {
          WantedBy = ["timers.target"];
        };
      };
    };
  };
}
