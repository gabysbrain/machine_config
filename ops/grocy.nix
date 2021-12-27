{ config, pkgs, nodes, ... }:
{ 
  networking.firewall.allowedTCPPorts = [ 
    80 443 
  ];

  services.grocy = {
    enable = true;
    hostName = "grocy.lan";
    nginx.enableSSL = false;

    settings = {
      currency = "EUR";
      culture = "en";
      calendar.firstDayOfWeek = 1; # monday
    };
  };

  # backup grocy stuff
  age.secrets.wasabi.file = ../secrets/wasabi.age;
  age.secrets.restic.file = ../secrets/restic.age;
  services.restic.backups.remote = {
    paths = [ "/var/lib/grocy" ];
    repository = "s3:https://s3.eu-central-1.wasabisys.com/gabysbrain-restic";
    passwordFile = "/run/agenix/restic"; # FIXME: this should use age.secrets.restic.path somehow
    environmentFile = "/run/agenix/wasabi"; # FIXME: this should use age.secrets.wasbi.path
    extraBackupArgs = [
      "--no-cache"
    ];
    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 8"
      "--keep-monthly 24"
      "--keep-yearly 5"
      "--keep-last 2"
    ];
    timerConfig = {
      OnCalendar = "02:15";
    };
  };
}

