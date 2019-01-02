#with import <nixpkgs> {};
{config, pkgs, lib, ...}:

let 
  cfg = config.services.dsmc;
  dsmc = import ./dsmc.nix {
    inherit lib;
    inherit pkgs;
    stdenv = pkgs.stdenv;
    config = cfg;
  };
in 

with lib;

{
  options = {
    services.dsmc = {
      enable = mkEnableOption "enable periodic Tivoli backup";

      interval = mkOption {
        type = types.str;
        default = "daily";
        description = ''
          How often we run dsmc. For most desktop and server systems
          a sufficient frequency is daily.

          The format is described in
          <citerefentry><refentrytitle>systemd.time</refentrytitle>
          <manvolnum>7</manvolnum></citerefentry>.
        '';
      };

      serverName = mkOption {
        type = types.str;
        description = ''
          The short hostname of the backup server to connect to
        '';
      };

      serverAddress = mkOption {
        type = types.str;
        description = ''
          The full hostname of the backup server to connect to
        '';
      };

      password = mkOption {
        type = types.str;
        default = "";
        description = ''
          Password to log into the server
        '';
      };

      nodename = mkOption {
        type = types.str;
        description = ''
          Name of the Tivoli node
        '';
      };

      excludes = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          List of files to exclude from the backup
        '';
      };

      excludeDirs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          List of directories to exclude from the backup
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.dsmc-backup = {
      description = "Tivoli run script";
      serviceConfig.ExecStart = "${dsmc}/bin/dsmc incremental";
    };
    systemd.timers.dsmc-backup = {
      timerConfig.OnCalendar = cfg.interval;
      wantedBy = [ "timers.target" ];
    };
  };
}

