let 
  diskstationIp = "192.168.0.14";
  rpiIp = "192.168.0.24";
in
{
  network.description = "Home network";

  defaults = {
  };

  media = 
    { config, pkgs, nodes, ... }:
    let
      sambaSecrets = pkgs.writeTextDir "smb-secrets" (builtins.readFile ./secrets/smb-secrets);
      wasabiSecrets = pkgs.writeTextDir "wasabi" (builtins.readFile ./secrets/wasabi);
      resticSecrets = pkgs.writeTextDir "restic-password" (builtins.readFile ./secrets/restic-password);
    in 
    { deployment.targetHost = rpiIp;
      nixpkgs.system = "aarch64-linux";

      networking.hostName = "media";
      
      imports = [
        ../rpi-configuration.nix
      ];

      fileSystems = {
        "/media/videos" = {
          device = "//${diskstationIp}/videos";
          fsType = "cifs";
          options = let
          # this line prevents hanging on network split
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s";
          in ["credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
        };
        "/media/music" = {
          device = "//${diskstationIp}/music";
          fsType = "cifs";
          options = let
          # this line prevents hanging on network split
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s";
          in ["credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
        };
        "/var/lib/jellyfin/metadata" = {
          device = "//${diskstationIp}/jellyfin/metadata";
          fsType = "cifs";
          options = [
            "credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"
          ];
        };
        "/var/lib/jellyfin/transcodes" = {
          device = "//${diskstationIp}/jellyfin/transcodes";
          fsType = "cifs";
          options = [
            "credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"
          ];
        };

        # for loki
        "/var/lib/loki" = {
          device = "//${diskstationIp}/loki";
          fsType = "cifs";
          options = [
            "credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=loki,nounix"
          ];
        };

        # for prometheus
        "/var/lib/prometheus2" = {
          device = "//${diskstationIp}/prometheus";
          fsType = "cifs";
          options = [
            "credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=prometheus,nounix"
          ];
        };
      };

      users.groups.media.members = [ "tom" "jellyfin" ];
      services.jellyfin = {
        enable = true;
        group = "media";
      };
      networking.firewall.allowedTCPPorts = [ 80 443 3100 ];

      #security.acme.email = "torsneyt@gmail.com";
      #security.acme.acceptTerms = true;

      services.nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        virtualHosts."media.tomtorsneyweir.com" = {
          #addSSL = true;
          #enableACME = true;
          locations."/" = {
            proxyPass = "http://localhost:8096";
          };
        };
      };

      # grafana service
      services.grafana = {
        enable = true;
        domain = "grafana.tomtorsneyweir.com";
        port = 2342;
        addr = "127.0.0.1";
        provision = {
          enable = true;
          datasources = [
            { name = "loki"; type = "loki"; url = "http://localhost:3100"; }
            { name = "prometheus"; type = "prometheus"; url = "http://localhost:9001"; }
          ];
        };
      };
  
      # nginx reverse proxy
      services.nginx.virtualHosts.${config.services.grafana.domain} = {
        locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
            proxyWebsockets = true;
        };
      };

      # prometheus database
      services.prometheus = {
        enable = true;
        port = 9001;

        exporters = {
          node = {
            enable = true;
            enabledCollectors = [ "systemd" ];
            port = 9002;
          };
        };
        scrapeConfigs = [
          {
            job_name = "media";
            static_configs = [{
              targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
            }];
          }
          /*
          {
            job_name = "philadelphia";
            static_configs = [{
              targets = [ "192.168.0.8:${toString config.services.prometheus.exporters.node.port}" ];
            }];
          }
          */
        ];
      };


      # loki log server
      services.loki = {
        enable = true;
        configFile = ./loki.yaml;
      };
      #services.promtail.enable = true; # FIXME: not in nixos yet :(

      # promtail to get logs into loki
      systemd.services.promtail = {
        description = "Promtail service for Loki";
        wantedBy = [ "multi-user.target" ];

        serviceConfig = {
          ExecStart = ''
            ${pkgs.grafana-loki}/bin/promtail --config.file ${./promtail.yaml}
          '';
        };
      };

      # backup media server
      services.restic.backups = {
        remote = {
          paths = [ 
            "/media" 
            "/var/lib/jellyfin"
          ];
          repository = "s3:https://s3.eu-central-1.wasabisys.com/gabysbrain-restic";
          passwordFile = "${resticSecrets}/restic-password";
          s3CredentialsFile = "${wasabiSecrets}/wasabi";
          extraBackupArgs = [
            "--host=media"
          ];
          timerConfig = {
            OnCalendar = "*-*-* 01:24:00";
            #OnBootSec = "2m";
            #OnUnitInactiveSec = "1d";
          };
        };
      };
    };
}

