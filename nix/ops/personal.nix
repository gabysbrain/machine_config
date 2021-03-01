let 
  diskstationIp = "192.168.0.14";
  rpiIp = "192.168.0.24";
  philadelphiaIp = "192.168.0.8";
in
{
  network.description = "Home network";

  defaults = {
  };

  media = 
    { config, pkgs, nodes, ... }:
    { 
      deployment.targetHost = rpiIp;
      deployment.keys = {
        smb-secrets = {
          text = builtins.readFile ./secrets/smb-secrets;
          #destDir = "/secrets";
        };
        wasabi = {
          text = builtins.readFile ./secrets/wasabi;
        };
        restic-password = {
          text = builtins.readFile ./secrets/restic-password;
        };
      };

      nixpkgs.system = "aarch64-linux";

      networking.hostName = "media";

      services.openssh = {
        enable = true;
        permitRootLogin = "prohibit-password";
        passwordAuthentication = false;
        challengeResponseAuthentication = false;
        extraConfig = "Compression no";
      };
      
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
          in ["credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
        };
        "/media/music" = {
          device = "//${diskstationIp}/music";
          fsType = "cifs";
          options = let
          # this line prevents hanging on network split
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s";
          in ["credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
        };
        "/var/lib/jellyfin/metadata" = {
          device = "//${diskstationIp}/jellyfin/metadata";
          fsType = "cifs";
          options = [
            "credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"
          ];
        };
        "/var/lib/jellyfin/transcodes" = {
          device = "//${diskstationIp}/jellyfin/transcodes";
          fsType = "cifs";
          options = [
            "credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"
          ];
        };

        # for loki
        "/var/lib/loki" = {
          device = "//${diskstationIp}/loki";
          fsType = "cifs";
          options = [
            "credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=loki,nounix"
          ];
        };

        # for prometheus
        "/var/lib/prometheus2" = {
          device = "//${diskstationIp}/prometheus";
          fsType = "cifs";
          options = [
            "credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=prometheus,nounix"
          ];
        };
      };

      users.groups.media.members = [ "tom" "jellyfin" ];
      users.groups.render.members = [ "jellyfin" ]; # for hardware acceleration
      services.jellyfin = {
        enable = true;
        group = "media";
      };

      /*
      services.deluge = {
        enable = true;
        group = "media";
        #openFirewall = true;
        #declarative = true;
        config = {
          max_upload_speed = "10";
          share_ratio_limit = "0.5";
        };
      };
      services.deluge.web = {
        enable = true;
        #group = "media";
        openFirewall = true;
      };
      services.sonarr = {
        enable = true;
        group = "media";
        openFirewall = true;
      };
      services.radarr = {
        enable = true;
        group = "media";
        openFirewall = true;
      };
      */


      networking.firewall.allowedTCPPorts = [ 53 80 443 3100 ];

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
      /*
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
      */
  
      # nginx reverse proxy
      /*
      services.nginx.virtualHosts.${config.services.grafana.domain} = {
        locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
            proxyWebsockets = true;
        };
      };
      */

      # send restic logs for prometheus
      systemd.services.restic-backups-remote-metrics = {
        description = "Generate prometheus metrics from restic";
        wantedBy = [ "multi-user.target" ];

        environment = {
          RESTIC_PASSWORD_FILE = "/run/keys/restic-password";
          RESTIC_REPOSITORY = "s3:https://s3.wasabisys.com/gabysbrain-restic";
          RESTIC_CACHE_DIR = "/root/.cache/restic";
        };
        path = [ pkgs.bash pkgs.restic pkgs.jq pkgs.openssh ];
        serviceConfig = {
          ExecStart = ''
            ${pkgs.callPackage ../pkgs/restic-metrics {}}/bin/restic-metrics
          '';
          User = "root";
          RuntimeDirectory = "restic-backups-remote";
          EnvironmentFile = "/run/keys/wasabi";
        };
      };
      systemd.timers.restic-backups-remote-metrics = {
        description = "Regenerate restic prometheus metrics";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "5m";
          OnUnitInactiveSec = "12h"; 
        };
      };

      # prometheus database
      services.prometheus = {
        enable = true;
        port = 9001;

        exporters = {
          node = {
            enable = true;
            enabledCollectors = [ "systemd" "textfile" ];
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
          {
            job_name = "philadelphia";
            static_configs = [{
              targets = [ "192.168.0.8:${toString config.services.prometheus.exporters.node.port}" ];
            }];
          }
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
          passwordFile = "/run/keys/restic-password";
          s3CredentialsFile = "/run/keys/wasabi";
          extraBackupArgs = [
            "--host=media"
            "--exclude=var/lib/jellyfin/transcodes"
            "--exclude='*.m4v'"
          ];
          timerConfig = {
            OnCalendar = "*-*-* 01:24:00";
            #OnBootSec = "2m";
            #OnUnitInactiveSec = "1d";
          };
        };
      };

      # coredns server
      /*
      services.coredns = {
        enable = true;
        config = ''
          . { 
            # cloudflare and google
            forward . 1.1.1.1 1.0.0.1 8.8.8.8 8.8.4.4
            cache
          }

          media.tomtorsneyweir.com {
            template IN A {
              answer "{{ .Name }} 0 IN A ${rpiIp}"
            }
          }

          diskstation.tomtorsneyweir.com {
            template IN A {
              answer "{{ .Name }} 0 IN A ${diskstationIp}"
            }
          }

        '';
      };
      */

      # Users
      /*
      users.users.tom = {
        home = "/home/tom";
        description = "Thomas Torsney-Weir";
        extraGroups = [ "wheel" "lp" "lpadmin" "media" ]; # Enable ‘sudo’ for the user.
        createHome = true;
        shell = "/run/current-system/sw/bin/zsh";
        openssh.authorizedKeys.keyFiles = [
          #"ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAwDPUjo80GFY2FO9bDH9cAXo7n7SiUKjXIHzfRMfsAqD9Rk/puV+W4QRvT0XOZSEZQf3gifcPM/raA35BVmAzAa2jYISWeUWIqYf+AcipFrMKKqS639Q9/GgJL2STr6Gh0EVHsGcFJpuJ8GO5eqnKR0ZYl3j9bpMO/WpgkAw7hUU= tom@katana"
          /home/tom/.ssh/id_rsa.pub
        ];
      };
      home-manager.users.tom = import ./home-config/server.nix; # needs to be a function
      */
    };
}

