let
  nas = "diskstation.lan";
  smbCreds = ./secrets/diskstation-smb;
in
{
  network.description = "Home network";

  defaults = {
  };

  monitor = 
    { config, pkgs, nodes, ... }:
    { 
      deployment.targetHost = "10.0.0.52";
      # TODO: replace with agenix
      deployment.keys = {
        smb-secrets = {
          text = builtins.readFile smbCreds;
          #destDir = "/secrets";
        };
      };

      nixpkgs.system = "aarch64-linux";

      networking.hostName = "monitor";

      services.openssh = {
        enable = true;
        permitRootLogin = "prohibit-password";
        passwordAuthentication = false;
        challengeResponseAuthentication = false;
        extraConfig = "Compression no";
      };
      
      imports = [
        ../rpi-image/rpi3-configuration.nix
      ];

      fileSystems = {
        # for loki
        "/var/lib/loki" = {
          device = "//${nas}/loki";
          fsType = "cifs";
          options = [
            "credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=loki,nounix"
          ];
        };

        # for prometheus
        "/var/lib/prometheus2" = {
          device = "//${nas}/prometheus";
          fsType = "cifs";
          options = [
            "credentials=/run/keys/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=prometheus,nounix"
          ];
        };
      };

      networking.firewall.allowedTCPPorts = [ 
        53 80 443 
        #3100 # grafana
      ];

      services.nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
      };

      # grafana service
      services.grafana = {
        enable = true;
        domain = "grafana.lan";
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
            enabledCollectors = [ "systemd" "textfile" ];
            port = 9002;
          };
        };
        scrapeConfigs = [
          {
            job_name = "monitor";
            static_configs = [{
              targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
            }];
          }
        ];
      };

      # loki log server
      services.loki = {
        enable = true;
        configFile = ./loki.yaml;
      };
      services.promtail = {
        enable = true;
        configFile = ./promtail.yaml;
      };
    };
}

