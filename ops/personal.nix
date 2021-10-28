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
        router-pw = {
          text = builtins.readFile ./secrets/router.prometheus;
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
        601 # syslog-ng receiver
      ];
      networking.firewall.allowedUDPPorts = [ 
        514 # syslog-ng receiver
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
  
      # nginx reverse proxy for grafana
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

        globalConfig = {
          scrape_timeout = "1m";
        };

        exporters = {
          node = {
            enable = true;
            enabledCollectors = [ "systemd" "textfile" ];
            port = 9002;
          };
          mikrotik = {
            enable = true;
            configuration = {
              devices = [
                { name = "main_router";
                  address = "10.0.0.1";
                  user = "prometheus";
                  #password_file = "/run/keys/router-pw";
                  password = "changeme";
                }
              ];
              features = {
                bgp = true;
                dhcp = true;
                routes = true;
                optics = true;
              };
            };
          };
          snmp = {
            enable = true;
            configurationPath = "${pkgs.prometheus-snmp-exporter.src}/snmp.yml";
          };
        };
        scrapeConfigs = [
          {
            job_name = "node";
            static_configs = [{
              targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
            }];
          }
          {
            job_name = "mikrotik";
            static_configs = [{
              targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.mikrotik.port}" ];
            }];
          }
          {
            job_name = "snmp";
            static_configs = [{
              targets = [ nas ];
            }];
            metrics_path = "/snmp";
            params = {
              module = [ "synology" ];
            };
            relabel_configs = [
              {
                source_labels = [ "__address__" ];
                target_label = "__param_target";
              }
              {
                source_labels = [ "__param_target" ];
                target_label = "instance";
              }
              {
                target_label = "__address__";
                replacement = "127.0.0.1:${toString config.services.prometheus.exporters.snmp.port}";
              }
            ];
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
        configuration = {
          server = {
            http_listen_port = 28183;
            grpc_listen_port = 0;
          };

          positions = {
            filename = "/tmp/positions.yaml";
          };

          clients = [
            { url = "http://127.0.0.1:3100/loki/api/v1/push"; }
          ];

          scrape_configs = [ 
            # local journal scraping
            { 
              job_name = "journal";
              journal = {
                max_age = "12h";
                labels = {
                  job = "systemd-journal";
                  host = "monitor";
                };
              };
              relabel_configs = [ {
                source_labels = [ "__journal__systemd_unit" ];
                target_label = "unit";
              } ];
            } 
            {
              job_name = "syslog";
              syslog = {
                listen_address = "127.0.0.1:1514";
                idle_timeout = "60s";
                label_structured_data = true;
                labels = {
                  job = "syslog";
                };
              };
              relabel_configs = [ {
                source_labels = [ "__syslog_message_hostname" ];
                target_label = "host";
              } ];
            }
          ];
        };
      };
      services.syslog-ng = {
        enable = true;
        extraConfig = ''
          source s_tcp {
            syslog( port(601) transport("tcp"));
          };
          source s_udp {
            network( port(514) transport("udp"));
          };

          destination d_loki {
            syslog("localhost" transport("tcp") port(1514));
          };

          log {
            source(s_udp);
            source(s_tcp);
            destination(d_loki);
          };
        '';
      };
    };
}

