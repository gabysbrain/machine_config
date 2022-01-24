{ config, pkgs, nodes, ... }:
{ 
  fileSystems = {
    # for loki and prometheus
    "/var/lib" = {
      device = "/dev/sda1";
      fsType = "ext4";
    };
  };

  networking.firewall.allowedTCPPorts = [ 
    53 80 443 
    3100 # loki receiver
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
    dataDir = "/var/lib/grafana";
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

  # secrets/secret configs for prometheus things
  age.secrets = {
    "mikrotik-prometheus-config.yml" = {
      file = ../secrets/mikrotik-prometheus-config.yml.age;
      # FIXME: the service doesn't create an actual user and I can't figure out how to do that
      #owner = "${config.services.prometheus.exporters.mikrotik.user}";
      mode = "0666";
    };
    fritzbox-pw = {
      file = ../secrets/fritzbox-pw.age;
      # FIXME: the service doesn't create an actual user and I can't figure out how to do that
      #owner = "${toString config.services.prometheus.exporters.fritzbox.user}";
      mode = "0666";
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
      mikrotik = {
        enable = true;
        configFile = /run/agenix/mikrotik-prometheus-config.yml;
      };
      snmp = {
        enable = true;
        configurationPath = "${pkgs.prometheus-snmp-exporter.src}/snmp.yml";
      };
      fritzbox = {
        enable = true;
        gatewayAddress = "192.168.178.1";
        extraFlags = [
          "-username" "admin"
          "-password" "`cat /run/agenix/fritzbox-pw`"
        ];
      };
    };
    scrapeConfigs = [
      {
        job_name = "node";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" 
                      "util.lan:${toString config.services.prometheus.exporters.node.port}"
                    ];
        }];
      }
      {
        job_name = "smartctl";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.smartctl.port}" 
                      #"util.lan:${toString config.services.prometheus.exporters.smartctl.port}"
                    ];
        }];
      }
      {
        job_name = "mikrotik";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.mikrotik.port}" ];
        }];
      }
      {
        job_name = "fritzbox";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.fritzbox.port}" ];
        }];
      }
      {
        job_name = "snmp";
        static_configs = [{
          targets = [ "lr-switch.lan" ];
        }];
        metrics_path = "/snmp";
        params = {
          module = [ "mikrotik" ];
          #walk = 
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
      {
        job_name = "unbound";
        static_configs = [{
          targets = [ "util.lan:${toString config.services.prometheus.exporters.unbound.port}" ];
        }];
      }
    ];
  };

  # loki log server
  services.loki = {
    enable = true;
    configFile = ./loki.yaml;
  };
  /*
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
  */
}
