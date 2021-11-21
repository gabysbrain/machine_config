{ config, pkgs, nodes, ... }:
{ 
  imports = [
    <agenix/modules/age.nix>
  ];

  services.openssh = {
    enable = true;
    permitRootLogin = "prohibit-password";
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
    extraConfig = "Compression no";
  };

  networking.firewall.allowedTCPPorts = [ 
    9002
  ];

  # make sure prometheus can track everyone
  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = [ "systemd" "textfile" ];
    port = 9002;
  };

  # sending syslogs
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
        { url = "http://10.0.0.52:3100/loki/api/v1/push"; }
      ];

      scrape_configs = [ 
        # local journal scraping
        { 
          job_name = "journal";
          journal = {
            max_age = "12h";
            labels = {
              job = "systemd-journal";
              host = "${config.networking.hostName}";
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

  # limit the systemd journal so it doesn't just fill up                                                        
  services.journald.extraConfig = ''                                                                            
    SystemMaxUse=100M                                                                                           
    MaxFileSec=7day                                                                                             
  '';

  nix.autoOptimiseStore = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 5d";
  };
}
