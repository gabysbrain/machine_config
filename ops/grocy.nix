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
}

