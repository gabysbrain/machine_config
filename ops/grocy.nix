{ config, pkgs, nodes, ... }:
{ 
  nixpkgs.system = "aarch64-linux";

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

