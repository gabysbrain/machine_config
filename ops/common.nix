{ config, pkgs, nodes, ... }:
{ 
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
