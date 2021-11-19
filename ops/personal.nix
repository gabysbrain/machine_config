let
  nas = "diskstation.lan";
in
{
  network.description = "Home network";

  defaults = {
  };

  monitor = 
    { config, pkgs, nodes, ... }:
    { 
      deployment.targetHost = "10.0.0.52";
      deployment.targetUser = "root";

      networking.hostName = "monitor";

      # TODO: replace with agenix
      deployment.secrets = {
        router-pw = {
          source = "./secrets/router.prometheus";
          destination = "/var/secrets/router-pw";
          action = ["sudo" "systemctl" "restart" "prometheus.service"];
        };
      };

      imports = [
        ./rpi-3.nix
        ./common.nix
        ./monitoring.nix
      ];
    };

  util = 
    { config, pkgs, nodes, ... }:
    { 
      deployment.targetHost = "10.0.0.53";
      deployment.targetUser = "root";

      networking.hostName = "util";

      imports = [
        ./rpi-3.nix
        ./common.nix
        ./grocy.nix
        ./dns.nix
      ];
    };


}

