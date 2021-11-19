{ config, pkgs, nodes, ... }:

let
  adblockLocalZones = pkgs.stdenv.mkDerivation {
    name = "unbound-zones-adblock";

    src = (pkgs.fetchFromGitHub {
      owner = "StevenBlack";
      repo = "hosts";
      rev = "3.0.0";
      sha256 = "01g6pc9s1ah2w1cbf6bvi424762hkbpbgja9585a0w99cq0n6bxv";
    } + "/hosts");

    phases = [ "installPhase" ];

    installPhase = ''
      cat $src | ${pkgs.gnugrep}/bin/grep '^0\.0\.0\.0' | ${pkgs.gnugrep}/bin/grep -v '^0\.0\.0\.0 0\.0\.0\.0' | ${pkgs.gawk}/bin/awk '{print "local-zone: \""$2".\" redirect\nlocal-data: \""$2" A 0.0.0.0\""}' > $out
    '';

  };
in
{ 
  networking.firewall.allowedUDPPorts = [ 53 ];
  networking.firewall.allowedTCPPorts = [ 53 ];

  services.unbound = {
    enable = true;
    settings = {
      server = {
        interface = [ "0.0.0.0" ];
        access-control = [ "10.0.0.0/8 allow" ];
        include = [ "${adblockLocalZones}" ];
      };

      forward-zone = [{
        name = ".";
        forward-addr = [ "8.8.8.8" ];
      }];

    };
  };
}

