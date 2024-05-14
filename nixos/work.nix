{ config, pkgs, ... }:

{
  #services.openvpn.servers = {
    #raiconVPN = { config = '' config /home/tom/keys/raicoon/vpn/VPN_GTS.ovpn ''; };
  #};

  networking.hosts = {
    "127.0.0.1" = [ 
      "staging.grafana.gfortis.net" 
      "staging.kibana.gfortis.net"
      "production.grafana.gfortis.net"
      "production.kibana.gfortis.net"
    ];
  };
}
