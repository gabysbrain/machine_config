{ config, pkgs, ... }:

let 
  cacert = pkgs.fetchurl {
    url = http://ca.vrvis.at/vrvis_ca.pem;
    sha256 = "0pjni0hx7p6zb71lv5l67cfbg26nbyjd0fc24mn5w4gal7cjyl8s";
  };
in 
{
  # ca certs
  security.pki.certificates = [
    "${builtins.readFile cacert}"
    #(builtins.readFile ../secrets/vrvis_ca.pem)
  ];

  # printers
  hardware.printers.ensurePrinters = [
  ];

  # vpn
  services.openvpn.servers = {
    vrvisVPN = { 
      config = '' config /etc/nixos/secrets/vrvisVPN/torsney-weir_philadelphia_vrvis.ovpn ''; 
      autoStart = false;
      updateResolvConf = true;
    };
  };

  environment.systemPackages = with pkgs; [
    remmina # rdp
    zulip   # chat
  ];
}
