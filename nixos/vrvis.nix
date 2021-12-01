{ config, pkgs, ... }:

# TODO: unify this with the profile/vrvis
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
  security.sudo.extraRules = [
    { 
      groups = [ "wheel" ]; 
      commands = [
        { command = "/usr/bin/systemctl restart openvpn-vrvisVPN.service"; options = [ "NOPASSWD" ]; }
        { command = "/usr/bin/systemctl stop openvpn-vrvisVPN.service"; options = [ "NOPASSWD" ]; }
        { command = "/usr/bin/systemctl start openvpn-vrvisVPN.service"; options = [ "NOPASSWD" ]; }
      ];
    }
  ];
}
