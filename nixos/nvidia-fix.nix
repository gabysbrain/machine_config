{ config, pkgs, ... }:

# Fix for nvidia spamming the dbus log and breaking everything. 
# see https://forums.developer.nvidia.com/t/bug-nvidia-v495-29-05-driver-spamming-dbus-enabled-applications-with-invalid-messages/192892/36
let dbus-service = 
  pkgs.writeTextDir "/etc/dbus-1/system.d/nvidia-fake-powerd.conf" ''
    <!DOCTYPE busconfig PUBLIC
         "-//freedesktop//DTD D-BUS Bus Configuration 1.0//EN"
         "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
    <busconfig>
      <policy user="messagebus">
        <allow own="nvidia.powerd.server"/>
      </policy>
      <policy context="default">
        <allow send_destination="nvidia.powerd.server"/>
        <allow receive_sender="nvidia.powerd.server"/>
      </policy>
    </busconfig>  
  '';
in
{
  systemd.services.nvidia-fake-powerd = {
    wantedBy = [ "default.target" ];
    aliases = [ "dbus-nvidia.fake-powerd.service" ];
    description = "NVIDIA fake powerd service";
    serviceConfig = {
      Type = "dbus";
      BusName = "nvidia.powerd.server";
      ExecStart = "${pkgs.dbus}/bin/dbus-test-tool black-hole --system --name=nvidia.powerd.server";
      User = "messagebus";
      Group = "messagebus";
      LimitNPROC=2;
      ProtectHome = true;
      ProtectSystem = "full";
    };
  };
  services.dbus.packages = [ (pkgs.callPackage pkgs/nvidia-fake-powerd.nix {}) ];

}
