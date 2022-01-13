{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

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
''

