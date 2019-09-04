{ config, pkgs, ... }:

{
  networking = {
    wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    #connman.enable = true;
  };

  environment.systemPackages = with pkgs; [
    (import ../pkgs/screenselect.nix)
    xorg.xbacklight
    powertop
    lm_sensors
    acpi
  ];
}
