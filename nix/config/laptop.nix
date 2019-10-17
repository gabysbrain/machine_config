{ config, pkgs, ... }:

{
  networking = {
    wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    #connman.enable = true;
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };

  boot.extraModprobeConfig = "options snd_hda_intel power_save=1";

  environment.systemPackages = with pkgs; [
    (callPackage ../pkgs/screenselect.nix {})
    xorg.xbacklight
    powertop
    lm_sensors
    acpi
  ];
}
