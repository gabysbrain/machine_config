{ config, pkgs, ... }:

{
  networking.wireless.iwd.enable = true;

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };

  boot.extraModprobeConfig = "options snd_hda_intel power_save=1";

  powerManagement.powertop.enable = true;
  services.thermald.enable = true;

  # change cpu performance if on battery
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
 };

  environment.systemPackages = with pkgs; [
    (callPackage ../pkgs/screenselect.nix {})
    (callPackage ../pkgs/speakerselect.nix {})
    xorg.xbacklight
    powertop
    lm_sensors
    acpi
  ];
}
