{ config, pkgs, ... }:

# packages needed for the modules that I'm teaching
{
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
  ];
  boot.extraModprobeConfig = ''
  options v4l2loopback video_nr=10 card_label="OBS Video Source" exclusive_caps=1
  '';
  boot.kernelModules = [
    "v4l2loopback"
  ];

  services.mysql = {
    enable = true;
    dataDir = "/var/db/mysql";
    package = pkgs.mysql;
  };

  environment.systemPackages = with pkgs; [
    obs-studio
    discord
    zoom-us
    v4l-utils
  ];
}
