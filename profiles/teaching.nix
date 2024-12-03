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
    settings = {
      mysqld = {
        lower_case_table_names = 1;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    obs-studio
    discord
    zoom-us
    v4l-utils

    vscode
    paraview
  ];

  # needed for mysql vscode password saving
  services.gnome-keyring.enable = true;
}
