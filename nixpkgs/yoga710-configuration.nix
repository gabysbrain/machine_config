# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

  #myvim = import /home/tom/Projects/dotfiles/nix/vim.nix;
{
  imports =
    [ # Include the results of the hardware scan.
      ../hardware-configuration.nix
      ./config/base.nix
      ./config/dev.nix
      ./config/writing.nix
      ./config/desktop-light.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    #blacklistedKernelModules =[ "i2c_designware_core" "i2c_designware_platform" ];
    kernelPackages = pkgs.linuxPackages;
    kernelModules = [
      "hid-sensor-trigger"
      "hid-sensor-hub"
      "hid-sensor-als"
      "hid-sensor-accel-3d"
      "hid-sensor-gyro-3d"
      "hid-sensor-magn-3d"
      "hid-sensor-incl-3d"
    ];
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
  };

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Vienna"; # hmoe time zone
  #time.timeZone = "US/Eastern"; # alternate time zone

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  #environment.systemPackages = with pkgs; [
  #];

  # List services that you want to enable

  # disable various buttons, like the power key!
  services.logind.extraConfig = ''
    HandlePowerKey = ignore
  '';
}
