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
      ./config/desktop-full.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    hostName = "philadelphia"; # Define your hostname.
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
  nixpkgs.config = {
    allowUnfree = true;
  };
  #environment.systemPackages = with pkgs; [
  #];

  # List services that you want to enable

  # disable various buttons, like the power key!
  #services.logind.extraConfig = ''
    #HandlePowerKey = ignore
  #'';
}
