# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../hardware-configuration.nix
      #../secure.nix
      ./config/base.nix
      ./config/dev.nix
      ./config/writing.nix
      ./config/desktop-full.nix
      #./pkgs/dsmc-service.nix
    ];

  environment.systemPackages = with pkgs; [
    pavucontrol
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    #kernelModules = [ "coretemp" ];
    #kernelParams = [
      #"pcie_aspm=force"
      #"drm.vblankoffdelay=1"
      #"i915.semaphores=1"
      #"i915.enable_psr=0"
      #"i915.enable_rc6=0"
    #];
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.enable = true;
      #grub.device = "nodev";
      grub.device = "/dev/sda";
      grub.efiSupport = true;
      grub.enableCryptodisk = true;
    };
  };

  networking = {
    hostName = "supermicro"; # Define your hostname.
    #networkmanager.enable = true;
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

  services.xserver = {
    monitorSection = ''
      DisplaySize 406 228
    '';
  };

  services.printing.enable = true;

  services.fprintd.enable = true;

  services.devmon.enable = true;

  services.syncthing = {
    enable = true;
    user = "torsnet6cs";
    dataDir = "/home/torsnet6cs/.config/syncthing";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.torsnet6cs = {
    name = "torsnet6cs";
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video" "networkmanager" "systemd-journal"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/torsnet6cs";
    shell = "/run/current-system/sw/bin/zsh";
  };

}