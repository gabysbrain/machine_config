# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../hardware-configuration.nix
      ./config/base.nix
      ./config/dev.nix
      ./config/writing.nix
      ./config/desktop-full.nix
    ];

  environment.systemPackages = with pkgs; [
    blueman
    pavucontrol
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelParams = [
      "pcie_aspm=force"
      "drm.vblankoffdelay=1"
      "i915.semaphores=1"
      "i915.enable_psr=0"
      "i915.enable_rc6=0"
    ];
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.enable = true;
      grub.device = "nodev";
      grub.efiSupport = true;
      grub.enableCryptodisk = true;
    };
    initrd = {
      luks.devices = [
        {
          name = "root";
          device = "/dev/disk/by-uuid/ac271696-4909-4b21-8731-77a3bc1f5392";
          preLVM = true;
          allowDiscards = true;
        }
      ];
    };
  };

  networking = {
    hostName = "philadelphia"; # Define your hostname.
  };

  # turn on bluetooth
  hardware.bluetooth.enable = true;

  # need to fix the sound config
  sound.extraConfig = ''
  '';

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
    /*xrandrHeads = [*/
      /*#{output="DP1-1";}*/
      /*#{output="DP1-2";}*/
      /*{output="eDP1"; primary=true;}*/
    /*];*/
    xkbOptions = "compose:menu";
  };

  services.printing.enable = true;

  services.fprintd.enable = true;

  services.devmon.enable = true;

  # power/temp management
  services.thermald.enable = true;
  services.tlp.enable = true;

  # disable various buttons, like the power key!
  #services.logind.extraConfig = ''
    #HandlePowerKey = ignore
  #'';
}
