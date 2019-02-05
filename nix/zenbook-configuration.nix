# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../hardware-configuration.nix
      ../../secret.nix
      ./config/base.nix
      ./config/dev.nix
      ./config/writing.nix
      ./config/laptop.nix
      ./config/desktop-full.nix
      ./pkgs/dsmc-service.nix
    ];

  environment.systemPackages = with pkgs; [
    blueman
    pavucontrol
    psensor
    linuxPackages.cpupower
    cpufrequtils
    acpi
    lm_sensors
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelModules = [ "coretemp" ];
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
          device = "/dev/nvme0n1p2";
          preLVM = true;
          allowDiscards = true;
        }
      ];
    };
  };

  networking = {
    hostName = "philadelphia"; # Define your hostname.
    #networkmanager.enable = true;
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

  # shared filesystem mounts
  fileSystems."/mnt/ds_homes" = {
    device = "//10.0.0.1/homes";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s,file_mode=0660,dir_mode=0770,gid=1,nounix";
      in ["${automount_opts},credentials=/etc/nixos/smb-secrets,vers=1.0"];
  };
  # List services that you want to enable

  services.printing.enable = true;

  services.fprintd.enable = true;

  services.devmon.enable = true;

  services.syncthing = {
    enable = true;
    user = "torsnet6cs";
    dataDir = "/home/torsnet6cs/.config/syncthing";
  };

  # univie tivoli backup
  services.dsmc = {
    enable = true;
    interval = "Mon..Fri 12:00";
    nodename = "PHILADELPHIA.CS.UNIVIE.AC.AT";
    serverName = "BACKUPX0";
    serverAddress = "BACKUPX0.UNIVIE.AC.AT";
    #password = "";
    excludes = [
      "/.../dsmsched.log"
      "/.../dsmprune.log" 
      "/.../dsmerror.log" 
      "/.../dsmj.log" 
    ];
    excludeDirs = [
      "/boot" 
      "/private" 
      "/nix" 
      "/tmp" 
      "/proc" 
      "/home/torsnet6cs/.cache" 
      "/home/torsnet6cs/.mozilla" 
      "/home/torsnet6cs/ownCloud"
      "/home/torsnet6cs/Dropbox"
      "/var/tmp/"
    ];
  };

  # home backup
  services.borgbackup.jobs = {
    homeBackup = {
      paths = "/home/torsnet6cs";
      repo = "/mnt/ds_homes/gabysbrain/backups/zenbook";
      compression = "auto,lzma";
      encryption.mode = "none";
      startAt = "daily";
    };
  };

  # power/temp management
  #services.thermald.enable = true;
  services.tlp = {
    enable = true;
    extraConfig = ''
      CPU_HWP_ON_AC=balance_power
      CPU_HWP_ON_BAT=power

      ENERGY_PERF_POLICY_ON_AC=balance-performance
    '';
  };
  services.upower.enable = true;
  
  powerManagement.enable = true;

  # change power button configs
  services.logind.extraConfig = ''
    HandlePowerKey = ignore
    HandleSuspendKey = hibernate
  '';

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
