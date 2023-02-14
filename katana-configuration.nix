# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ ./nixos/nvidia-fix.nix
      ./nixos/common.nix
      ./nixos/games.nix
      ./nixos/desktop.nix
      ./nixos/rgb.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # for building nixos on other systems (e.g. raspberry pi)
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/home";
      fsType = "ext4";
    };

  fileSystems."/photos" =
    { device = "/dev/disk/by-label/photos";
      fsType = "ext4";
    };

  fileSystems."/games" =
    { device = "/dev/disk/by-label/games";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swap"; }
    ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  networking.hostName = "katana"; # Define your hostname.

  # Video drivers setup
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp5s0.useDHCP = true;
  #networking.interfaces.wlp4s0.useDHCP = true;

  # Set your time zone.
  time.timeZone = "Europe/Vienna";
  #services.localtime.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    restic
  ];

  ### List services that you want to enable:

  # this machine uses 27 inch 4k displays
  hardware.video.hidpi.enable = true;
  services.xserver.dpi = 157; # got this off the internet...

  # allow nvidia overclocking and overvolting (8 + 16 respectively)
  services.xserver.deviceSection = ''
    Option "Coolbits" "24"
  '';

  # The remaining syncthing config
  services.syncthing = {
    enable = true;
    user = "tom";
    dataDir = "/home/tom/";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tom = {
    home = "/home/tom";
    description = "Thomas Torsney-Weir";
    extraGroups = [ "wheel" "lp" "lpadmin" "adbusers" ]; # Enable ‘sudo’ for the user.
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
  };

  # home backup
  age.secrets.wasabi.file = ./secrets/wasabi.age;
  age.secrets.restic.file = ./secrets/restic.age;
  services.restic.backups = {
    remote = {
      paths = [ "/home" "/photos" ];
      repository = "s3:https://s3.eu-central-1.wasabisys.com/gabysbrain-restic";
      passwordFile = "/run/agenix/restic"; # FIXME: this should use age.secrets.restic.path somehow
      environmentFile = "/run/agenix/wasabi"; # FIXME: this should use age.secrets.wasbi.path
      extraBackupArgs = [
        "--exclude='**/.cache'"
        "--exclude='**/cache'"
        "--exclude='home/**/Downloads'"
        "--exclude='home/**/Sync'"
        "--exclude='home/*/.cache'"
        "--exclude='home/*/.config'"
        "--exclude='home/*/.julia'"
        "--exclude='home/*/.local'"
        "--exclude='home/*/.mozilla'"
        "--exclude='photos/photoprism-data/sidecar'"
      ];
      pruneOpts = [
        "--keep-within-daily 7d"
        "--keep-within-weekly 2m"
        "--keep-within-monthly 2y"
        "--keep-within-yearly 20y"
        "--keep-last 2"
        "--compression max"
      ];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitInactiveSec = "1d";
      };
    };
  };

  # virtualization
  virtualisation.libvirtd.enable = true;
  users.extraGroups.libvirtd.members = [ "tom" ];
  #boot.extraModprobeConfig = "options kvm ignore_msrs=1";

  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = [ "tom" ];

  # android dev stuff
  programs.adb.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.01"; # Did you read the comment?

}
