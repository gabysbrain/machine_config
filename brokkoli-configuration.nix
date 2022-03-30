# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./nixos/nvidia-fix.nix
      ./nixos/common.nix
      ./nixos/desktop.nix
      ./nixos/vrvis.nix
      ./sysadmin.nix
    ];

  # use UEFI boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # kernel modules
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/5fa1b0df-4dee-4791-9cc5-81a0448f4f73";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/ff43a8ff-665e-482f-906a-7705d2a0c435";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/925D-E65B";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/6035c430-9da5-4288-84dd-dbcfd3232121"; }
    ];


  age.secrets.vrvis-smb.file = ./secrets/vrvis-smb.age;
  fileSystems."/mnt/stone/torsney-weir" = {
    device = "//stone.vrvis.lan/torsney-weir";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
      user_opts = "uid=1000,gid=100";
      cred_path = "/run/agenix/vrvis-smb";

    in ["${automount_opts},${user_opts},credentials=${cred_path}"]; # FIXME: should reference age path
  };

  # Set your time zone.
  time.timeZone = "Europe/Vienna";

  networking.hostName = "brokkoli"; # Define your hostname.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp4s0.useDHCP = true;

  # Configure VRVis proxy 
  networking.proxy.httpProxy  = "http://proxy.vrvis.at:3128";
  networking.proxy.httpsProxy = "http://proxy.vrvis.at:3128";
  networking.proxy.noProxy    = "127.0.0.1,localhost,vrvis.lan,vrvis.at";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable NVIDIA stuff
  services.xserver.videoDrivers = [ "nvidia" ];

  # Users
  users.users.torsney-weir = {
    home = "/home/torsney-weir";
    description = "Thomas Torsney-Weir";
    extraGroups = [ "wheel" "lp" "lpadmin" "adbusers" ];
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
  };

  # forward xsessions
  services.openssh.forwardX11 = true;

  # syncthing config
  services.syncthing = {
    enable = true;
    user = "torsney-weir";
    dataDir = "/home/torsney-weir";
  };

  networking.firewall.allowedTCPPorts = [ 

  ];
  
  # lots of dealing with docker containers
  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = [ "torsney-weir" ];

  environment.systemPackages = with pkgs; [
    openconnect
    vpn-slice
  ];

  # android development stuff
  programs.adb.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

