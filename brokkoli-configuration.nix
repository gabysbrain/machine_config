# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../hardware-configuration.nix
      ./nixos/common.nix
      ./nixos/desktop.nix
      ./nixos/vrvis.nix
      ./sysadmin.nix
    ];

  # use UEFI boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  age.secrets.vrvis-smb.file = ./secrets/vrvis-smb.age;
  fileSystems."/mnt/stone/torsney-weir" = {
    device = "//stone.vrvis.lan/torsney-weir";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

    in ["${automount_opts},credentials=/run/secrets/vrvis-smb"]; # FIXME: should reference age path
  };

  # Set your time zone.
  time.timeZone = "Europe/Vienna";

  networking.hostName = "brokkoli"; # Define your hostname.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable NVIDIA stuff
  services.xserver.videoDrivers = [ "nvidia" ];

  # Users
  users.users.torsney-weir = {
    home = "/home/torsney-weir";
    description = "Thomas Torsney-Weir";
    extraGroups = [ "wheel" "lp" "lpadmin" ];
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
  };
  home-manager.users.torsney-weir = { pkgs, ...} : {
    imports = [
        ./home-config/common.nix
        ./home-config/desktop.nix
        ./profiles/dev.nix
        ./profiles/writing.nix
        ./profiles/vrvis.nix
    ];

  };

  # forward xsessions
  services.openssh.forwardX11 = true;

  # syncthing config
  services.syncthing = {
    enable = true;
    user = "torsney-weir";
    dataDir = "/home/torsney-weir";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

