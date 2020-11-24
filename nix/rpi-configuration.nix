{ config, pkgs, lib, options, ... }:
{
  imports = [
    ./config/base.nix
    ./wireless.nix
  ];

  # Boot
  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 3;
  boot.loader.raspberryPi.uboot.enable = true;

  # Kernel configuration
  boot.kernelPackages = pkgs.linuxPackages_4_19;
  boot.kernelParams = ["cma=32M"];

  # Enable additional firmware (such as Wi-Fi drivers).
  hardware.enableRedistributableFirmware = true;

  # Filesystems
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };
  swapDevices = [ { device = "/swapfile"; size = 1024; } ];

  # Networking (see official manual or `/config/sd-image.nix` in this repo for other options)
  networking.hostName = "nixpi"; # unleash your creativity!

  networking.enableIPv6 = false;
  networking.wireless = {
    enable = true;
    interfaces = ["wlan0"];
  };

  # Packages
  environment.systemPackages = with pkgs; [
    # customize as needed!
    zsh vim git htop
  ];

  # Users
  users.users.tom = {
    home = "/home/tom";
    description = "Thomas Torsney-Weir";
    extraGroups = [ "wheel" "lp" "lpadmin" "media" ]; # Enable ‘sudo’ for the user.
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
    openssh.authorizedKeys.keyFiles = [
      #"ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAwDPUjo80GFY2FO9bDH9cAXo7n7SiUKjXIHzfRMfsAqD9Rk/puV+W4QRvT0XOZSEZQf3gifcPM/raA35BVmAzAa2jYISWeUWIqYf+AcipFrMKKqS639Q9/GgJL2STr6Gh0EVHsGcFJpuJ8GO5eqnKR0ZYl3j9bpMO/WpgkAw7hUU= tom@katana"
      /home/tom/.ssh/id_rsa.pub
    ];
  };
  home-manager.users.tom = import ./home-config/server.nix; # needs to be a function

  # Miscellaneous
  time.timeZone = "Europe/London"; 
  #services.openssh.enable = true;
  #services.openssh.hostKeys = options.services.openssh.hostKeys.default;

  # server doesn't compile on raspberry pi
  services.localtime.enable = pkgs.lib.mkForce false;

  services.openssh = {
    enable = true;
    permitRootLogin = "prohibit-password";
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
    extraConfig = "Compression no";
  };
  
  users.users.root = {
    openssh.authorizedKeys.keyFiles = [ /home/tom/.ssh/id_rsa.pub ];
  };

  # WARNING: if you remove this, then you need to assign a password to your user, otherwise
  # `sudo` won't work. You can do that either by using `passwd` after the first rebuild or
  # by setting an hashed password in the `users.users.yourName` block as `initialHashedPassword`.
  security.sudo.wheelNeedsPassword = false;

  # Nix
  boot.cleanTmpDir = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "20.09";
}
