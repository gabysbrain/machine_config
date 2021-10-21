{ config, pkgs, lib, options, ... }:
{
  imports = [
    #<home-manager/nixos>
  ];

  # Boot
  boot.loader.grub.enable = false;
  boot.loader.raspberryPi.enable = true;
  boot.loader.raspberryPi.version = 3;
  boot.loader.raspberryPi.uboot.enable = true;
  boot.loader.raspberryPi.firmwareConfig = ''
     gpu_mem=0
  '';

  boot.consoleLogLevel = 7;

  # Kernel configuration
  #boot.kernelPackages = pkgs.linuxPackages_5_4;
  boot.kernelPackages = pkgs.linuxPackages_4_19;
  boot.kernelParams = ["cma=256M"];

  # Enable additional firmware (such as Wi-Fi drivers).
  hardware.enableRedistributableFirmware = true;
  #hardware.enableAllFirmware = true;

  # Filesystems
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  #swapDevices = [ { device = "/swapfile"; size = 4096; } ]; # setting to 2048 broke things...
  swapDevices = [ { device = "/swapfile"; size = 8192; } ]; # setting to 2048 broke things...

  # Networking (see official manual or `/config/sd-image.nix` in this repo for other options)
  #networking.hostName = "nixpi"; # unleash your creativity!

  networking.enableIPv6 = false;
  /*
  networking.wireless = {
    enable = true;
    interfaces = ["wlan0"];
  };
  */

  # Packages
  environment.systemPackages = with pkgs; [
    # customize as needed!
    bash zsh vim git
    libraspberrypi
  ];

  # Miscellaneous
  time.timeZone = "Europe/Vienna"; 

  # server doesn't compile on raspberry pi
  services.localtime.enable = pkgs.lib.mkForce false;

  # WARNING: if you remove this, then you need to assign a password to your user, otherwise
  # `sudo` won't work. You can do that either by using `passwd` after the first rebuild or
  # by setting an hashed password in the `users.users.yourName` block as `initialHashedPassword`.
  security.sudo.wheelNeedsPassword = false;
  users.users.root = {
    openssh.authorizedKeys.keyFiles = [ /home/tom/.ssh/id_rsa.pub ];
  };
  

  # Nix
  boot.cleanTmpDir = true;

  # don't install man pages to save space
  documentation.man.enable = false;
  documentation.nixos.enable = false;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "21.03";
}
