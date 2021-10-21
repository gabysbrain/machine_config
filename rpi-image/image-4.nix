# configuration for building an sd image for raspberry pi booting
{ config, pkgs, lib, options, ... }:
{
  imports = [
     <nixpkgs/nixos/modules/installer/sd-card/sd-image-aarch64.nix>
    ./rpi4-configuration.nix
  ];

  networking.hostName = "newrpi";

  # auto login as root
  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
  };
  
  users.users.nixos = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" ];
    initialPassword = "nixos";
  };
  services.getty.autologinUser = "nixos";

  users.users.root.initialPassword = "nixos"; # Log in without a password

  # bzip2 compression takes loads of time with emulation, skip it.
  sdImage.compressImage = false;
}
