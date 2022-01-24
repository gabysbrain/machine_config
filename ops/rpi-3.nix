{ config, pkgs, nodes, ... }:
{ 
  nixpkgs.system = "aarch64-linux";

  # save the sd card
  #systemd.services.systemd-journald.enable = false;

  imports = [
    ../rpi-image/rpi3-configuration.nix
  ];
}

