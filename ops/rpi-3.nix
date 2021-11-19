{ config, pkgs, nodes, ... }:
{ 
  nixpkgs.system = "aarch64-linux";

  imports = [
    ../rpi-image/rpi3-configuration.nix
  ];
}

