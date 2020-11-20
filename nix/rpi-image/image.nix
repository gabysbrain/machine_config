# configuration for building an sd image for raspberry pi booting
{ config, pkgs, lib, options, ... }:
{
  imports = [
    ../rpi-configuration.nix
  ];

  # bzip2 compression takes loads of time with emulation, skip it.
  sdImage.compressImage = false;
}
