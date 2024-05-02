{ config, pkgs, ... }:

let
  openrgb = pkgs.openrgb.withPlugins [ pkgs.openrgb-plugin-effects ];
in
{
  services.hardware.openrgb.enable = true;
  services.hardware.openrgb.package = openrgb;
}
