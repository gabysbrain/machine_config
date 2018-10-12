{ config, pkgs, ... }:

{
  hardware.pulseaudio = {
    enable = true;
  };
  nixpkgs.config.pulseaudio = true;

  # needed for steam
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  environment.systemPackages = with pkgs; [
    steam
  ];
}
