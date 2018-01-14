{ config, pkgs, ... }:

{
  imports = [
    ./desktop-light.nix
  ];

  hardware.pulseaudio = {
    enable = true;
  };
  nixpkgs.config.pulseaudio = true;

  environment.systemPackages = with pkgs; [
    blueman

    firefox
    libreoffice
    slack
    spotify
    skype
    jabref
    meld

    inkscape
    gimp
    imagemagick
  ];
}
