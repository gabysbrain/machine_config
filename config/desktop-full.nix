{ config, pkgs, ... }:

{
  imports = [
    ./desktop-light.nix
  ];

  hardware.pulseaudio = {
    enable = true;
  };
  nixpkgs.config.pulseaudio = true;
  nixpkgs.config.firefox.enableBrowserPass = true;

  environment.systemPackages = with pkgs; [
    blueman
    wpa_supplicant_gui
    connman-gtk

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
    shotcut

    rstudio
  ];
}
