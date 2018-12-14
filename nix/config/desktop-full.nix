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
    connman_dmenu

    firefox
    libreoffice
    franz
    spotify
    skype
    jabref
    meld
    newsboat

    inkscape
    gimp
    imagemagick
    shotcut

    rstudio
  ];
}
