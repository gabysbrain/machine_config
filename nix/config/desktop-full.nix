{ config, pkgs, ... }:

{
  imports = [
    ./desktop-light.nix
  ];

  hardware.pulseaudio = {
    enable = true;
    daemon.config = {
      flat-volumes = "no";
      default-sample-format = "s24le";
      default-sample-rate = "192000";
      resample-method = "speex-float-10";
      avoid-resampling = "true";
    };
    package = pkgs.pulseaudioFull;
  };

  nixpkgs.config.pulseaudio = true;

  environment.systemPackages = with pkgs; [
    blueman
    wpa_supplicant_gui
    connman-gtk
    connman_dmenu
    pavucontrol

    python36Packages.glances

    libreoffice-unwrapped
    spotify
    skype
    zotero
    meld
    newsboat
    weechat

    gnome3.gnome-calendar
    gnome3.evolution-data-server

    inkscape
    gimp
    imagemagick
    shotcut

    jrnl

    exiftool
    poppler_utils
  ];

  #nixpkgs.config.oraclejdk.accept_license = true;
  nixpkgs.overlays = [
    (
      self: super: {
        weechat = super.weechat.override {
          #pythonPackages = super.python36Packages;
          configure = { availablePlugins, ...}: {
            plugins = with availablePlugins; [
              (python.withPackages (ps: with ps; [
                websocket_client
                #yowsup
              ]))
            ];
            scripts = with pkgs.weechatScripts; [
              weechat-xmpp
              wee-slack
              (import ../pkgs/weechat-whatsapp)
            ];
          };
        };
      }
    )
  ];
}
