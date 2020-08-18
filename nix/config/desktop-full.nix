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

    alacritty

    python36Packages.glances

    libreoffice-unwrapped
    spotify
    zoom-us
    zotero
    meld

    (callPackage ../pkgs/gcal-conky {})

    inkscape
    gimp
    imagemagick
    shotcut

    jrnl

    exiftool
    poppler_utils
    #glib-networking # feedreader needs this for ssl

    # ledger/finance stuff
    hledger
    hledger-interest
  ];

  services.system-config-printer.enable = true;

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
              ]))
            ];
            scripts = with pkgs.weechatScripts; [
              wee-slack
            ];
          };
        };
      }
    )
  ];
}
