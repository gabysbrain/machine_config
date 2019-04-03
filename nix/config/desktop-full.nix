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

    python36Packages.glances

    libreoffice-unwrapped
    spotify
    skype
    jabref
    meld
    newsboat
    weechat

    inkscape
    gimp
    imagemagick
    shotcut

    exiftool
    poppler_utils
  ];

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
